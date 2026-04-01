# CODE ADAPTED FROM ORIGINAL FILE BY Kathi Jankowsky AND EDITED BY Elizabeth Elmstrom
# MODIFIED BY Nicholas Fox

# Credits from the original file ("WRTDS_DIN_example.R"):
################################################################################
## Kathi Jo's WRTDS Code
## Edits by Liz 8/25/2023

## Running EGRET/WRTDS/WRTDS Kalman - DIN (NO3 + NH4)
# information on EGRET and WRTDS Model Here: https://doi-usgs.github.io/EGRET/
# User manual: https://pubs.er.usgs.gov/publication/tm4A10
################################################################################


#args <- commandArgs(trailingOnly=TRUE)

# Set working directory to script location
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("/Users/sagefox/My Drive (sagefox@uw.edu)/Research/Python Code/chile_river_research/WRTDS/WRTDS_Working_Bootstrap_Demo_Full")
#setwd(args[3])


#==============================================#
#      More Preparation                        #
#==============================================#

## Adjust these toggles and paths as needed:
# alternate_percentiles <- TRUE
# toggle_streamflow <- TRUE


# # Load rivers and chem from command line args
# river <- args[1]
# chem <- args[2]
# wd <- args[4]
# rcf <- glue("{wd}{river}/{chem}")
# rf <- glue("{wd}{river}")
# nboot <- as.numeric(args[5])
# nkalman <- as.numeric(args[6])
# in_seed <- as.numeric(args[7])
#rho <- args[6]

# print(args)
# print(chem)
# print(rcf)
# print(river)
# print(list.files(wd))
# print(list.files("."))


# Prepare results folder
#output_folder <- glue("{rcf}/Results")
#if (!dir.exists(output_folder)) {
#  dir.create(output_folder)
#}

create_eList <- function(Info, Daily, Sample)
{
  ######## Read in Data ########
  Info <- makeUserInfo(Info)
  Daily <- makeUserDaily(Daily, qUnit = 2)
  Sample <- makeUserSample(Sample)

  ######## Merge Data ##########
  return(mergeReport(Info, Daily, Sample))
}

estimate_tables <- function(river, chem,
                            eList,
                            nboot, nkalman, in_seed,
                            output_folder = NA,
                            alternate_percentiles=TRUE, toggle_streamflow=TRUE)
{

  
  ######## Run Model ##########
  set.seed(in_seed)
  eList1 <- modelEstimation(eList, minNumObs = 5, minNumUncen = 1)
  eList1 <- WRTDSKalman(eList1, rho = 0.9, niter = 200, seed = NA, verbose = FALSE)
  gc()

  
  ######## Add Confidence Intervals ##########
  # Generate bootstrap replicates
  dailyBootOut <- genDailyBootAlt(
    eList1, nBoot = nboot, nKalman = nkalman, rho = 0.9,
      jitterOn = TRUE, V = 0.2, blockLength = 240
  )
  if (!is.na(output_folder))
  {
    write.csv(dailyBootOut, glue(output_folder, "/dailyBootOut.csv"), row.names = FALSE)
  }
  
  # Create prediction intervals
  dayPct <- makeDailyPI(dailyBootOut, eList1, fluxUnit = 3)
  if (!is.na(output_folder))
  {
      write.csv(dayPct$conc, glue(output_folder, "/dayPct_conc.csv"), row.names = FALSE)
      write.csv(dayPct$flux, glue(output_folder, "/dayPct_flux.csv"), row.names = FALSE)
  }
  
  ######### Cumulative Load ##########
  ContConc <- eList1$Daily
  ContConc$CumulativeFlux <- cumsum(ContConc$FluxDay)
  ContConc$CumulativeGenFlux <- cumsum(ContConc$GenFlux)
  
  # Rolling sums across bootstrap replicates
  if (alternate_percentiles) {
    cumulative_boot_sums <- apply(dailyBootOut, 2, cumsum)
    
    # Ensure correct lengths
    n_rows <- nrow(ContConc)
    n_p5 <- length(dayPct$flux$p5_cum)
    n_p95 <- length(dayPct$flux$p95_cum)
    
    if (n_p5 < n_rows) {
      missing_rows <- n_rows - n_p5
      last_value <- tail(dayPct$flux$p5_cum, 1)
      dayPct$flux$p5_cum <- c(dayPct$flux$p5_cum, rep(last_value, missing_rows))
    } else if (n_p5 > n_rows) {
      dayPct$flux$p5_cum <- dayPct$flux$p5_cum[1:n_rows]
    }
    
    if (n_p95 < n_rows) {
      missing_rows <- n_rows - n_p95
      last_value <- tail(dayPct$flux$p95_cum, 1)
      dayPct$flux$p95_cum <- c(dayPct$flux$p95_cum, rep(last_value, missing_rows))
    } else if (n_p95 > n_rows) {
      dayPct$flux$p95_cum <- dayPct$flux$p95_cum[1:n_rows]
    }
    
    p5_cum <- apply(cumulative_boot_sums, 1, function(x) {
      quantile(x, probs = 0.05, na.rm = TRUE, type = 3)
    })
    p95_cum <- apply(cumulative_boot_sums, 1, function(x) {
      quantile(x, probs = 0.95, na.rm = TRUE, type = 3)
    })
    
    dayPct$flux$p5_cum <- p5_cum
    dayPct$flux$p95_cum <- p95_cum
    
  } else {
    dayPct$flux$p5_cum <- cumsum(dayPct$flux$p5)
    dayPct$flux$p95_cum <- cumsum(dayPct$flux$p95)
  }
  
  # More checks for p5_cum/p95_cum length
  n_rows <- nrow(ContConc)
  
  if (length(dayPct$flux$p5_cum) < n_rows) {
    missing_rows <- n_rows - length(dayPct$flux$p5_cum)
    dayPct$flux$p5_cum <- c(dayPct$flux$p5_cum, rep(NA, missing_rows))
  }
  if (length(dayPct$flux$p95_cum) < n_rows) {
    missing_rows <- n_rows - length(dayPct$flux$p95_cum)
    dayPct$flux$p95_cum <- c(dayPct$flux$p95_cum, rep(NA, missing_rows))
  }
  
  ContConc$p5_cum <- dayPct$flux$p5_cum
  ContConc$p95_cum <- dayPct$flux$p95_cum
  
  # Repeat checks to ensure matching lengths
  n_rows <- nrow(ContConc)
  n_p5 <- length(dayPct$flux$p5_cum)
  n_p95 <- length(dayPct$flux$p95_cum)
  
  if (n_p5 < n_rows) {
    missing_rows <- n_rows - n_p5
    dayPct$flux$p5_cum <- c(dayPct$flux$p5_cum, rep(NA, missing_rows))
  } else if (n_p5 > n_rows) {
    dayPct$flux$p5_cum <- dayPct$flux$p5_cum[1:n_rows]
  }
  
  if (n_p95 < n_rows) {
    missing_rows <- n_rows - n_p95
    dayPct$flux$p95_cum <- c(dayPct$flux$p95_cum, rep(NA, missing_rows))
  } else if (n_p95 > n_rows) {
    dayPct$flux$p95_cum <- dayPct$flux$p95_cum[1:n_rows]
  }
  
  ContConc$p5_cum <- dayPct$flux$p5_cum
  ContConc$p95_cum <- dayPct$flux$p95_cum
  
  # Save daily data
  if (!is.na(output_folder)) {
      write.csv(
        ContConc,
        paste0(output_folder, "/", river, glue("_daily{chem}_WRTDS.csv"))
      )
 }
 
  # Extract error statistics
  fluxBias <- fluxBiasStat(getSample(eList1))
  print(paste("FLUX BIAS: ", fluxBias[3]))
  error <- errorStats(eList1)
  error$fluxBias1 <- fluxBias[1][[1,1]] # Hope this doesnt mess up the data pipeline
  error$fluxBias2 <- fluxBias[2][[1,1]] # [[1,1]] is used to unpack the dataframe type
  error$fluxBias3 <- fluxBias[3][[1,1]] # We use fluxBias3 but i'm pretty sure theyre all the same
  if (!is.na(output_folder)) {
      write.csv(
        error,
        paste0(output_folder, "/", river, "_ErrorStats_WRTDS.csv"),
        row.names = FALSE
      )
  }
  
  return (list(
    eList=eList1,
    dailyBootOut=dailyBootOut,
    dayPct=dayPct,
    ContConc=ContConc,
    error=error
  ))
}

pdf_output <- function(river, chem, eList1, dayPct, ContConc, output_folder)
{
  ######## Graphical Output ########
  pdf(paste0(output_folder, "/", river, "_WRTDS_output_with_CI_and_CumulativeLoad.pdf"))
  
  # Residual plots
  fluxBiasMulti(eList1)
  
  # Data overview
  multiPlotDataOverview(eList1)
  
  # Debugging: Check sampled cumulative flux values
  sampled_indices <- match(Sample$Date, ContConc$Date)
  lastIndex <- tail(sampled_indices, n = 1) + 10
  eList1$Daily <- eList1$Daily[0:lastIndex, ]
  ContConc <- ContConc[0:lastIndex, ]
  dayPct$flux <- dayPct$flux[0:lastIndex, ]
  dayPct$conc <- dayPct$conc[0:lastIndex, ]
  
  # Prepare monthly tick marks
  all_months <- seq(
    from = min(eList1$Daily$Date), to = max(eList1$Daily$Date), by = "1 month"
  )
  month_labels <- format(all_months, "%b %Y")
  month_labels[seq_along(month_labels) %% 2 != 0] <- ""
  
  # 7-day rolling average
  smoothed_streamflow <- rollmean(eList1$Daily$Q, 7, fill = 0, align = "center")
  
  plot_streamflow <- function() {
    par(new = TRUE)
    plot(eList1$Daily$Date, smoothed_streamflow,
         type = "n", yaxt = "n", xaxt = "n", ann = FALSE
    )
    last_date <- max(eList1$Daily$Date)
    start_date <- min(eList1$Daily$Date)
    polygon(
      c(start_date, eList1$Daily$Date, last_date),
      c(0, smoothed_streamflow, 0),
      col = rgb(0.8, 0.8, 0.8, 0.4), border = NA
    )
    lines(eList1$Daily$Date, smoothed_streamflow,
          col = rgb(0.8, 0.8, 0.8, 0.4), lwd = 1.5
    )
    par(yaxt = "s", xaxt = "n")
    
    streamflow_range <- range(smoothed_streamflow, na.rm = TRUE)
    axis(4,
         at = seq(0, max(streamflow_range), by = 100),
         labels = seq(0, max(streamflow_range), by = 100)
    )
    mtext("Streamflow (m³/s)", side = 4, line = 2.5, cex = 1.0)
  }
  
  ############ Plot concentration ############
  par(mar = c(5, 5, 4, 5))
  ylim_values <- range(dayPct$conc$p5, dayPct$conc$p95, na.rm = TRUE)
  ylim_values[is.infinite(ylim_values)] <- NA
  
  plot(ContConc$Date, ContConc$GenConc, type = "l",
       ylab = "Concentration (mg/L)",
       xlab = "",
       xaxt = "n",
       col = "black",
       ylim = ylim_values
  )
  polygon(
    c(eList1$Daily$Date, rev(eList1$Daily$Date)),
    c(dayPct$conc$p5, rev(dayPct$conc$p95)),
    col = rgb(0, 0, 1, 0.2), border = NA
  )
  
  points(Sample$Date, Sample$ConcAve, col = "red", pch = 16)
  
  if (toggle_streamflow) {
    plot_streamflow()
    legend(
      "topleft",
      legend = c(
        "Daily Concentration (Kalman Estimate)",
        "90% Confidence Interval (Kalman Estimate)",
        "Streamflow (7-day Rolling Avg.)",
        "Water Sampling Dates"
      ),
      col = c("black", NA, NA, "red"),
      lty = c(1, 1, NA, NA, NA),
      lwd = c(2, 2, NA, NA, NA),
      pch = c(NA, NA, NA, 16),
      fill = c(NA, rgb(0, 0, 1, 0.2), rgb(0.8, 0.8, 0.8, 0.4), NA),
      bty = "n",
      border = c(NA, NA, NA, NA)
    )
  } else {
    legend(
      "topleft",
      legend = c(
        "GenConc",
        "ConcDay",
        "90% Confidence Interval",
        "Water Sampling Dates"
      ),
      col = c("blue", "black", NA, "red"),
      lty = c(1, 1, NA, NA),
      lwd = c(2, 2, NA, NA),
      pch = c(NA, NA, NA, 16),
      fill = c(NA, NA, rgb(0, 0, 1, 0.2), NA),
      bty = "n",
      border = c(NA, NA, NA, NA)
    )
  }
  
  par(xaxt = "s")
  axis(1, at = all_months, labels = FALSE)
  text(
    x = all_months,
    y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
    labels = month_labels,
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8
  )
  title(glue("{river} Daily {chem} Concentration"))
  
  ############ Plot flux ############
  ylim_flux <- range(dayPct$flux$p5, dayPct$flux$p95, na.rm = TRUE)
  ylim_flux[is.infinite(ylim_flux)] <- NA
  
  plot(ContConc$Date, ContConc$GenFlux, type = "l",
       ylab = "Flux (metric tonnes / day)",
       xlab = "",
       xaxt = "n",
       col = "black",
       ylim = ylim_flux
  )
  polygon(
    c(eList1$Daily$Date, rev(eList1$Daily$Date)),
    c(dayPct$flux$p5, rev(dayPct$flux$p95)),
    col = rgb(0, 0, 1, 0.2), border = NA
  )
  points(ContConc$Date[sampled_indices], ContConc$GenFlux[sampled_indices],
         col = "red", pch = 16
  )
  
  par(mar = c(5, 5, 4, 5))
  par(xaxt = "s")
  axis(1, at = all_months, labels = FALSE)
  text(
    x = all_months,
    y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
    labels = month_labels,
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8
  )
  title(glue("{river} Daily {chem} Flux"))
  
  if (toggle_streamflow) {
    plot_streamflow()
    legend(
      "topleft",
      legend = c(
        "Daily Flux (Kalman Estimate)",
        "90% Confidence Interval (Kalman Estimate)",
        "Streamflow (7-day Rolling Avg.)",
        "Water Sampling Dates"
      ),
      col = c("black", NA, NA, "red"),
      lty = c(1, 1, NA, NA, NA),
      lwd = c(2, 2, NA, NA, NA),
      pch = c(NA, NA, NA, 16),
      fill = c(
        NA, rgb(0, 0, 1, 0.2),
        rgb(0.8, 0.8, 0.8, 0.5), NA
      ),
      bty = "n",
      border = c(NA, NA, NA, NA)
    )
  } else {
    legend(
      "topleft",
      legend = c(
        "Observed Flux",
        "Estimated Flux",
        "90% Confidence Interval for (obs/est?) Flux",
        "Water Sampling Dates"
      ),
      col = c("black", "green", NA, "red"),
      lty = c(1, 1, NA, NA),
      lwd = c(2, 2, NA, NA),
      pch = c(NA, NA, NA, 16),
      fill = c(NA, NA, rgb(0.8, 0.8, 0.8, 0.5), NA),
      bty = "n",
      border = c(NA, NA, NA, NA)
    )
  }
  
  # Ensure Date columns are Date objects
  ContConc$Date <- as.Date(ContConc$Date)
  Sample$Date <- as.Date(Sample$Date)
  eList1$Daily$Date <- as.Date(eList1$Daily$Date)
  
  ######## Plot cumulative load and streamflow ########
  # Convert cumulative load from kg to tonnes (if desired)
  ContConc$CumulativeGenFlux <- ContConc$CumulativeGenFlux / 1000
  dayPct$flux$p5_cum <- dayPct$flux$p5_cum / 1000
  dayPct$flux$p95_cum <- dayPct$flux$p95_cum / 1000
  
  par(mar = c(5, 5, 4, 5))
  plot(eList1$Daily$Date, ContConc$CumulativeGenFlux, type = "l",
       col = "black", lwd = 2,
       xlab = "", ylab = "Cumulative Load (metric tonnes)",
       main = glue("{river} Cumulative {chem} Flux"),
       xaxt = "n"
  )
  par(xaxt = "s")
  axis(1, at = all_months, labels = FALSE)
  
  text(
    x = all_months,
    y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
    labels = month_labels,
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8
  )
  
  polygon(
    c(eList1$Daily$Date, rev(eList1$Daily$Date)),
    c(dayPct$flux$p5_cum, rev(dayPct$flux$p95_cum)),
    col = rgb(0, 0, 1, 0.2), border = NA
  )
  
  sampled_cumulative_flux <- ContConc$CumulativeGenFlux[sampled_indices]
  points(eList1$Daily$Date[sampled_indices], sampled_cumulative_flux,
         col = "red", pch = 16
  )
  
  print("Sampled dates values:")
  print(sampled_indices)
  print(eList1$Daily$Date[sampled_indices])
  print(head(sampled_cumulative_flux))
  
  par(new = TRUE)
  plot(eList1$Daily$Date, smoothed_streamflow,
       type = "n", yaxt = "n", xaxt = "n", ann = FALSE
  )
  last_date <- max(eList1$Daily$Date)
  start_date <- min(eList1$Daily$Date)
  polygon(
    c(start_date, eList1$Daily$Date, last_date),
    c(0, smoothed_streamflow, 0),
    col = rgb(0.8, 0.8, 0.8, 0.4), border = NA
  )
  lines(eList1$Daily$Date, smoothed_streamflow,
        col = rgb(0.8, 0.8, 0.8, 0.4), lwd = 1.5
  )
  
  par(yaxt = "s", xaxt = "n")
  streamflow_range <- range(smoothed_streamflow, na.rm = TRUE)
  axis(4,
       at = seq(0, max(streamflow_range), by = 100),
       labels = seq(0, max(streamflow_range), by = 100)
  )
  mtext("Streamflow (m³/s)", side = 4, line = 2.5, cex = 1.0)
  
  legend(
    "topleft",
    legend = c(
      "Cumulative Flux (Kalman Estimate)",
      "Cumulative Flux (90% Confidence Interval)",
      "Streamflow (7-day Rolling Avg.)",
      "Water Sampling Dates"
    ),
    col = c("black", NA, NA, "red"),
    lty = c(1, NA, NA, NA),
    lwd = c(2, NA, NA, NA),
    pch = c(NA, NA, NA, 16),
    fill = c(
      NA, rgb(0, 0, 1, 0.2),
      rgb(0.8, 0.8, 0.8, 0.5), NA
    ),
  bty = "n",
  border = c(NA, NA, NA, NA)
)

}

NA