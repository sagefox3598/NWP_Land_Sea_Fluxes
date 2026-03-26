# All of the below is copied and/or modified from the EGRET source code


# Weird bug.. `remove_zeros` not found. Copying it here
remove_zeros <- function(data, verbose){
  flaggedData1 <- data[(data$ConcLow == 0 & data$ConcHigh == 0),]
  data <- data[!(data$ConcLow == 0 & data$ConcHigh == 0),]
  
  if (nrow(flaggedData1) > 0){
    WarningMessage <- paste("Deleted", nrow(flaggedData1), "rows of data because concentration was reported as 0.0, the program is unable to interpret that result and is therefore deleting it.")    
    warning(WarningMessage)
    if (verbose){
      cat("Deleted Rows:\n")
      print(flaggedData1)
    }
  }
  return(data)
}


# Copied and modified from readUserInfo
makeUserInfo <- function(siteInfo,hasHeader=TRUE,separator=",",interactive=FALSE){
#
#  # Keeping things backwards compatible:
##    filePath <- substr(filePath, 1, nchar(filePath)-1)
#  }
#    
#  totalPath <- file.path(filePath, fileName)
#
#  if(file.exists(totalPath)){
#    siteInfo <- utils::read.delim(  
#      totalPath, 
#      header = hasHeader,
#      sep=separator,
#      colClasses=c('character'),
#      fill = TRUE, 
#      comment.char="#")
#  } else {
#    message("File not found, continuing with interactive section.")
#    siteInfo <- data.frame(station.nm="",
#                           shortName="",
#                           param.nm="",
#                           paramShortName="",
#                           param.units="",
#                           drainSqKm="",
#                           stringsAsFactors=FALSE)
#  }
  
  if(interactive){

    if (!("station.nm" %in% names(siteInfo))){
      cat("No station name was listed. Please enter a station name here(no quotes): \n")
      siteInfo$station.nm <- readline()
    }
    cat("Your site name is", siteInfo$station.nm,"\n")
    
    if(!("shortName" %in% names(siteInfo))){
      cat("but you can modify this to a short name in a style you prefer. \n")
      cat("The shortName name will be used to label graphs and tables. \n")
      cat("If you want the program to use the name given above, \n")
      cat("just do a carriage return, otherwise enter the preferred short name(no quotes):\n")
      siteInfo$shortName <- readline()
      if (!nzchar(siteInfo$shortName)) siteInfo$shortName <- siteInfo$station.nm
    }
    
    if (!("param.nm" %in% names(siteInfo))){
      cat("No water quality parameter name was listed.\n")
      cat("Please enter the name here(no quotes): \n")
      siteInfo$param.nm <- readline()
    }
    
    cat("Your water quality data are for '", siteInfo$param.nm, "'.\n")

    if (!("paramShortName" %in% names(siteInfo))){
      cat("Typically you will want a shorter name to be used in graphs and tables. \n")
      cat("The suggested short name is:'", siteInfo$paramShortName, "'.\n")
      cat("If you would like to change the short name, enter it here, otherwise just hit enter (no quotes):")
      shortNameTemp <- readline()
      
      if (nchar(shortNameTemp)>0) siteInfo$paramShortName <- shortNameTemp
    }
    
    if (!("param.units" %in% names(siteInfo))){
      cat("No water quality parameter unit was listed.\n")
      cat("Please enter the units here(no quotes): \n")
      siteInfo$param.units <- readline()
    }

    if (!("constitAbbrev" %in% names(siteInfo))){
      cat("It is helpful to set up a constiuent abbreviation, \n")
      cat("enter a unique id (three or four characters should work something like tn or tp or NO3).\n")
      cat("Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
      siteInfo$constitAbbrev <- readline()
    }
    
    if (!("staAbbrev" %in% names(siteInfo))){
      cat("It is helpful to set up a station, enter a unique id (three or four characters should work).\n")
      cat("Even if you don't feel you need an abbreviation for your site you need to enter something(no quotes):\n")
      siteInfo$staAbbrev <- readline()
    }
    
    if (!("drainSqKm" %in% names(siteInfo))){
      cat("No drainage area was listed as a column named 'drainSqKm'.\n")
      cat("Drainage area is used to calculate runoff parameters in flow history calculations.\n")
      cat("Please enter the drainage area, you can enter it in the units of your choice.\n")
      cat("Enter the area, then enter drainage area code, \n")
      cat("1 is square miles, \n")
      cat("2 is square kilometers, \n")
      cat("3 is acres, \n")
      cat("4 is hectares.\n")
      cat("Area(no quotes):\n")
      siteInfo$drain.area.va <- readline()
      siteInfo$drain.area.va <- as.numeric(siteInfo$drain.area.va)
      cat("Unit Code (1-4, no quotes):")
      qUnit <- readline()
      qUnit <- as.numeric(qUnit)
      conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
      siteInfo$drainSqKm <- siteInfo$drain.area.va * conversionVector[qUnit]
    }
  } else {
    requiredColumns <- c("drainSqKm", "staAbbrev", "constitAbbrev", 
                         "param.units", "paramShortName","shortName")
    if(!all(requiredColumns %in% names(siteInfo))){
      message("The following columns are expected in the EGRET package:\n")
      message(requiredColumns[!(requiredColumns %in% names(siteInfo))])
    }
  }
  
  localUnits <- toupper(siteInfo$param.units)
  possibleGoodUnits <- c("mg/l","mg/l as N", "mg/l as NO2", 
                         "mg/l as NO3","mg/l as P","mg/l as PO3","mg/l as PO4","mg/l as CaCO3",
                         "mg/l as Na","mg/l as H","mg/l as S","mg/l NH4" )
  
  allCaps <- toupper(possibleGoodUnits)
  if(!(localUnits %in% allCaps)){
    if(interactive){
      message("Required concentration units are mg/l. \nThe INFO dataframe indicates:",siteInfo$param.units,
              "\nFlux calculations will be wrong if units are not consistent")
    } 
  }
  
  namesToNum <- c("paLong", "paStart", "drainSqKm", "bottomLogQ", "stepLogQ", "stepYear","windowY","windowQ",
                  "windowS","dec.lat.va","dec.long.va","drain.area.va")
  namesToNum <- namesToNum[which(namesToNum %in% names(siteInfo))]
  
  namesToInt <- c("nVectorYear","minNumObs","minNumUncen")
  namesToInt <- namesToInt[which(namesToInt %in% names(siteInfo))]
  
  if(length(namesToNum) > 0){
    siteInfo[,namesToNum] <- as.numeric(siteInfo[,namesToNum])
  }
  
  if(length(namesToInt) > 0){
    siteInfo[,namesToNum] <- as.numeric(siteInfo[,namesToNum])
  }
    
  siteInfo$queryTime <- Sys.time()
  if(!("paStart" %in% names(siteInfo))){
    siteInfo$paStart <- 10
  }
  if(!("paLong" %in% names(siteInfo))){
    siteInfo$paLong <- 12
  }
  return(siteInfo)
}


# Copied and modified from readUserSample
makeUserSample <- function (data,
                            hasHeader = TRUE,
                            separator = ",", 
                            verbose = TRUE, 
                            format = "%m/%d/%Y"){

#  data <- readDataFromFile(filePath,
#                           fileName,
#                           hasHeader = hasHeader,
#                           separator = separator, 
#                           format = format)
  
  compressedData <- compressData(data, verbose = verbose)
  compressedData <- remove_zeros(compressedData, verbose = verbose)
  Sample <- populateSampleColumns(compressedData)
  orig_Sample <- c("Date", "ConcLow", "ConcHigh", "Uncen", "ConcAve",
                   "Julian", "Month", "Day", "DecYear", "waterYear", "MonthSeq",
                   "SinDY", "CosDY")
  Sample <- Sample[, c(orig_Sample, names(Sample)[!names(Sample) %in% orig_Sample])]
  Sample <- Sample[order(Sample$Date), ]
  return(Sample)
}

# Copied and modified from readUserDaily
makeUserDaily <- function (data,
                           hasHeader = TRUE,
                           separator = ",",
                           qUnit = 1,
                           format = "%m/%d/%Y",
                           verbose = TRUE){
  
#  data <- readDataFromFile(filePath,
#                           fileName,
#                           hasHeader = hasHeader,
#                           separator = separator,
#                           format = format)
  convertQ <- c(35.314667, 1, 0.035314667, 0.001)
  qConvert<-convertQ[qUnit]

  if(qUnit==1) message("The input discharge are assumed to be in cubic feet per second, if they are in cubic meters per second, then the call to readUserDaily should specify qUnit=2")

  names(data) <- c("dateTime", "value")
  localDaily <- populateDaily(data,qConvert, verbose=verbose)
  localDaily <- localDaily[!is.na(localDaily$Q),]
  return(localDaily)
}

# genDailyBootAlt function provided by Robert M. Hirsch, USGS
genDailyBootAlt <- function (eList, nBoot = 10, nKalman = 10, blockLength = 200,
                             rho = 0.9, setSeed = NA, jitterOn = FALSE, V = 0.2)
# this is just like genDailyBoot() but it makes the block length an argument
{
  nTotalReps <- nBoot * nKalman
  localDaily <- eList$Daily
  localSample <- eList$Sample
  localINFO <- eList$INFO
  nDaily <- length(localDaily$Date)
  if (!is.na(setSeed)) {
    set.seed(setSeed)
  }
  if (!all(c("windowY", "windowQ", "windowS", "minNumObs", 
             "minNumUncen") %in% names(localINFO))) {
    stop("Run EGRET::setUpEstimation on eList before running genDailyBoot")
  }
  dailyBootOut <- matrix(data = NA, nrow = nDaily, ncol = nTotalReps)
  for (iBoot in 1:nBoot) {
    message("Boot: ", iBoot)
    bootSample <- blockSample(localSample, blockLength = blockLength)
    if (jitterOn) 
      bootSample <- jitterSam(bootSample, V = V)
    eListBoot <- EGRET::as.egret(localINFO, localDaily, bootSample)
    surfaces1 <- EGRET::estSurfaces(eListBoot, verbose = FALSE, 
                                    windowY = localINFO$windowY, windowQ = localINFO$windowQ, 
                                    windowS = localINFO$windowS, minNumObs = localINFO$minNumObs, 
                                    minNumUncen = localINFO$minNumUncen, edgeAdjust = ifelse(is.null(localINFO$edgeAdjust), 
                                                                                             TRUE, localINFO$edgeAdjust))
    eListBoot <- EGRET::as.egret(localINFO, localDaily, localSample, 
                                 surfaces1)
    message("made surfaces from boot sample", iBoot, "replicate")
    for (iKalman in 1:nKalman) {
      message("     - Kalman index: ", iKalman)
      eListK <- EGRET::WRTDSKalman(eListBoot, rho = rho, 
                                   verbose = FALSE, niter = 1, seed = setSeed + 
                                     iKalman)
      iter <- ((iBoot - 1) * nKalman) + iKalman
      dailyBootOut[, iter] <- eListK$Daily$GenFlux
    }
  }
  return(dailyBootOut)
}

