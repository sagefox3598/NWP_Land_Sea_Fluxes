import pandas as pd
import numpy as np
import os


pwd = os.getcwd().split("/")

try:
    wq_path = os.environ["WQ_PATH"]
    flow_path = os.environ["FLOW_DATA_PATH"]
except KeyError:
    # this module could be called from anywhere, so it has to figure out the river data path
    #River_Data_path = "/".join(pwd[:pwd.index("Research")]) + "/Research/data/River_Data.xlsx"
    wq_path = "../data/wq_uncen_all.xlsx"
    flow_path = "../data/Downstream_Estimate_All_Rivers_rolling60.csv"

area_path = "../data/watershed_area_stats.csv"
area_df = pd.read_csv(area_path)

rivers = ["Puelo", "Yelcho", "Palena", "Cisnes", "Aysen"]
areas = {"Puelo": area_df['total_area'][0], "Yelcho": area_df['total_area'][1], "Palena": area_df['total_area'][2], "Cisnes": area_df['total_area'][3], "Aysen": area_df['total_area'][4]} # km^2
areas_l = [areas[r] for r in rivers]

rvrclr = {"Puelo": 'b', "Yelcho": 'r', "Palena": 'y', "Cisnes": 'g', "Aysen": 'm'}
rvrclr_l = [rvrclr[r] for r in rivers]

rvrmkr = {"Puelo": 'v', "Yelcho": 's', "Palena": '*', "Cisnes": '^', "Aysen": 'D'}


# X codes for certain data sources
xcodes = {'Puelo': 'X10523002',
          'Yelcho': 'X10704002',
          'Palena': 'X11040001',
          'Cisnes':'X11147001',
          'Aysen': 'X11342001'}
xcodes_l = [xcodes[r] for r in rivers]
xcodes_r = dict(zip(xcodes_l, rivers))


with pd.ExcelFile(wq_path) as DATA:
#    Puelo_Flow  = pd.read_excel(DATA, 'Puelo_Flow',  comment="#")
#    Yelcho_Flow = pd.read_excel(DATA, 'Yelcho_Flow', comment="#")
#    Palena_Flow = pd.read_excel(DATA, 'Palena_Flow', comment="#")
#    Cisnes_Flow = pd.read_excel(DATA, 'Cisnes_Flow', comment="#")
#    Aysen_Flow  = pd.read_excel(DATA, 'Aysen_Flow',  comment="#")

    Aysen_Chem_raw  = pd.read_excel(DATA, 'Aysen',  skiprows=range(2), comment="#")
    Puelo_Chem_raw  = pd.read_excel(DATA, 'Puelo',  skiprows=range(2), comment="#")
    Cisnes_Chem_raw = pd.read_excel(DATA, 'Cisnes', skiprows=range(2), comment="#")
    Yelcho_Chem_raw = pd.read_excel(DATA, 'Yelcho', skiprows=range(2), comment="#")
    Palena_Chem_raw = pd.read_excel(DATA, 'Palena', skiprows=range(2), comment="#")


flow_table = pd.read_csv(flow_path, index_col="Date")
Puelo_Flow  = flow_table["Puelo"].rename("Flow").to_frame()
Yelcho_Flow = flow_table["Yelcho"].rename("Flow").to_frame()
Palena_Flow = flow_table["Palena"].rename("Flow").to_frame()
Cisnes_Flow = flow_table["Cisnes"].rename("Flow").to_frame()
Aysen_Flow  = flow_table["Aysen"].rename("Flow").to_frame()


flow_dfs = {
    "Aysen": Aysen_Flow,
    "Puelo": Puelo_Flow,
    "Cisnes": Cisnes_Flow,
    "Yelcho": Yelcho_Flow,
    "Palena": Palena_Flow
}

for (r, df) in flow_dfs.items():
    df.name = r

raw_chem_dfs = {
    "Aysen": Aysen_Chem_raw,
    "Puelo": Puelo_Chem_raw,
    "Cisnes": Cisnes_Chem_raw,
    "Yelcho": Yelcho_Chem_raw,
    "Palena": Palena_Chem_raw
}

for (r, df) in raw_chem_dfs.items():
    df.name = r

# average out the replicates
# mean_chem_dfs = dict()
# std_chem_dfs = dict()

#Following section is necessary for averaging out triplicates, but not needed on newer dataset (July 15, 2025). Uncomment if needed again


# stats_chem_dfs = dict()
# for (r, df) in raw_chem_dfs.items():
#     gb = df.drop(columns=["Team", "Site", "replicate"]).groupby("Date")
#     # means = gb.mean().rename("{} mean".format)
#     # stds = gb.std().rename("{} std".format)
#     # stats_chem_dfs[r] = pd.concat([means, stds], axis=1)
#     stats_chem_dfs[r] = gb.agg(["mean", "std"]).swaplevel(0, 1, axis=1) # TODO: Figure out how to use agg and multi index
#     # mean_chem_dfs[r] = gb.mean()
#     # std_chem_dfs[r] = gb.std()


# also affects the explicitly named dfs (Aysen_Chem, Puelo_Flow, etc. because in Python the dicts contain shallow copies
# leads to warning, the ability to do this may be deprecated in the future
#for df in flow_dfs.values():
#    df.set_index("Date", inplace=True)
for df in raw_chem_dfs.values():
    df.set_index("Date", inplace=True)

# if "Site" in Puelo_Chem.columns:
#     del Puelo_Chem["Site"]

# Replace b.d. with 0 --- probably does not do anything
for df in raw_chem_dfs.values():
    df.replace('b.d.', 0.0, inplace=True)
    df.replace('.', 0.0, inplace=True)

# # verify that all the chemical columns are the same and in the same order - not technically necessary but still useful
# all_chemicals = next(iter(chem_dfs.values())).columns
# for df in chem_dfs.values():
#     assert (df.columns==all_chemicals).all()


# The only one with replicates is Cisnes, so we'll just fix that real quick.
Cisnes_Chem_raw = Cisnes_Chem_raw.groupby("Date").agg(lambda x: np.sum(x.astype("str")) if x.dtype=="object" else np.mean(x))
#Cisnes_Chem_raw[:, Cisnes_Chem_raw.dtypes != "object"]
raw_chem_dfs["Cisnes"] = Cisnes_Chem_raw

assert all([df.index.is_unique for df in raw_chem_dfs.values()])