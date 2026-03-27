import pandas as pd
import os


pwd = os.getcwd().split("/")

try:
    River_Data_path = os.environ["../../../data/chemistry/cleaned_data/wq_uncen_all.xlsx"]
except KeyError:
    # this module could be called from anywhere, so it has to figure out the river data path
    #River_Data_path = "/".join(pwd[:pwd.index("Research")]) + "/Research/data/River_Data.xlsx"
    River_Data_path = "../../../data/chemistry/cleaned_data/wq_uncen_all.xlsx"

rivers = ["Aysen", "Puelo", "Palena", "Cisnes", "Yelcho"]
areas = {"Aysen": 12181.39361, "Puelo": 9121.633249, "Palena": 13147.56309, "Cisnes": 5116.698637, "Yelcho": 11369.27711} # km^2
areas_l = [areas[r] for r in rivers]

rvrclr = {"Aysen": 'b', "Puelo": 'r', "Palena": 'y', "Cisnes": 'g', "Yelcho": 'm'}
rvrclr_l = [rvrclr[r] for r in rivers]

rvrmkr = {"Aysen": 'v', "Puelo": 's', "Palena": '*', "Cisnes": '^', "Yelcho": 'D'}


# X codes for certain data sources
xcodes = {'Puelo': 'X10523002',
          'Yelcho': 'X10704002',
          'Palena': 'X11040001',
          'Cisnes':'X11147001',
          'Aysen': 'X11342001'}
xcodes_l = [xcodes[r] for r in rivers]
xcodes_r = dict(zip(xcodes_l, rivers))


with pd.ExcelFile(River_Data_path) as DATA:
    Aysen_Flow  = pd.read_excel(DATA, 'Aysen_Flow',  comment="#")
    Puelo_Flow  = pd.read_excel(DATA, 'Puelo_Flow',  comment="#")
    Cisnes_Flow = pd.read_excel(DATA, 'Cisnes_Flow', comment="#")
    Yelcho_Flow = pd.read_excel(DATA, 'Yelcho_Flow', comment="#")
    Palena_Flow = pd.read_excel(DATA, 'Palena_Flow', comment="#")

    Aysen_Chem_raw  = pd.read_excel(DATA, 'Aysen_Chem',  skiprows=range(2), comment="#")
    Puelo_Chem_raw  = pd.read_excel(DATA, 'Puelo_Chem',  skiprows=range(2), comment="#")
    Cisnes_Chem_raw = pd.read_excel(DATA, 'Cisnes_Chem', skiprows=range(2), comment="#")
    Yelcho_Chem_raw = pd.read_excel(DATA, 'Yelcho_Chem', skiprows=range(2), comment="#")
    Palena_Chem_raw = pd.read_excel(DATA, 'Palena_Chem', skiprows=range(2), comment="#")

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
stats_chem_dfs = dict()
for (r, df) in raw_chem_dfs.items():
    gb = df.drop(columns=["Team", "Site", "replicate"]).groupby("Date")
    # means = gb.mean().rename("{} mean".format)
    # stds = gb.std().rename("{} std".format)
    # stats_chem_dfs[r] = pd.concat([means, stds], axis=1)
    stats_chem_dfs[r] = gb.agg(["mean", "std"]).swaplevel(0, 1, axis=1) # TODO: Figure out how to use agg and multi index
    # mean_chem_dfs[r] = gb.mean()
    # std_chem_dfs[r] = gb.std()


# also affects the explicitly named dfs (Aysen_Chem, Puelo_Flow, etc. because in Python the dicts contain shallow copies
# leads to warning, the ability to do this may be deprecated in the future
for df in flow_dfs.values():
    df.set_index("Date", inplace=True)
for df in raw_chem_dfs.values():
    df.set_index("Date", inplace=True)

# if "Site" in Puelo_Chem.columns:
#     del Puelo_Chem["Site"]

# for df in chem_dfs.values():
#     df.replace('b.d.', 0.0, inplace=True)
#     df.replace('.', 0.0, inplace=True)

# # verify that all the chemical columns are the same and in the same order - not technically necessary but still useful
# all_chemicals = next(iter(chem_dfs.values())).columns
# for df in chem_dfs.values():
#     assert (df.columns==all_chemicals).all()