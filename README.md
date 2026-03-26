# NWP_Land_Sea_Fluxes
*Code and data for Fox et. al. (2026) Riverine Biogeochemical Exports from Major Watersheds
to the Northwest Patagonian Estuarine Network*

*Created by Conner Sage Fox*

*Contributions from Nicholas Fox*

2026-3-25

### Folder structure
```
.
|--results/
|  |--* [gitignored]
|  |--wrts/
|     |--csv/ [gitignored]
|     |--fig/ [gitignored]
|     |--netcdf/ [gitignored]
|--data/
|  |--geospatial/ [gitignored]
|--scripts/
   |--wrtds_python/
      |--library_folder/ [gitignored]
```  
The `scripts` folder contains Jupyter Notebook files for creating the plots and tables that appear in the paper, as well as .py modules and R scripts to support them.

These notebooks draw their data from the `data` folder, which is mirrored in in GitHub repo. However, its `geospatial` subfolder is not mirrored because it contains large files (over 120MB). 

