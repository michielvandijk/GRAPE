This repository contains scripts and auxiliary data related to the
construction of the WUR-ERS Global Research on Agriculture: Personnel &
Expenditures database, which contains data on the number of public agricultural
researchers and R&D expenditures for 190 countries, broadly covering the period
1960-2022.

More information on GRAPE is provided in this paper: 

Van Dijk, M. et al. (2025), A global database of public agricultural R&D investment: 1960-2022,

The GRAPE database is stored in a Zenodo repository: https://doi.org/10.5281/zenodo.15081424.

The repository contains a main folder that refers to the version of the GRAPE database, which includes two subfolders:

- scripts, which (a) includes the scripts to reproduce the approach that was used to impute missing public agricultural R&D and number of researchers data (impute_hr_rd.R and final_adjustments_rd_hr.R) and (b) the scripts to reproduce the figures in the paper (fig_manuscript.R) and the Supplementary Information (fig_si.R). In addition it includes a script with functions that are called to in several of the other scripts (functions.R).
- data. which contains several auxiliary data sets that are needed to for the validation exercise.




