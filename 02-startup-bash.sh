#######################################
## BASH STARTUP FOR ANALYSIS
#######################################

mkdir data
mkdir results

# download cleaned data files from the platform storage
dx download "cleandata/cleandata_infl.csv" -o "data/cleandata_infl.csv"
dx download "cleandata/cleandata_controls.csv" -o "data/cleandata_controls.csv"