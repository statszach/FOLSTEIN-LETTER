rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","001_libraries.R"))
source(here::here("R","002_folder-paths.R"))


## AN_R has the MMSE. 

ADAMS1AN_R <- haven::read_sav(fs::path(data_path, "ADAMS1AN_R.sav"))

# AJ_R has DSM-IV and DSM-III

ADAMS1AJ_R <- haven::read_sav(fs::path(data_path, "ADAMS1AJ_R.sav"))

## Loading tracker filer for demographics and weights

ADAMS1TRK_R <- haven::read_dta(fs::path(data_path, "ADAMS1TRK_R.dta"))

## AD_R has dementia information

ADAMS1AD_R <- haven::read_sav(fs::path(data_path, "ADAMS1AD_R.sav"))

## AM_R has medical history

ADAMS1AM_R <- haven::read_sav(fs::path(data_path, "ADAMS1AM_R.sav"))

## AB_R has depression treatment

ADAMS1AB_R <- haven::read_sav(fs::path(data_path, "ADAMS1AB_R.sav"))

## AG_R has caregiver info

ADAMS1AG_R <- haven::read_sav(fs::path(data_path, "ADAMS1AG_R.sav"))

## Save out files

saveRDS(ADAMS1AN_R, here::here(RDS_path, "010_ADAMS1AN_R.RDS"))
saveRDS(ADAMS1AJ_R, here::here(RDS_path, "010_ADAMS1AJ_R.RDS"))
saveRDS(ADAMS1TRK_R, here::here(RDS_path, "010_ADAMS1TRK_R.RDS"))
saveRDS(ADAMS1AD_R, here::here(RDS_path, "010_ADAMS1AD_R.RDS"))
saveRDS(ADAMS1AM_R, here::here(RDS_path, "010_ADAMS1AM_R.RDS"))
saveRDS(ADAMS1AB_R, here::here(RDS_path, "010_ADAMS1AB_R.RDS"))
saveRDS(ADAMS1AG_R, here::here(RDS_path, "010_ADAMS1AG_R.RDS"))