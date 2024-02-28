rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","001_libraries.R"))
source(here::here("R","002_folder-paths.R"))

ADAMS1AN_R  <- readRDS(here::here(RDS_path, "010_ADAMS1AN_R.RDS"))
ADAMS1AJ_R  <- readRDS(here::here(RDS_path, "010_ADAMS1AJ_R.RDS"))
ADAMS1TRK_R <- readRDS(here::here(RDS_path, "010_ADAMS1TRK_R.RDS"))
ADAMS1AD_R  <- readRDS(here::here(RDS_path, "010_ADAMS1AD_R.RDS"))
ADAMS1AM_R  <- readRDS(here::here(RDS_path, "010_ADAMS1AM_R.RDS"))
ADAMS1AB_R  <- readRDS(here::here(RDS_path, "010_ADAMS1AB_R.RDS"))
ADAMS1AG_R  <- readRDS(here::here(RDS_path, "010_ADAMS1AG_R.RDS"))

# Get MMSE scores and IDs

MMSETotandID <- ADAMS1AN_R %>% 
  dplyr::select(ADAMSSID, ANMSETOT) %>% 
  dplyr::mutate(ANMSETOT = na_if(ANMSETOT, 97))

table(is.na(MMSETotandID$ANMSETOT))

# Get clinical data

DementiaDX <- ADAMS1AD_R %>% 
  dplyr::select(ADAMSSID, ADFDX1, ADFDX2, ADFDX3, ADHACHSC, ADMH1, ADMH6, ADAPOE) %>% 
  dplyr::mutate(demcat = dplyr::case_when(
    ADFDX1 == "1"  | ADFDX2 == "1"  | ADFDX3 == "1" ~  1,
    ADFDX1 == "2"  | ADFDX2 == "2"  | ADFDX3 == "2" ~  1,
    ADFDX1 == "3"  | ADFDX2 == "3"  | ADFDX3 == "3" ~  1,
    ADFDX1 == "4"  | ADFDX2 == "4"  | ADFDX3 == "4" ~  1,
    ADFDX1 == "5"  | ADFDX2 == "5"  | ADFDX3 == "5" ~  1,
    ADFDX1 == "6"  | ADFDX2 == "6"  | ADFDX3 == "6" ~  1,
    ADFDX1 == "10" | ADFDX2 == "10" | ADFDX3 == "10" ~ 1,
    ADFDX1 == "13" | ADFDX2 == "13" | ADFDX3 == "13" ~ 1,
    ADFDX1 == "16" | ADFDX2 == "16" | ADFDX3 == "16" ~ 1,
    ADFDX1 == "18" | ADFDX2 == "18" | ADFDX3 == "18" ~ 1,
    ADFDX1 == "19" | ADFDX2 == "19" | ADFDX3 == "19" ~ 1,
    ADFDX1 == "32" | ADFDX2 == "32" | ADFDX3 == "32" ~ 1,
    
    ADFDX1 == "20" | ADFDX2 == "20" | ADFDX3 == "20" ~ 2,
    ADFDX1 == "21" | ADFDX2 == "21" | ADFDX3 == "21" ~ 2,
    ADFDX1 == "22" | ADFDX2 == "22" | ADFDX3 == "22" ~ 2,
    ADFDX1 == "33" | ADFDX2 == "33" | ADFDX3 == "33" ~ 2,
    
    ADFDX1 == "7"  | ADFDX2 == "7"  | ADFDX3 == "7"  ~ 3,
    ADFDX1 == "8"  | ADFDX2 == "8"  | ADFDX3 == "8"  ~ 3,
    ADFDX1 == "11" | ADFDX2 == "11" | ADFDX3 == "11" ~ 3,
    ADFDX1 == "14" | ADFDX2 == "14" | ADFDX3 == "14" ~ 3,
    ADFDX1 == "15" | ADFDX2 == "15" | ADFDX3 == "15" ~ 3,
    ADFDX1 == "17" | ADFDX2 == "17" | ADFDX3 == "17" ~ 3,
    ADFDX1 == "26" | ADFDX2 == "26" | ADFDX3 == "26" ~ 3,
    ADFDX1 == "27" | ADFDX2 == "27" | ADFDX3 == "27" ~ 3,
    ADFDX1 == "28" | ADFDX2 == "28" | ADFDX3 == "28" ~ 3,
    ADFDX1 == "29" | ADFDX2 == "29" | ADFDX3 == "29" ~ 3,
    
    ADFDX1 == "23" | ADFDX2 == "23" | ADFDX3 == "23" ~ 4,
    ADFDX1 == "24" | ADFDX2 == "24" | ADFDX3 == "24" ~ 4,
    
    ADFDX1 == "25" | ADFDX2 == "25" | ADFDX3 == "25" ~ 5,
    
    ADFDX1 == "30" | ADFDX2 == "30" | ADFDX3 == "30" ~ 6,
    
    ADFDX1 == "31" | ADFDX2 == "31" | ADFDX3 == "31" ~ 7),
    
    demcat4 = dplyr::case_when(
      demcat == 1 ~ 1,
      demcat == 2 ~ 2,
      demcat == 7 ~ 4,
      TRUE ~ 3),
    
    dementia = dplyr::if_else(demcat == 1, 1, 0),
    vasculardementia = dplyr::case_when(ADHACHSC == 97 ~ NA_real_,
                                        ADHACHSC == 12 ~ 1,
                                        ADHACHSC == 11 ~ 1,
                                        ADHACHSC == 10 ~ 1,
                                        ADHACHSC == 9 ~ 1,
                                        ADHACHSC == 8 ~ 1,
                                        TRUE ~ 0),
    seendoctorformemory1Y0N = dplyr::case_when(ADMH1 == 98 ~ 0,
                                               ADMH1 == 97 ~ 0,
                                               ADMH1 == 5 ~ 0,
                                               ADMH1 == 1 ~ 1),
    APOE41Y0N = dplyr::case_when(ADAPOE == 22 ~ 0,
                                 ADAPOE == 23 ~ 0,
                                 ADAPOE == 24 ~ 1,
                                 ADAPOE == 33 ~ 0,
                                 ADAPOE == 34 ~ 1,
                                 ADAPOE == 44 ~ 1,
                                 ADAPOE == 96 ~ NA_real_,
                                 ADAPOE == 97 ~ NA_real_)) %>% 
  dplyr::select(ADAMSSID, demcat4, seendoctorformemory1Y0N, APOE41Y0N)

# Get medical history

medhist1 <- ADAMS1AM_R %>% 
  dplyr::mutate(parkinsons1Y0N = dplyr::if_else(AM16 == 1, 1, 0),
                stroke1Y0N = dplyr::if_else(AM25 == 1, 1, 0)) %>% 
  dplyr::select(ADAMSSID, parkinsons1Y0N, stroke1Y0N)

# Now get depression

depression1 <- ADAMS1AB_R %>% 
  dplyr::mutate(treatdepression1Y0N = dplyr::case_when(ABNPD1 == 1 ~ 1, # yes = yes
                                                       ABNPD1 == 5 ~ 0, # no = no
                                                       ABNPD1 == 96 ~ 0, # skip logic = no
                                                       ABNPD1 == 97 ~ NA_real_, # not asked = missing
                                                       ABNPD1 == 98 ~ NA_real_)) %>% # don't know = missing
  dplyr::select(ADAMSSID, treatdepression1Y0N)

# Get caregiver-reported adls/iadls

caregiver1 <- ADAMS1AG_R %>% 
  dplyr::mutate(adl1 = dplyr::case_when(AGQ30A == 1 ~ 1,
                                        AGQ30A == 5 ~ 0),
                adl2 = dplyr::case_when(AGQ30B == 1 ~ 1,
                                        AGQ30B == 5 ~ 0),
                adl3 = dplyr::case_when(AGQ30C == 1 ~ 1,
                                        AGQ30C == 5 ~ 0),
                adl4 = dplyr::case_when(AGQ30D == 1 ~ 1,
                                        AGQ30D == 5 ~ 0),
                adl5 = dplyr::case_when(AGQ30E == 1 ~ 1,
                                        AGQ30E == 5 ~ 0),
                adl6 = dplyr::case_when(AGQ30F == 1 ~ 1,
                                        AGQ30F == 5 ~ 0),
                adl_sum = adl1 + adl2 + adl3 + adl4 + adl5 + adl6,
                iadl1 = dplyr::case_when(AGQ30G == 1 ~ 1,
                                         AGQ30G == 5 ~ 0,
                                         AGQ30G == 7 ~ NA_real_,
                                         AGQ30G == 8 ~ NA_real_),
                iadl2 = dplyr::case_when(AGQ30H == 1 ~ 1,
                                         AGQ30H == 5 ~ 0,
                                         AGQ30H == 7 ~ NA_real_,
                                         AGQ30H == 8 ~ NA_real_),
                iadl3 = dplyr::case_when(AGQ30I == 1 ~ 1,
                                         AGQ30I == 5 ~ 0,
                                         AGQ30I == 7 ~ NA_real_,
                                         AGQ30I == 8 ~ NA_real_),
                iadl4 = dplyr::case_when(AGQ30J == 1 ~ 1,
                                         AGQ30J == 5 ~ 0,
                                         AGQ30J == 7 ~ NA_real_,
                                         AGQ30J == 8 ~ NA_real_),
                iadl5 = dplyr::case_when(AGQ30K == 1 ~ 1,
                                         AGQ30K == 5 ~ 0,
                                         AGQ30K == 7 ~ NA_real_,
                                         AGQ30K == 8 ~ NA_real_),
                iadl_sum = iadl1 + iadl2 + iadl3 + iadl4 + iadl5) %>% 
  dplyr::select(ADAMSSID, adl_sum, iadl_sum)

## Now, pull demographics and weights from tracker file
## Need age, gender, race, and weights

demos1 <- ADAMS1TRK_R %>% 
  dplyr::select(ADAMSSID, AAGE, GENDER, RACE, ETHNIC, EDYRS, SECLUST, SESTRAT, AASAMPWT_F) 

### Merge datasets

merged <- MMSETotandID %>% 
  dplyr::left_join(demos1, by = "ADAMSSID") %>% 
  dplyr::left_join(DementiaDX, by = "ADAMSSID") %>% 
  dplyr::left_join(medhist1, by = "ADAMSSID") %>% 
  dplyr::left_join(depression1, by = "ADAMSSID") %>% 
  dplyr::left_join(caregiver1, by = "ADAMSSID")

## Make table

merged %>% 
  dplyr::select(-ADAMSSID, -SECLUST, -SESTRAT, -AASAMPWT_F) %>% 
  gtsummary::tbl_summary(type = ANMSETOT ~ "categorical",
                         statistic = gtsummary::all_continuous() ~ "{mean} ({sd})") %>% 
  gtsummary::as_gt() %>% 
  gt::gtsave("folstein-imputation-table.rtf")

# looks good

# impute missing data

preimp <- merged %>% 
  dplyr::mutate(GENDER = as.factor(GENDER),
                RACE = as.factor(RACE),
                ETHNIC = as.factor(ETHNIC),
                demcat4 = as.factor(demcat4),
                seendoctorformemory1Y0N = as.factor(seendoctorformemory1Y0N),
                APOE41Y0N = as.factor(APOE41Y0N),
                parkinsons1Y0N = as.factor(parkinsons1Y0N),
                stroke1Y0N = as.factor(stroke1Y0N),
                adl_sum = as.factor(adl_sum),
                iadl_sum = as.factor(iadl_sum))

table(is.na(preimp))

# 408 / (408 + 15000) 2.6%

imp01 <- mice(data = preimp, 
              m = 1, 
              seed = 8675309, 
              pred = quickpred(preimp, 
                               # exclude IDs/weights from imp routine
                               exc = c("ADAMSSID", "SECLUST", "SESTRAT", "AASAMPWT_F"))) 

imp_complete <- complete(imp01, "long")

## Explode data

explodeddata00 <- imp_complete %>% 
  dplyr::mutate(roundedweight = round(AASAMPWT_F / min(AASAMPWT_F), 0))

explodeddata01 <- explodeddata00 %>% #dividing by minimum weight and rounding
  slice(rep(seq_len(n()), roundedweight)) 

explodeddata02 <- splitstackshape::expandRows(explodeddata00, "roundedweight")


saveRDS(merged, here::here(RDS_path, "010_tidied_data.RDS"))
saveRDS(explodeddata01, here::here(RDS_path, "010_explodeddata01.RDS"))
saveRDS(explodeddata02, here::here(RDS_path, "010_explodeddata02.RDS"))

save.image(here::here(RDS_path, "010_tidy-data.Rdata"))