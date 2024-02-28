# clear environment
rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","001_libraries.R"))
source(here::here("R","002_folder-paths.R"))

explodeddata01 <- readRDS(here::here(RDS_path, "010_explodeddata01.RDS"))
multinomialLR_1 <- readRDS(here::here(RDS_path, "020_multinomialLR.RDS"))
multinomialLR_seendoc<-      readRDS(here::here(RDS_path, "020_multinomialLR_seendoc.RDS"))

explodeddata01$demcat4 <- factor(explodeddata01$demcat4, levels = c(1:4),
                                 labels = c("AD/ADRD", "MCI/CIND", "Other medical or neuropsychiatric", "Normal"))
explodeddata01$seendoctorformemory1Y0N <- factor(explodeddata01$seendoctorformemory1Y0N, levels = c(0, 1),
                                                 labels = c("Did not see doctor for memory problems", "Has seen doctor for memory problems"))


## Generate vars

epidata <- explodeddata01 %>% 
  dplyr::mutate(AD = dplyr::if_else(demcat4 == "AD/ADRD", 1, 0),
                MCI = dplyr::if_else(demcat4 == "MCI/CIND", 1, 0),
                MD = dplyr::if_else(seendoctorformemory1Y0N == "Has seen doctor for memory problems", 1, 0),
                MMSE_r = 30-ANMSETOT,
                NotAD = 1 - AD,
                NotADorMCI = 1 - (MCI | AD),
                MCIorAD = 1 - NotADorMCI)

epi_stats_table <- table(mmse = epidata$ANMSETOT, ad = epidata$AD) %>% 
  as_tibble() %>%
  pivot_wider(id_cols = "mmse", names_from = "ad", values_from = "n") %>% 
  mutate(TP = cumsum(`1`), # sum those with dementia diagnosis
         FP = cumsum(`0`), # sume those without dementia diagnosis
         TN = 8213 - FP, # 8194 in dataset we know do not have dementia, so if we take away the false positives, the leftovers are true negatives
         FN = 9491 - (TP + FP + TN), # 9346 is total in dataset
         SN =  round(TP/(TP+FN),2),
         SP =  round(TN/(TN+FP),2),
         J  =  round(SN+SP-1,2),
         PPV = round(TP/(TP+FP),2),
         NPV = round(TN/(TN+FN),2),
         mmse = as.numeric(mmse)) 

# Get predprob from multinomial model

overall_predprob <- as.data.frame(fitted(multinomialLR_1))

explodeddata01$prob_ADRD <- overall_predprob$`AD/ADRD`
explodeddata01$prob_normal <- overall_predprob$`Normal`

ADRD_prob <- explodeddata01 %>% 
  select(ANMSETOT, prob_ADRD, prob_normal) %>% 
  group_by(ANMSETOT) %>% 
  slice(1)

### Format table

table1 <- epi_stats_table %>% 
  select(-`1`, -`0`) %>% 
  left_join(ADRD_prob, by = c("mmse" = "ANMSETOT")) %>% 
  mutate(prob_ADRD = round(prob_ADRD, 2))

## Do it all again for folks who have seen a doctor for memory problems

epi_data_seendoc <- epidata %>% 
  filter(MD == 1)

epi_stats_table_seendoc <- table(mmse = epi_data_seendoc$ANMSETOT, ad = epi_data_seendoc$AD) %>% 
  as_tibble() %>%
  pivot_wider(id_cols = "mmse", names_from = "ad", values_from = "n") %>% 
  mutate(TP = cumsum(`1`), # sum those with dementia diagnosis
         FP = cumsum(`0`), # sume those without dementia diagnosis
         TN = 204 - FP, # 204 in dataset we know do not have dementia, so if we take away the false positives, the leftovers are true negatives
         FN = 757 - (TP + FP + TN), # 757 is total in dataset
         SN =  round(TP/(TP+FN),2),
         SP =  round(TN/(TN+FP),2),
         J  =  round(SN+SP-1,2),
         PPV = round(TP/(TP+FP),2),
         NPV = round(TN/(TN+FN),2),
         mmse = as.numeric(mmse)) 

seendoc_predprob <- as.data.frame(fitted(multinomialLR_seendoc))

epi_data_seendoc$prob_ADRD <- seendoc_predprob$`AD/ADRD`
epi_data_seendoc$prob_normal <- seendoc_predprob$`Normal`

ADRD_prob_seendoc <- epi_data_seendoc %>% 
  select(ANMSETOT, prob_ADRD, prob_normal) %>% 
  group_by(ANMSETOT) %>% 
  slice(1) %>% 
  mutate(prob_normal = round(prob_normal, digits = 2))

table2 <- epi_stats_table_seendoc %>% 
  select(-`1`, -`0`) %>% 
  left_join(ADRD_prob_seendoc, by = c("mmse" = "ANMSETOT")) %>% 
  mutate(prob_ADRD = round(prob_ADRD, 2))


#####
## Do again for AD and MCI
#####


epi_stats_table_mciad <- table(mmse = epidata$ANMSETOT, mciad = epidata$MCIorAD) %>% 
  as_tibble() %>%
  pivot_wider(id_cols = "mmse", names_from = "mciad", values_from = "n") %>% 
  mutate(TP = cumsum(`1`), # sum those with AD or MCI diagnosis
         FP = cumsum(`0`), # sume those without AD or MCI diagnosis
         TN = 7193 - FP, # 8194 in dataset we know do not have AD or MCI, so if we take away the false positives, the leftovers are true negatives
         FN = 9491 - (TP + FP + TN), # 9346 is total in dataset
         SN =  round(TP/(TP+FN),2),
         SP =  round(TN/(TN+FP),2),
         J  =  round(SN+SP-1,2),
         PPV = round(TP/(TP+FP),2),
         NPV = round(TN/(TN+FN),2),
         mmse = as.numeric(mmse)) 

# Get predprob from multinomial model

overall_predprob <- as.data.frame(fitted(multinomialLR_1))

explodeddata01$prob_ADRD <- overall_predprob$`AD/ADRD`
explodeddata01$prob_MCI <- overall_predprob$`MCI/CIND`
explodeddata01$prob_normal <- overall_predprob$`Normal`

admci_prob <- explodeddata01 %>% 
  mutate(prob_ADMCI = prob_ADRD + prob_MCI) %>% 
  select(ANMSETOT, prob_ADMCI, prob_normal) %>% 
  group_by(ANMSETOT) %>% 
  slice(1)

### Format table

tables4 <- epi_stats_table_mciad %>% 
  select(-`1`, -`0`) %>% 
  left_join(admci_prob, by = c("mmse" = "ANMSETOT")) %>% 
  mutate(prob_ADMCI = round(prob_ADMCI, 2),
         prob_normal = round(prob_normal, 2))

# again for mem prob

epi_stats_table_mciad_seendoc <- table(mmse = epi_data_seendoc$ANMSETOT, mciad = epi_data_seendoc$MCIorAD) %>% 
  as_tibble() %>%
  pivot_wider(id_cols = "mmse", names_from = "mciad", values_from = "n") %>% 
  mutate(TP = cumsum(`1`), # sum those with AD or MCI diagnosis
         FP = cumsum(`0`), # sume those without AD or MCI diagnosis
         TN = 153 - FP, # 153 in dataset we know do not have AD or MCI, so if we take away the false positives, the leftovers are true negatives
         FN = 757 - (TP + FP + TN), # 757 is total in dataset
         SN =  round(TP/(TP+FN),2),
         SP =  round(TN/(TN+FP),2),
         J  =  round(SN+SP-1,2),
         PPV = round(TP/(TP+FP),2),
         NPV = round(TN/(TN+FN),2),
         mmse = as.numeric(mmse)) 

# Get predprob from multinomial model

seendoc_predprob <- as.data.frame(fitted(multinomialLR_seendoc))

epi_data_seendoc$prob_ADRD <- seendoc_predprob$`AD/ADRD`
epi_data_seendoc$prob_MCI <- seendoc_predprob$`MCI/CIND`
epi_data_seendoc$prob_normal <- seendoc_predprob$`Normal`

admci_prob_seendoc <- epi_data_seendoc %>% 
  mutate(prob_ADMCI = prob_ADRD + prob_MCI) %>% 
  select(ANMSETOT, prob_ADMCI, prob_normal) %>% 
  group_by(ANMSETOT) %>% 
  slice(1)

### Format table

tables5 <- epi_stats_table_mciad_seendoc %>% 
  select(-`1`, -`0`) %>% 
  left_join(admci_prob_seendoc, by = c("mmse" = "ANMSETOT")) %>% 
  mutate(prob_ADMCI = round(prob_ADMCI, 2),
         prob_normal = round(prob_normal, 2))

## Get in-line reporting results

community_ppv <- table1 %>% 
  dplyr::filter(mmse == 23) %>% 
  dplyr::select(PPV) %>% 
  pull()

clinical_ppv <- table2 %>% 
  dplyr::filter(mmse == 23) %>% 
  dplyr::select(PPV) %>% 
  pull()

community_probadrd <- table1 %>% 
  dplyr::filter(mmse == 23) %>% 
  dplyr::select(prob_ADRD) %>% 
  pull()

clinical_probadrd <- table2 %>% 
  dplyr::filter(mmse == 23) %>% 
  dplyr::select(prob_ADRD) %>% 
  pull()

community_maxj <- table1 %>% 
  dplyr::slice_max(J) %>% 
  dplyr::select(mmse) %>% 
  pull()
  
clinical_maxj <- table2 %>% 
  dplyr::slice_max(J) %>% 
  dplyr::select(mmse) %>% 
  pull()

## Generate Table 1

community_half_table1 <- table1 %>% 
  dplyr::filter(mmse == 17 |
                mmse == 18 |
                mmse == 22 |
                mmse == 23 |
                mmse == 24 |
                mmse == 26) %>% 
  dplyr::select(mmse, SN, SP, J, PPV, NPV, prob_ADRD, prob_normal) %>% 
  dplyr::rename(`Cut point` = mmse,
                Sensitivity = SN,
                Specificity = SP,
                `Youden's J` = J,
                `Positive predictive value` = PPV,
                `Negative predictive value` = NPV,
                `Conditional probability dementia` = prob_ADRD,
                `Conditional probability normal cognition` = prob_normal)

clinical_half_table1 <- table2 %>% 
  dplyr::filter(mmse == 17 |
                  mmse == 18 |
                  mmse == 22 |
                  mmse == 23 |
                  mmse == 24 |
                  mmse == 26) %>% 
  dplyr::select(mmse, SN, SP, J, PPV, NPV, prob_ADRD, prob_normal) %>% 
  dplyr::rename(`Cut point` = mmse,
                Sensitivity = SN,
                Specificity = SP,
                `Youden's J` = J,
                `Positive predictive value` = PPV,
                `Negative predictive value` = NPV,
                `Conditional probability dementia` = prob_ADRD,
                `Conditional probability normal cognition` = prob_normal) 

reducedtable1 <- bind_rows(community_half_table1, clinical_half_table1)
  
save.image(here::here(RDS_path, "030_sespnpvppv.Rdata"))