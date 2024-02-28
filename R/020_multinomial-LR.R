rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","001_libraries.R"))
source(here::here("R","002_folder-paths.R"))

#load data

explodeddata01 <- readRDS(here::here(RDS_path, "010_explodeddata01.RDS"))

# use "normal" as the reference category
explodeddata01$demcat4 <- factor(explodeddata01$demcat4, levels = c(1:4),
                                  labels = c("AD/ADRD", "MCI/CIND", "Other medical or neuropsychiatric", "Normal"))

explodeddata01$demcat4 <- relevel(explodeddata01$demcat4, ref = "Normal")

# run a multinomial model as function of MMSE

multinomialLR_1 <- nnet::multinom(demcat4 ~ ANMSETOT, data = explodeddata01)

# again but with rcs

multinomialLR_2 <- nnet::multinom(demcat4 ~ rms::rcs(ANMSETOT), data = explodeddata01)

# compare models

anova(multinomialLR_1, multinomialLR_2)

# Likelihood ratio tests of Multinomial Models

# Response: demcat4
#                 Model Resid. df Resid. Dev   Test    Df LR stat. Pr(Chi)
# 1           ANMSETOT     28467   13897.73                              
# 2 rms::rcs(ANMSETOT)     28458   13578.76 1 vs 2     9  318.971       0

# compare R2

performance::r2_mcfadden(multinomialLR_1) # .29
performance::r2_mcfadden(multinomialLR_2) # .32

# use RCS!

# run again, stratified by seen doctor status

explodeddata01$seendoctorformemory1Y0N <- factor(explodeddata01$seendoctorformemory1Y0N, levels = c(0, 1),
                                                  labels = c("Did not see doctor for memory problems", "Has seen doctor for memory problems"))


seendoctordata <- explodeddata01 %>% 
  dplyr::filter(seendoctorformemory1Y0N == "Has seen doctor for memory problems")

notseendoctordata <- explodeddata01 %>% 
  dplyr::filter(seendoctorformemory1Y0N == "Did not see doctor for memory problems")

multinomialLR_seendoc <- nnet::multinom(demcat4 ~ rms::rcs(ANMSETOT), data = seendoctordata)
multinomialLR_notseendoc <- nnet::multinom(demcat4 ~ rms::rcs(ANMSETOT), data = notseendoctordata)



saveRDS(multinomialLR_2, here::here(RDS_path, "020_multinomialLR.RDS"))
saveRDS(multinomialLR_seendoc, here::here(RDS_path, "020_multinomialLR_seendoc.RDS"))
saveRDS(multinomialLR_notseendoc, here::here(RDS_path, "020_multinomialLR_notseendoc.RDS"))
saveRDS(seendoctordata, here::here(RDS_path, "020_seendoctordata.RDS"))
saveRDS(notseendoctordata, here::here(RDS_path, "020_notseendoctordata.RDS"))
save.image(here::here(RDS_path, "020_multinomial-LR.Rdata"))