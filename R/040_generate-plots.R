rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R","001_libraries.R"))
source(here::here("R","002_folder-paths.R"))

#load data

explodeddata01 <- readRDS(here::here(RDS_path, "010_explodeddata01.RDS"))
multinomialLR_2 <- readRDS(here::here(RDS_path, "020_multinomialLR.RDS"))
seendoctordata<-   readRDS(here::here(RDS_path, "020_seendoctordata.RDS"))
notseendoctordata<-readRDS(here::here(RDS_path, "020_notseendoctordata.RDS"))
multinomialLR_seendoc<-      readRDS(here::here(RDS_path, "020_multinomialLR_seendoc.RDS"))
multinomialLR_notseendoc<-   readRDS(here::here(RDS_path, "020_multinomialLR_notseendoc.RDS"))

explodeddata01$demcat4 <- factor(explodeddata01$demcat4, levels = c(1:4),
                                 labels = c("AD/ADRD", "MCI/CIND", "Other medical or neuropsychiatric", "Normal"))
explodeddata01$seendoctorformemory1Y0N <- factor(explodeddata01$seendoctorformemory1Y0N, levels = c(0, 1),
                                                 labels = c("Did not see doctor for memory problems", "Has seen doctor for memory problems"))


overall_data <- explodeddata01

overall_predprob <- as.data.frame(fitted(multinomialLR_2))

overall_data$prob_ADRD <- overall_predprob$`AD/ADRD`
overall_data$prob_MCI <- overall_predprob$`MCI/CIND`
overall_data$prob_other <- overall_predprob$`Other medical or neuropsychiatric`
overall_data$prob_normal <- overall_predprob$`Normal`

## Overall Plot 

stacked_prob_plot <- overall_data %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, contains("prob")) %>% 
  pivot_longer(!ANMSETOT, names_to = "diagnosis", values_to = "prob") %>% 
  mutate(ordered_diagnosis = case_when(diagnosis == "prob_ADRD" ~ 4,
                                       diagnosis == "prob_MCI" ~ 3,
                                       diagnosis == "prob_other" ~ 2,
                                       diagnosis == "prob_normal" ~ 1))




stacked_prob_plot$ordered_diagnosis <- factor(stacked_prob_plot$ordered_diagnosis,
                                              levels = c(1, 2, 3, 4),
                                              labels = c("Normal", "Other",
                                                         "MCI/CIND", "ADRD"))

overall_unweighted_plot <- ggplot(stacked_prob_plot, aes(fill = ordered_diagnosis,
                                                         y = prob, x = ANMSETOT)) +
  geom_bar(position="fill", stat = "identity") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "MMSE Score",
       y = "Probability of diagnostic group membership") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"))

## Stacked N

overall_adrd_n <- explodeddata01 %>% 
  filter(demcat4 == "AD/ADRD") %>% 
  nrow() 

overall_mci_n <- explodeddata01 %>% 
  filter(demcat4 == "MCI/CIND") %>% 
  nrow() 

overall_other_n <- explodeddata01 %>% 
  filter(demcat4 == "Other medical or neuropsychiatric") %>% 
  nrow()

overall_normal_n <- explodeddata01 %>% 
  filter(demcat4 == "Normal") %>% 
  nrow()

stacked_sample_data <- overall_data %>% 
  mutate(ADRD_n = overall_adrd_n*prob_ADRD,
         MCI_n = overall_mci_n*prob_MCI,
         other_n = overall_other_n*prob_other,
         normal_n = overall_normal_n*prob_normal)

weighted_prob_plot <- stacked_sample_data %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, ADRD_n, MCI_n, other_n, normal_n) %>% 
  pivot_longer(!ANMSETOT, names_to = "diagnosis", values_to = "N") %>% 
  mutate(ordered_diagnosis = case_when(diagnosis == "ADRD_n" ~ 4,
                                       diagnosis == "MCI_n" ~ 3,
                                       diagnosis == "other_n" ~ 2,
                                       diagnosis == "normal_n" ~ 1),
         N = round(N, digits = 2))

total_by_mmse <- weighted_prob_plot %>% 
  group_by(ANMSETOT) %>% 
  dplyr::summarize(Freq = sum(N))

weighted_prob_data <- left_join(weighted_prob_plot, total_by_mmse,
                                by = "ANMSETOT") %>% 
  mutate(prob = N / Freq)

weighted_prob_data$ordered_diagnosis <- factor(weighted_prob_data$ordered_diagnosis,
                                               levels = c(1, 2, 3, 4),
                                               labels = c("Normal", "Other",
                                                          "MCI/CIND", "ADRD"))

overall_weighted_plot <- ggplot(weighted_prob_data, aes(fill = ordered_diagnosis,
                                                        y = prob, x = ANMSETOT)) +
  geom_bar(position="fill", stat = "identity") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "MMSE Score",
       y = "Proportion") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"))


### Seen doc plots

seendoc_predprob <- as.data.frame(fitted(multinomialLR_seendoc))

seendoctordata$prob_ADRD <- seendoc_predprob$`AD/ADRD`
seendoctordata$prob_MCI <- seendoc_predprob$`MCI/CIND`
seendoctordata$prob_other <- seendoc_predprob$`Other medical or neuropsychiatric`
seendoctordata$prob_normal <- seendoc_predprob$`Normal`

seendoctordata %>% 
  filter(ANMSETOT == 23) %>% 
  select(prob_ADRD)


seendoc_stacked_prob_plot <- seendoctordata %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, contains("prob")) %>% 
  pivot_longer(!ANMSETOT, names_to = "diagnosis", values_to = "prob") %>% 
  mutate(ordered_diagnosis = case_when(diagnosis == "prob_ADRD" ~ 4,
                                       diagnosis == "prob_MCI" ~ 3,
                                       diagnosis == "prob_other" ~ 2,
                                       diagnosis == "prob_normal" ~ 1))

seendoc_stacked_prob_plot$ordered_diagnosis <- factor(seendoc_stacked_prob_plot$ordered_diagnosis,
                                                      levels = c(1, 2, 3, 4),
                                                      labels = c("Normal", "Other",
                                                                 "MCI/CIND", "ADRD"))

seendoc_unweighted_plot <- ggplot(seendoc_stacked_prob_plot, aes(fill = ordered_diagnosis,
                                                                 y = prob, x = ANMSETOT)) +
  geom_bar(position="fill", stat = "identity") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "MMSE Score",
       y = "Probability of diagnostic group membership") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"))


seendoc_adrd_n <- seendoctordata %>% 
  filter(demcat4 == "AD/ADRD") %>% 
  nrow() 

seendoc_mci_n <- seendoctordata %>% 
  filter(demcat4 == "MCI/CIND") %>% 
  nrow() 

seendoc_other_n <- seendoctordata %>% 
  filter(demcat4 == "Other medical or neuropsychiatric") %>% 
  nrow()

seendoc_normal_n <- seendoctordata %>% 
  filter(demcat4 == "Normal") %>% 
  nrow()

seendoc_stacked_sample_data <- seendoctordata %>% 
  mutate(ADRD_n = seendoc_adrd_n*prob_ADRD,
         MCI_n = seendoc_mci_n*prob_MCI,
         other_n = seendoc_other_n*prob_other,
         normal_n = seendoc_normal_n*prob_normal)

seendoc_weighted_prob_plot <- seendoc_stacked_sample_data %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, ADRD_n, MCI_n, other_n, normal_n) %>% 
  pivot_longer(!ANMSETOT, names_to = "diagnosis", values_to = "N") %>% 
  mutate(ordered_diagnosis = case_when(diagnosis == "ADRD_n" ~ 4,
                                       diagnosis == "MCI_n" ~ 3,
                                       diagnosis == "other_n" ~ 2,
                                       diagnosis == "normal_n" ~ 1),
         N = round(N, digits = 2))

seendoc_total_by_mmse <- seendoc_weighted_prob_plot %>% 
  group_by(ANMSETOT) %>% 
  dplyr::summarize(Freq = sum(N))

seendoc_weighted_prob_data <- left_join(seendoc_weighted_prob_plot, seendoc_total_by_mmse,
                                        by = "ANMSETOT") %>% 
  mutate(prob = N / Freq)



seendoc_weighted_prob_data$ordered_diagnosis <- factor(seendoc_weighted_prob_data$ordered_diagnosis,
                                                       levels = c(1, 2, 3, 4),
                                                       labels = c("Normal", "Other",
                                                                  "MCI/CIND", "ADRD"))

seendoc_weighted_plot <- ggplot(seendoc_weighted_prob_data, aes(fill = ordered_diagnosis,
                                                                y = prob, x = ANMSETOT)) +
  geom_bar(position="fill", stat = "identity") +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "MMSE Score",
       y = "Proportion") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"))

## Stacked N plots


overall_predprob <- as.data.frame(fitted(multinomialLR_2))

explodeddata01$prob_ADRD <- overall_predprob$`AD/ADRD`
explodeddata01$prob_MCI <- overall_predprob$`MCI/CIND`
explodeddata01$prob_other <- overall_predprob$`Other medical or neuropsychiatric`
explodeddata01$prob_normal <- overall_predprob$`Normal`


overall_adrd_n <- explodeddata01 %>% 
  filter(demcat4 == "AD/ADRD") %>% 
  nrow() 

overall_mci_n <- explodeddata01 %>% 
  filter(demcat4 == "MCI/CIND") %>% 
  nrow() 

overall_other_n <- explodeddata01 %>% 
  filter(demcat4 == "Other medical or neuropsychiatric") %>% 
  nrow()

overall_normal_n <- explodeddata01 %>% 
  filter(demcat4 == "Normal") %>% 
  nrow()

samplesize_by_demcat_MMSE <- explodeddata01 %>% 
  select(demcat4, ANMSETOT) %>% 
  group_by(demcat4, ANMSETOT) %>% 
  dplyr::summarize(count = n())%>% 
  mutate(ordered_diagnosis = case_when(demcat4 == "AD/ADRD" ~ 4,
                                       demcat4 == "MCI/CIND" ~ 3,
                                       demcat4 == "Other medical or neuropsychiatric" ~ 2,
                                       demcat4 == "Normal" ~ 1))


samplesize_by_demcat_MMSE$ordered_diagnosis <- factor(samplesize_by_demcat_MMSE$ordered_diagnosis,
                                                      levels = c(1, 2, 3, 4),
                                                      labels = c("Normal", "Other",
                                                                 "MCI/CIND", "ADRD"))

overall_unweighted_N_plot <- ggplot(samplesize_by_demcat_MMSE, aes(fill = ordered_diagnosis,
                                                                   y = count, x = ANMSETOT)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "MMSE score",
       y = "UnN") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"))


sample_by_adrd <- samplesize_by_demcat_MMSE %>% 
  ungroup() %>% 
  filter(demcat4 == "AD/ADRD") %>% 
  rename(adrd_count = count) %>% 
  select(ANMSETOT, adrd_count)

sample_by_mci <- samplesize_by_demcat_MMSE %>% 
  ungroup() %>% 
  filter(ordered_diagnosis == "MCI/CIND") %>% 
  rename(mci_count = count) %>% 
  select(ANMSETOT, mci_count)

sample_by_other <- samplesize_by_demcat_MMSE %>%
  ungroup() %>% 
  filter(ordered_diagnosis == "Other") %>% 
  rename(other_count = count) %>% 
  select(ANMSETOT, other_count)

sample_by_norm <- samplesize_by_demcat_MMSE %>% 
  ungroup() %>% 
  filter(ordered_diagnosis == "Normal") %>% 
  rename(normal_count = count) %>% 
  select(ANMSETOT, normal_count)

base <- data.frame(ANMSETOT = 0:30)
merge1 <- left_join(base, sample_by_adrd, by = "ANMSETOT")
merge2 <- left_join(merge1, sample_by_mci, by = "ANMSETOT")
merge3 <- left_join(merge2, sample_by_other, by = "ANMSETOT")
N_by_MMSE_DIAG <- left_join(merge3, sample_by_norm, by = "ANMSETOT") %>% 
  mutate(adrd_count = dplyr::if_else(is.na(adrd_count), 0, as.numeric(adrd_count)),
         mci_count = dplyr::if_else(is.na(mci_count), 0, as.numeric(mci_count)),
         other_count = dplyr::if_else(is.na(other_count), 0, as.numeric(other_count)),
         normal_count = dplyr::if_else(is.na(normal_count), 0, as.numeric(normal_count)),
         total_count = adrd_count + mci_count + other_count + normal_count)

diagnosis_probs <- explodeddata01 %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, prob_ADRD, prob_MCI, prob_other, prob_normal) %>% 
  group_by(ANMSETOT) %>% 
  slice(1)

prep_stacked_plot_data <- left_join(N_by_MMSE_DIAG, diagnosis_probs, by = "ANMSETOT") %>% 
  group_by(ANMSETOT) %>% 
  mutate(adrd_count_prob = total_count*prob_ADRD,
         mci_count_prob = total_count*prob_MCI,
         other_count_prob = total_count*prob_other,
         normal_count_prob = total_count*(1-prob_ADRD)*(1-prob_MCI)*(1-prob_other))

stacked_n_plot <- prep_stacked_plot_data %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, adrd_count_prob, mci_count_prob, other_count_prob, normal_count_prob) %>% 
  pivot_longer(!ANMSETOT, names_to = "diagnosis", values_to = "N") %>% 
  mutate(ordered_diagnosis = case_when(diagnosis == "adrd_count_prob" ~ 4,
                                       diagnosis == "mci_count_prob" ~ 3,
                                       diagnosis == "other_count_prob" ~ 2,
                                       diagnosis == "normal_count_prob" ~ 1),
         N = round(N, digits = 2))

stacked_n_plot$ordered_diagnosis <- factor(stacked_n_plot$ordered_diagnosis,
                                           levels = c(1, 2, 3, 4),
                                           labels = c("Normal", "Other",
                                                      "MCI/CIND", "ADRD"))

overall_weighted_N_plot <- ggplot(stacked_n_plot, aes(fill = ordered_diagnosis,
                                                      y = N, x = ANMSETOT)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = c(0.25, 0.75)) +
  labs(x = "MMSE score",
       y = "N") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"),
                    name = "Diagnostic group")


seendoc_predprob <- as.data.frame(fitted(multinomialLR_seendoc))

seendoctordata$prob_ADRD <- seendoc_predprob$`AD/ADRD`
seendoctordata$prob_MCI <- seendoc_predprob$`MCI/CIND`
seendoctordata$prob_other <- seendoc_predprob$`Other medical or neuropsychiatric`
seendoctordata$prob_normal <- seendoc_predprob$`Normal`


seenmemdoc_adrd_n <- explodeddata01 %>% 
  filter(seendoctorformemory1Y0N == "Has seen doctor for memory problems") %>% 
  filter(demcat4 == "AD/ADRD") %>% 
  nrow() 

seenmemdoc_mci_n <- explodeddata01 %>% 
  filter(seendoctorformemory1Y0N == "Has seen doctor for memory problems") %>% 
  filter(demcat4 == "MCI/CIND") %>% 
  nrow() 

seenmemdoc_other_n <- explodeddata01 %>% 
  filter(seendoctorformemory1Y0N == "Has seen doctor for memory problems") %>% 
  filter(demcat4 == "Other medical or neuropsychiatric") %>% 
  nrow()

seenmemdoc_normal_n <- explodeddata01 %>% 
  filter(seendoctorformemory1Y0N == "Has seen doctor for memory problems") %>% 
  filter(demcat4 == "Normal") %>% 
  nrow()

seenmemdoc_samplesize_by_demcat_MMSE <- explodeddata01 %>% 
  filter(seendoctorformemory1Y0N == "Has seen doctor for memory problems") %>% 
  select(demcat4, ANMSETOT) %>% 
  group_by(demcat4, ANMSETOT) %>% 
  dplyr::summarize(count = n())%>% 
  mutate(ordered_diagnosis = case_when(demcat4 == "AD/ADRD" ~ 4,
                                       demcat4 == "MCI/CIND" ~ 3,
                                       demcat4 == "Other medical or neuropsychiatric" ~ 2,
                                       demcat4 == "Normal" ~ 1))


seenmemdoc_samplesize_by_demcat_MMSE$ordered_diagnosis <- factor(seenmemdoc_samplesize_by_demcat_MMSE$ordered_diagnosis,
                                                                 levels = c(1, 2, 3, 4),
                                                                 labels = c("Normal", "Other",
                                                                            "MCI/CIND", "ADRD"))

seenmemdoc_unweighted_N_plot <- ggplot(seenmemdoc_samplesize_by_demcat_MMSE, aes(fill = ordered_diagnosis,
                                                                                 y = count, x = ANMSETOT)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "MMSE score",
       y = "UnN") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"))


seenmemdoc_sample_by_adrd <- seenmemdoc_samplesize_by_demcat_MMSE %>% 
  ungroup() %>% 
  filter(demcat4 == "AD/ADRD") %>% 
  rename(adrd_count = count) %>% 
  select(ANMSETOT, adrd_count)

seenmemdoc_sample_by_mci <- seenmemdoc_samplesize_by_demcat_MMSE %>% 
  ungroup() %>% 
  filter(ordered_diagnosis == "MCI/CIND") %>% 
  rename(mci_count = count) %>% 
  select(ANMSETOT, mci_count)

seenmemdoc_sample_by_other <- seenmemdoc_samplesize_by_demcat_MMSE %>%
  ungroup() %>% 
  filter(ordered_diagnosis == "Other") %>% 
  rename(other_count = count) %>% 
  select(ANMSETOT, other_count)

seenmemdoc_sample_by_norm <- seenmemdoc_samplesize_by_demcat_MMSE %>% 
  ungroup() %>% 
  filter(ordered_diagnosis == "Normal") %>% 
  rename(normal_count = count) %>% 
  select(ANMSETOT, normal_count)

seenmemdoc_base <- data.frame(ANMSETOT = 0:30)
seenmemdoc_merge1 <- left_join(seenmemdoc_base, seenmemdoc_sample_by_adrd, by = "ANMSETOT")
seenmemdoc_merge2 <- left_join(seenmemdoc_merge1, seenmemdoc_sample_by_mci, by = "ANMSETOT")
seenmemdoc_merge3 <- left_join(seenmemdoc_merge2, seenmemdoc_sample_by_other, by = "ANMSETOT")
seenmemdoc_N_by_MMSE_DIAG <- left_join(seenmemdoc_merge3, seenmemdoc_sample_by_norm, by = "ANMSETOT") %>% 
  mutate(adrd_count = dplyr::if_else(is.na(adrd_count), 0, as.numeric(adrd_count)),
         mci_count = dplyr::if_else(is.na(mci_count), 0, as.numeric(mci_count)),
         other_count = dplyr::if_else(is.na(other_count), 0, as.numeric(other_count)),
         normal_count = dplyr::if_else(is.na(normal_count), 0, as.numeric(normal_count)),
         total_count = adrd_count + mci_count + other_count + normal_count)

seenmemdoc_diagnosis_probs <- seendoctordata %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, prob_ADRD, prob_MCI, prob_other, prob_normal) %>% 
  group_by(ANMSETOT) %>% 
  slice(1)

seenmemdoc_prep_stacked_plot_data <- left_join(seenmemdoc_N_by_MMSE_DIAG, seenmemdoc_diagnosis_probs, by = "ANMSETOT") %>% 
  group_by(ANMSETOT) %>% 
  mutate(adrd_count_prob = total_count*prob_ADRD,
         mci_count_prob = total_count*prob_MCI,
         other_count_prob = total_count*prob_other,
         normal_count_prob = total_count*(1-prob_ADRD)*(1-prob_MCI)*(1-prob_other))

seenmemdoc_stacked_n_plot <- seenmemdoc_prep_stacked_plot_data %>% 
  filter(!is.na(ANMSETOT)) %>% 
  select(ANMSETOT, adrd_count_prob, mci_count_prob, other_count_prob, normal_count_prob) %>% 
  pivot_longer(!ANMSETOT, names_to = "diagnosis", values_to = "N") %>% 
  mutate(ordered_diagnosis = case_when(diagnosis == "adrd_count_prob" ~ 4,
                                       diagnosis == "mci_count_prob" ~ 3,
                                       diagnosis == "other_count_prob" ~ 2,
                                       diagnosis == "normal_count_prob" ~ 1),
         N = round(N, digits = 2))

seenmemdoc_stacked_n_plot$ordered_diagnosis <- factor(seenmemdoc_stacked_n_plot$ordered_diagnosis,
                                                      levels = c(1, 2, 3, 4),
                                                      labels = c("Normal", "Other",
                                                                 "MCI/CIND", "ADRD"))

seenmemdoc_weighted_N_plot <- ggplot(seenmemdoc_stacked_n_plot, aes(fill = ordered_diagnosis,
                                                                    y = N, x = ANMSETOT)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "MMSE score",
       y = "N") +
  scale_fill_manual(values = c("#ECE7EF", "#B4A0C0", "#7C5991", "#441262"))


weighted_plot <- ggpubr::ggarrange(overall_weighted_plot, overall_weighted_N_plot,
                                   seendoc_weighted_plot, seenmemdoc_weighted_N_plot,
                                   nrow = 2, ncol = 2)

# unweighted_plot <- gridExtra::grid.arrange(overall_unweighted_stacked_probability_plot, overall_unweighted_stacked_n_plot,
#                                            seendoc_unweighted_stacked_probability_plot, seendoc_unweighted_stacked_n_plot,
#                                            nrow = 2)


saveRDS(weighted_plot, here::here(images_path, "040_weighted_plot.rds"))

ggsave(fs::path(images_path, "Folstein Replication Figure 1.pdf"),
       plot = weighted_plot,
       scale = 2,
       width = 6,
       height = 3.7,
       units = "in")

# ggsave(fs::path(images_path, "250_unweighted_combined.pdf"),
#        plot = unweighted_plot,
#        scale = 2,
#        width = 6,
#        height = 3.7,
#        units = "in")
