data_path <- fs::path(QSPtools::network_path(),
                      "STUDIES", "HRSADAMS", "data", "SOURCE")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "Replicating_Folstein", "R", "Images"))
images_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "Replicating_Folstein", "R", "Images")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "Replicating_Folstein", "R", "Mplus"))
Mplus_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "Replicating_Folstein", "R", "Mplus")

fs::dir_create(here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "Replicating_Folstein", "R", "RDS"))
RDS_path <- here::here(QSPtools::network_path(), "STUDIES", "HRSADAMS", "Replicating_Folstein", "R","RDS")