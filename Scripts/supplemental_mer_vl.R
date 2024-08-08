
rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(mozR)
library(glamr)
library(glue)
library(fs)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)
library(googlesheets4)
library(googledrive)
load_secrets() 


# VALUES & PATHS -----------------------------------------------------------

# update each month
period <- "2024 Q3" # UPDATE EACH QUARTER
file <- glue::glue("mer_supplemental_vl_{period}")



path_monthly_input_repo <- glue::glue("Data/supplemental/{period}/") # paths for inmporting monthly ip submissions


# do not update each month
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U") # path for fetching ajuda site map in google sheets
path_quarterly_output_repo <- "Dataout/supplemental_vl/" # folder path where quarterly dataset archived
path_quarterly_output_file <- path(path_quarterly_output_repo, file, ext = "txt") # composite path/filename where quarterly dataset saved
path_quarterly_output_gdrive <- as_id("1qXRO_6rSBJeg8DI8Q-cYjObdj5VJQiKT") # google drive folder where quarterly dataset saved 
path_historic_output_file <- "Dataout/mer_supplemental_vl.txt" # folder path where historic dataset archived
path_historic_output_gdrive <- as_id("1VNaZ2LnaUaW9IayH8Nqciz3tDo02vV56") # google drive folder where historic dataset saved


# METADATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap() %>% 
  select(
    datim_uid,
    sisma_uid,
    site_nid,
    snu,
    psnu,
    sitename,
    ends_with("tude"),
    starts_with("partner_"),
    grm_sernap,
    starts_with("his_"),
    starts_with("program_"),
    security_insecure)


# PROCESS IP SUBMISSIONS --------------------------------------------------


input_files <- dir({path_monthly_input_repo}, pattern = "*.xlsx")

df <- input_files %>%
  map(~ reshape_sup_vl(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind)


# CHECK ALL SITES AJUDA CODED -----------------------------------------------------------

# detect lines not coded with datim_uids
df %>% 
  distinct(datim_uid, snu, psnu, sitename) %>% 
  anti_join(ajuda_site_map, by = "datim_uid")


# PRINT OUTPUT TO DISK ------------------------------------------------------


readr::write_tsv(
  df,
  {path_quarterly_output_file},
  na ="")


#---- SURVEY ALL MONTHLY TXTB DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_quarterly_output_repo}, pattern = "*.txt")

vl_source_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(path_quarterly_output_repo, .))) %>%
  reduce(rbind)


#---- JOIN AJUDA SITEMAP AND CLEAN DATAFRAME -----------------------


vl_source_tidy_history_1 <- vl_source_tidy_history %>%
  dplyr::select(!c(snu, psnu, sitename)) %>% 
  dplyr::left_join(ajuda_site_map, by = "datim_uid") %>% 
  dplyr::select(datim_uid,
                sisma_uid,
                site_nid,
                period,
                starts_with("partner_"),
                grm_sernap,
                snu,
                psnu,
                sitename,
                ends_with("tude"),
                starts_with("support"),
                starts_with("his"),
                security_insecure,
                sex,
                age,
                pop_type,
                pop_subtype,
                keypop,
                motive,
                source,
                indicator,
                value)


# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  vl_source_tidy_history_1,
  path_historic_output_file)


# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)

