rm(list = ls())

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(mozR)
library(glamr)
library(fs)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)
library(googlesheets4)
library(googledrive)
library(glue)
load_secrets() 


# VALUES & PATHS -----------------------------------------------------------

# update each month
period <- "2024 Q1" # UPDATE EACH QUARTER
file <- glue("mer_supplemental_kp_",{period})

path_monthly_input_repo <- glue::glue("Data/supplemental/{period}/") # paths for inmporting monthly ip submissions
path_quarterly_output_repo <- "Dataout/supplemental_kp/" # folder path where quarterly dataset archived
path_quarterly_output_file <- path(path_quarterly_output_repo, file, ext = "txt") # composite path/filename where quarterly dataset saved
path_quarterly_output_gdrive <- as_id("1qRgJwV9Mip331-dRAdLTaPbYTRqB4_Qf") # google drive folder where quarterly dataset saved 
path_historic_output_file <- "Dataout/mer_supplemental_kp.txt" # folder path where historic dataset archived
path_historic_output_gdrive <- as_id("1VNaZ2LnaUaW9IayH8Nqciz3tDo02vV56") # google drive folder where historic dataset saved

# METADATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap(sheet = "list_ajuda") %>%  
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


# FUNCTIONS RUN --------------------------------------------------


input_files <- dir({path_monthly_input_repo}, pattern = "*.xlsx")

df <- input_files %>%
  map(~ reshape_sup_kp(file.path(path_monthly_input_repo, .)), .progress	= TRUE) %>%
  reduce(rbind) %>% 
  group_by(across(!contains("TX_")), .drop = TRUE) %>% 
  summarize(across(contains("TX_"), sum, na.rm = TRUE))


# COMPILE DATASETS -----------------------------------------------------------


# detect lines not coded with datim_uids
df %>% 
  filter(is.na(datim_uid)) %>% 
  distinct(datim_uid, snu, psnu, sitename)



# PRINT OUTPUT TO DISK ------------------------------------------------------


readr::write_tsv(
  df,
  {path_quarterly_output_file})


# write to google drive
drive_put(path_quarterly_output_file,
          path = path_quarterly_output_gdrive,
          name = glue({file}, '.txt'))



# SURVEY ALL MONTHLY TXTB DATASETS THAT NEED TO BE COMBINED FOR HISTORIC DATASET ---------------------------------


historic_files <- dir({path_quarterly_output_repo}, pattern = "*.txt")

df_historic <- historic_files %>%
  map(~ read_tsv(file.path(path_quarterly_output_repo, .))) %>%
  reduce(rbind)


# JOIN AJUDA SITEMAP AND CLEAN DATAFRAME -----------------------


df_historic_clean <- df_historic %>%
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
                pop_type,
                keypop,
                age,
                starts_with("TX_"))



# PRINT FINAL OUTPUT TO DISK ----------------------------------------------


readr::write_tsv(
  df_historic_clean,
  path_historic_output_file)


# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive)


