
rm(list = ls())

# PURPOSE: This script processes quarterly supplemental MER transfer-in indicator results submitted by partners.  This indicator is used to complete the accounting of ins/outs of ART and is incorporated into a waterfall cascade visual
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-08-28
# NOTES: 

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(mozR)
library(glamr)
library(grabr)
library(glue)
library(fs)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)
library(googlesheets4)
library(googledrive)
library(Wavelength)
load_secrets() 


# VALUES & PATHS -----------------------------------------------------------

# update each month
period <- "2024 Q1" # UPDATE EACH QUARTER
file <- glue("mer_supplemental_trn_in_",{period})

path_monthly_input_repo <- glue::glue("Data/supplemental/{period}/") # paths for inmporting monthly ip submissions

# do not update each month
path_quarterly_output_repo <- "Dataout/supplemental_trn_in/quarterly_processed/" # folder path where quarterly dataset archived
path_quarterly_output_file <- path(path_quarterly_output_repo, file, ext = "txt") # composite path/filename where quarterly dataset saved
path_quarterly_output_gdrive <- as_id("1QS8cOpDcdtNTtrdY-fsvJfMHs3MTH4GY") # google drive folder where quarterly dataset saved
path_historic_output_file <- "Dataout/mer_waterfall_temp2.txt" # folder path where historic dataset archived
path_historic_output_gdrive <- as_id("1VNaZ2LnaUaW9IayH8Nqciz3tDo02vV56") # google drive folder where historic dataset saved

site_removal <- c("jKg6rpNATKH_ITECH", "W04POMswKUF")

# LOAD DATA -----------------------------------------------------------


ajuda_site_map <- pull_sitemap() %>%  
  select(
    datim_uid,
    sisma_uid,
    site_nid,
    ends_with("tude"),
    starts_with("partner_"),
    grm_sernap,
    starts_with("his_"),
    starts_with("program_"))


df_mer_waterfall <- read_delim("Dataout/supplemental_trn_in/mer_waterfall_cascade.txt",
                               delim = "\t", escape_double = FALSE,
                               trim_ws = TRUE) %>%
  filter(!datim_uid %in% c(site_removal))



cntry <- "Mozambique"
uid <- get_ouuid(cntry)
datim_orgsuids <- pull_hierarchy(uid, username = datim_user(), password = datim_pwd()) %>%
  filter(!is.na(facility) & !is.na(psnu)) %>%
  select(datim_uid = orgunituid,
         snu1,
         psnu = community, # changed to community with update to datim
         sitename = facility) %>%
  arrange(snu1, psnu, sitename)


# FUNCTIONS RUN --------------------------------------------------


input_files <- dir({path_monthly_input_repo}, pattern = "*.xlsx")

df <- input_files %>%
  map(~ reshape_sup_trn(file.path(path_monthly_input_repo, .)), .progress = TRUE) %>%
  reduce(rbind)


# QUARTERLY FILE WRITE ------------------------------------------------------


write_tsv(
  df,
  {path_quarterly_output_file},
  na ="")


# write to google drive
drive_put(path_quarterly_output_file,
          path = path_quarterly_output_gdrive,
          name = glue({file}, '.txt'))



# HISTORIC DATASET BUILD ---------------------------------


historic_files <- dir({path_quarterly_output_repo}, pattern = "*.txt")

df_trn_in_tidy_history <- historic_files %>%
  map(~ read_tsv(file.path(path_quarterly_output_repo, .))) %>%
  reduce(rbind)


# CLEAN HISTORIC DATASET -----------------------


df_trn_in_tidy_history_1 <- df_trn_in_tidy_history %>%
  pivot_longer(starts_with("TX_"), names_to = "indicator", values_to = "value") %>%
  mutate(indicator = recode(indicator, "TX_RTT" = "TX_RTT_SUP"),
         funding_agency = case_when(partner == "ECHO" ~ "USAID",
                                    partner == "JHPIEGO-DoD" ~ "DOD",
                                    TRUE ~ "HHS/CDC"),
         agecoarse = case_when(ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "<15",
                               ageasentered == "Unknown" ~ "Unknown Age",
                               TRUE ~ "15+"),
         ageasentered = case_match(ageasentered, "Unknown" ~ "Unknown Age", .default = ageasentered),
         sex = case_match(sex, "Unknown" ~ "Unknown Sex", .default = sex)) %>%
  rename(snu1 = snu,
         partner_recode = partner) %>%
  filter(!datim_uid %in% c(site_removal),
         !is.na(datim_uid))



# MER CLINICAL UNION ---------------------------------


df_waterfall_final <- bind_rows(df_mer_waterfall, df_trn_in_tidy_history_1) %>%
  select(!c(snu1, psnu, sitename)) %>%
  left_join(datim_orgsuids, by = "datim_uid") %>%
  left_join(ajuda_site_map, by = "datim_uid") %>%
  mutate(across(c(snu1, psnu, sitename),
                ~ case_when(datim_uid == "siMZUtd2cJW" ~ "_Military Mozambique",
                            TRUE ~ .))) %>%
  select(datim_uid,
         sisma_uid,
         site_nid,
         period,
         funding_agency,
         partner_recode,
         grm_sernap,
         snu1,
         psnu,
         sitename,
         ends_with("tude"),
         starts_with("program"),
         his_epts,
         his_emr,
         his_idart,
         his_disa,
         sex,
         starts_with("age"),
         otherdisaggregate,
         indicator,
         value) %>%
  filter(!is.na(value)) %>%
  glimpse()


df_waterfall_final %>%
  filter(is.na(datim_uid)) %>%
  view()


df_waterfall_final %>%
  filter(period == "2023 Q4") %>%
  distinct(period, ageasentered) %>%
  print(n=100)

df_waterfall_final %>%
  filter(period == "2023 Q4") %>%
  distinct(period, indicator) %>%
  print(n=100)

# HISTORIC FILE WRITE -----------------------------------------------------------


write_tsv(
  df_waterfall_final,
  path_historic_output_file,
  na = "")

# write to google drive
drive_put(path_historic_output_file,
          path = path_historic_output_gdrive,
          name = "mer_waterfall.txt")
