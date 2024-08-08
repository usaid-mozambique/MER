

# PURPOSE: To create a SIMS prioritization list using MER and Enhanced Monitoring indicator results
# AUTHOR:  Joe Lara | USAID
# DATE: 2022-05-23
# NOTES: 

# LOCALS & SETUP ------------------------


library(tidyverse)
library(lubridate)
library(glue)
library(gophr)
library(glamr)
library(janitor)
library(openxlsx)
library(readxl)
library(mozR)


# DEFINE PATHS & VALUES -----------------------------------------------------------

val_period <- "2024 Q2"

path_mer <- "Dataout/results_cumulative_new.rds"
path_tpt <- "Data/tpt_comp.txt"
path_output <- glue::glue("Dataout/site_scoring/site_scoring_{val_period}.xlsx")
path_monthly_output_repo <- "Dataout/site_scoring/" # folder path where historical datasets archived

list_indicators <- c("TX_CURR", 
                     "TX_CURR_6MO_PRIOR",
                     "TX_ML",
                     "TX_PVLS_PLW",
                     "TX_PVLS_PLW_D",
                     "TX_PVLS_D",
                     "TX_PVLS",
                     "TX_TB_D",
                     "PMTCT_STAT_POS",
                     "PMTCT_EID_D",
                     "PMTCT_EID_MOD",
                     "PMTCT_EID_Less_Equal_Two_Months",
                     "PMTCT_HEI_POS")

list_partners <- c("ECHO",
                   "EGPAF",
                   "FGH",
                   "ICAP",
                   "CCS",
                   "JHPEIGO",
                   "ARIEL", 
                   "ITECH")

# LOAD DATA ---------------------------------------------------------------

df_ajuda <- pull_sitemap() %>% 
  select(datim_uid,
         snu,
         psnu,
         sitename,
         partner_pepfar_clinical)

df <- read_rds(path_mer) %>% 
  filter(period_type == "results") %>% 
  select(datim_uid,
         snu1,
         psnu,
         sitename,
         funding_agency,
         mech_name,
         period,
         date,
         sex,
         ageasentered,
         agecoarse,
         standardizeddisaggregate,
         otherdisaggregate,
         all_of(list_indicators)) %>% 
  pivot_longer(TX_CURR:PMTCT_HEI_POS, names_to = "indicator", values_to = "value") %>% 
  filter(!is.na(value),
         mech_name %in% list_partners)

temp <- df |> 
  filter(period == val_period)

max_date <- max(temp$date)

rm(temp)

# CALCULATE SCORING -------------------------------------------------------

# TX_CURR
score_clin_txcurr <- df %>% 
  filter(indicator == "TX_CURR",
         period == val_period) %>% 
  group_by(datim_uid) %>% 
  summarize(value_txcurr = sum(value, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(score_txcurr = case_when(
    value_txcurr > 3500 ~ 1,
    between(value_txcurr, 1500, 3499) ~ 2,
    between(value_txcurr, 1000, 1490) ~ 3,
    between(value_txcurr, 700, 999) ~ 4,
    value_txcurr < 700 ~ 5,
    TRUE ~ 0))

# TX_CURR Pediatric
score_clin_txcurr_ped <- df %>% 
  filter(indicator == "TX_CURR",
         period == val_period,
         agecoarse == "<15") %>% 
  group_by(datim_uid) %>% 
  summarize(value_txcurr_ped = sum(value, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(score_txcurr_ped = case_when(
    value_txcurr_ped > 300 ~ 1,
    between(value_txcurr_ped, 150, 300) ~ 2,
    between(value_txcurr_ped, 80, 150) ~ 3,
    between(value_txcurr_ped, 40, 80) ~ 4,
    value_txcurr_ped < 40 ~ 5,
    TRUE ~ 0))

# Viral Load Coverage
score_clin_vlc <- df %>%  
  filter(indicator %in% c("TX_PVLS_D", "TX_CURR_6MO_PRIOR"),
         period == val_period) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(across(starts_with("TX_"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(value_vlc = TX_PVLS_D / TX_CURR_6MO_PRIOR) %>% 
  select(!c(TX_PVLS_D, TX_CURR_6MO_PRIOR)) %>% 
  mutate(score_vlc = case_when(
    value_vlc < .5 ~ 1,
    between(value_vlc, .5, .6) ~ 2,
    between(value_vlc, .6, .75) ~ 3,
    between(value_vlc, .75, .9) ~ 4,
    value_vlc > .9 ~ 5,
    TRUE ~ 0)) 

# Pediatric Viral Load Coverage
score_clin_vlc_ped <- df %>%  
  filter(indicator %in% c("TX_PVLS_D", "TX_CURR_6MO_PRIOR"),
         period == val_period,
         agecoarse == "<15") %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(across(starts_with("TX_"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(value_vlc_ped = TX_PVLS_D / TX_CURR_6MO_PRIOR) %>% 
  select(!c(TX_PVLS_D, TX_CURR_6MO_PRIOR)) %>% 
  mutate(score_vlc_ped = case_when(
    value_vlc_ped < .5 ~ 1,
    between(value_vlc_ped, .5, .6) ~ 2,
    between(value_vlc_ped, .6, .75) ~ 3,
    between(value_vlc_ped, .75, .9) ~ 4,
    value_vlc_ped > .9 ~ 5,
    TRUE ~ 0))

# Viral Load Suppression
score_clin_vls <- df %>% 
  filter(indicator %in% c("TX_PVLS", "TX_PVLS_D"),
         period == val_period) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(across(starts_with("TX_PVLS"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(value_vls = TX_PVLS / TX_PVLS_D) %>% 
  select(!c(TX_PVLS, TX_PVLS_D)) %>% 
  mutate(score_vls = case_when(
    value_vls < .7 ~ 1,
    between(value_vls, .7, .8) ~ 2,
    between(value_vls, .8, .9) ~ 3,
    between(value_vls, .9, .95) ~ 4,
    value_vls > .95 ~ 5,
    TRUE ~ 0))


# Pediatric Viral Load Suppression
score_clin_vls_ped <- df %>% 
  filter(indicator %in% c("TX_PVLS", "TX_PVLS_D"),
       period == val_period,
       agecoarse == "<15") %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(across(starts_with("TX_PVLS"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(value_vls_ped = TX_PVLS / TX_PVLS_D) %>% 
  select(!c(TX_PVLS, TX_PVLS_D)) %>% 
  mutate(score_vls_ped = case_when(
    value_vls_ped < .7 ~ 1,
    between(value_vls_ped, .7, .8) ~ 2,
    between(value_vls_ped, .8, .9) ~ 3,
    between(value_vls_ped, .9, .95) ~ 4,
    value_vls_ped > .95 ~ 5,
    TRUE ~ 0))


# PW Viral Load Suppression
score_clin_vls_pw <- df %>% 
  filter(indicator %in% c("TX_PVLS_PLW", "TX_PVLS_PLW_D"),
         period == val_period) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(across(starts_with("TX_PVLS"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(value_vls_pw = TX_PVLS_PLW / TX_PVLS_PLW_D) %>% 
  select(!c(TX_PVLS_PLW, TX_PVLS_PLW_D)) %>% 
  mutate(score_vls_pw = case_when(
    value_vls_pw < .7 ~ 1,
    between(value_vls_pw, .7, .8) ~ 2,
    between(value_vls_pw, .8, .9) ~ 3,
    between(value_vls_pw, .9, .95) ~ 4,
    value_vls_pw > .95 ~ 5,
    TRUE ~ 0))


# PMTCT_STAT_POS
score_clin_pmtct_pos <- df %>% 
  filter(indicator %in% c("PMTCT_STAT_POS"),
         between(date, max(date) - months(9), max(date))) %>%  
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(value_pmtct_pos = sum(PMTCT_STAT_POS, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(score_pmtct_pos = case_when(
    value_pmtct_pos > 400 ~ 1,
    between(value_pmtct_pos, 200, 400) ~ 2,
    between(value_pmtct_pos, 120, 200) ~ 3,
    between(value_pmtct_pos, 80, 120) ~ 4,
    value_pmtct_pos < 80 ~ 5,
    TRUE ~ 0))


# PMTCT_HEI_POS
score_clin_hei_pos <- df %>% 
  filter(indicator %in% c("PMTCT_EID_MOD", "PMTCT_HEI_POS"),
         between(date, max(date) - months(9), max(date))) %>% 
  group_by(datim_uid, indicator) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(PMTCT_HEI_POS = replace_na(PMTCT_HEI_POS, 0),
         value_hei_pos = PMTCT_HEI_POS / PMTCT_EID_MOD,
         score_hei_pos = case_when(
           value_hei_pos > .05 ~ 1,
           between(value_hei_pos, .04, .05) ~ 2,
           between(value_hei_pos, .03, .04) ~ 3,
           between(value_hei_pos, .02, .03) ~ 4,
           value_hei_pos < .02 ~ 5,
           TRUE ~ 0)) %>% 
  select(!c(PMTCT_EID_MOD, PMTCT_HEI_POS))
  


# PMTCT_EID <2m
score_clin_eid_u2m <- df %>% 
  filter(indicator %in% c("PMTCT_EID_Less_Equal_Two_Months", "PMTCT_EID_D"),
         between(date, max(date) - months(9), max(date))) %>% 
  group_by(datim_uid, indicator) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(value_eid_u2m = PMTCT_EID_Less_Equal_Two_Months / PMTCT_EID_D,
         score_eid_u2m = case_when(
           value_eid_u2m < .6 ~ 1,
           between(value_eid_u2m, .6, .7) ~ 2,
           between(value_eid_u2m, .7, .8) ~ 3,
           between(value_eid_u2m, .8, .9) ~ 4,
           value_eid_u2m > .9 ~ 5,
           TRUE ~ 0)) %>% 
  select(!c(PMTCT_EID_Less_Equal_Two_Months, PMTCT_EID_D))


# TXTB
score_clin_txtb <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_TB_D"), 
         if (str_detect(val_period, pattern = "Q2|Q4")) {
           date == max_date
         } else {
           date == max_date - months(3)
         }
  ) %>%
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(across(starts_with("TX_"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(value_txtb = TX_TB_D / TX_CURR) %>% 
  select(datim_uid, value_txtb) %>% 
  mutate(score_txtb = case_when(
    value_txtb < .65 ~ 1,
    between(value_txtb, .65, .75) ~ 2,
    between(value_txtb, .75, .85) ~ 3,
    between(value_txtb, .85, .95) ~ 4,
    value_txtb > .95 ~ 5,
    TRUE ~ 0))

# TX_ML
score_clin_txml <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_ML"),
         period == val_period) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(datim_uid) %>% 
  summarize(across(starts_with("TX_"), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(value_txml = TX_ML / TX_CURR) %>% 
  select(datim_uid, value_txml) %>% 
  mutate(score_txml = case_when(
    value_txml > .02 ~ 1,
    between(value_txml, .015, .02) ~ 2,
    between(value_txml, .01, .015) ~ 3,
    between(value_txml, .005, .01) ~ 4,
    value_txml < .005 ~ 5,
    TRUE ~ 0))


# TPT  # this probably needs correction to take a period that aligns with the other indicators
score_clin_tpt <- read_delim(path_tpt, 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE) %>% 
  rename(datim_uid = orgunituid,
         value_tpt = TPT_CUM_PER) %>% 
  drop_na(value_tpt) %>% 
  select(datim_uid, value_tpt) %>% 
  mutate(score_tpt = case_when(
    value_tpt < .5 ~ 1,
    between(value_tpt, .5, .64) ~ 2,
    between(value_tpt, .64, .74) ~ 3,
    between(value_tpt, .74, .9) ~ 4,
    value_tpt > .9 ~ 5,
    TRUE ~ 0))



# COMPILE -----------------------------------------------------------------

df_compile <- df_ajuda %>%
  filter(partner_pepfar_clinical != "MISAU") %>% 
  rename("partner" = partner_pepfar_clinical) %>% 
  left_join(score_clin_tpt) %>% # MOST RECENT PERIOD (FY22 Q2) CALCULATED USING TX_CURR FROM 2 QUARTERS AGO
  left_join(score_clin_txtb) %>% # MOST RECENT PERIOD (FY22 Q2)
  left_join(score_clin_txcurr) %>% # MOST RECENT PERIOD (FY22 Q2)
  left_join(score_clin_txcurr_ped) %>% # MOST RECENT PERIOD (FY22 Q2)
  left_join(score_clin_txml) %>% # MOST RECENT PERIOD (FY22 Q2). NOTE THIS IS TX_ML / TX_CURR
  left_join(score_clin_vlc) %>% # MOST RECENT PERIOD (FY22 Q2) CALCULATED USING TX_CURR FROM 2 QUARTERS AGO
  left_join(score_clin_vlc_ped) %>% # MOST RECENT PERIOD (FY22 Q2) CALCULATED USING TX_CURR FROM 2 QUARTERS AGO
  left_join(score_clin_vls) %>% # MOST RECENT PERIOD (FY22 Q2)
  left_join(score_clin_vls_ped) %>% # MOST RECENT PERIOD (FY22 Q2)
  left_join(score_clin_vls_pw) %>% # MOST RECENT PERIOD (FY22 Q2)
  left_join(score_clin_pmtct_pos) %>% # LAST 4 QUARTERS SUMMED
  left_join(score_clin_eid_u2m) %>% # MOST RECENT PERIOD NUMERATOR WITH PMTCT_EID_D FROM 2 QUARTERS AGO
  left_join(score_clin_hei_pos) %>% # MOST RECENT PERIOD (FY22 Q2)
  mutate(across(.col = starts_with("score"), .fns = ~ replace_na(.x, 0))) %>%  
  rowwise %>%
  mutate(score_clin = sum(c_across(starts_with("score")), na.rm = TRUE),
         score_clin_wo_volume = rowSums(across(contains(c("score_tpt", 
                                                          "score_txtb", 
                                                          "score_txml",
                                                          "score_vlc",
                                                          "score_vlc_u15",
                                                          "score_vls",
                                                          "score_vls_ped",
                                                          "score_vls_pw",
                                                          "score_clin_eid_u2m",
                                                          "score_clin_hei_pos")))),
         period = val_period) %>% 
  relocate(starts_with("score"), .after = last_col()) %>% 
  relocate(period, .after = datim_uid) %>% 
  arrange((score_clin)) %>% 
  select(!c(snu, psnu, sitename, partner)) %>%
  glimpse()


# WRITE TO DISK -----------------------------------------------------------

write.xlsx(df_compile, 
           path_output, 
           sheetName = glue::glue({val_period}),
           overwrite = TRUE,
           append = TRUE)


# COMPILE HISTORICAL DATASETS ---------------------------------------------


historic_files <- dir({path_monthly_output_repo}, pattern = "*.xlsx")

sims_prioritization_tidy_history <- historic_files %>%
  map(~ read_excel(file.path(path_monthly_output_repo, .))) %>%
  reduce(rbind) %>% 
  left_join(df_ajuda, by = "datim_uid") %>% 
  relocate(snu:partner_pepfar_clinical, .after = datim_uid) %>% 
  select(snu, psnu, sitename, datim_uid, period, starts_with("score_"), score_clin, score_clin_wo_volume)


write.xlsx(sims_prioritization_tidy_history, 
           "Dataout/site_scoring.xlsx", 
           sheetName = "sheet_name",
           overwrite = TRUE,
           append = TRUE)
