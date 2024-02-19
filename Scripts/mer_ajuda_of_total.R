
# PURPOSE:  Munge and Analysis to Determine Running Total % AJUDA vs. Sustainability
# AUTHOR:  Joe Lara | USAID
# DATE: 2021-10-12
# NOTES: 

# LOCALS & SETUP ------------------------


library(tidyverse)
library(glamr)
library(readxl)
library(janitor)
library(glue)
library(here)
library(ggthemes)
library(gophr)
library(openxlsx)
library(googlesheets4)
library(googledrive)
load_secrets()


# PATHS & VALUES -------------------------------------------------------------------


period <- "FY23Q4"
remove_dup <- c("ChVIuS0HRZ8", "KzmQ90ne3V1", "ftbjGljVY2B") # remove reporting duplicates from mer dataset (lindsay indicated these sites were duplicated)
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")
path_dedup_map <- as_sheets_id("14asAzxiRf73ek2D5ImQ0Beu6oL635q7nbc2FgWPBxkg")
path_output <- "Dataout/ajuda_of_national/ajuda_of_national.xlsx"
file_date <- base::format(as.Date(Sys.Date()), 
                          "_%Y_%m_%d")

indicators <- c("HTS_TST_POS", "PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_EID", "PMTCT_HEI_POS", "TX_CURR")

# LOAD DATA ----------------------------


dg <- read_rds("Data/Genie/dg_ref.rds")

dedup_map <- read_sheet(path_dedup_map) %>%
  select(indicator, standardizeddisaggregate, indicator_new)

ajuda_site_map <- read_sheet(path_ajuda_site_map, sheet = "map_partner_alt") %>%  select(datim_uid, partner_alt)


# MUNGE ---------------------------------


dg_1 <- dg %>% 
  filter(indicator %in% indicators,
         !orgunituid %in% remove_dup) %>% 
  left_join(dedup_map) %>% 
  drop_na(indicator_new) %>% 
  select(-c(indicator)) %>% 
  rename(indicator = indicator_new) %>% 
  reshape_msd("long") %>% 
  filter(period_type == "results",
         period == "FY22Q4",
         sitetype %in% c("Facility", "Military")) %>% # may need to remove military for analysis
  mutate(indicator = paste0(indicator,
                                 if_else(numeratordenom %in% c("D"), "_D", "")),
         mech_name = case_when(orgunituid %in% c("FThJa71HEEs", "iJax0w1KHEN", "OgLIyzin181") ~ "ICAP",  # recoding to account for COP22 sites added
                               orgunituid == "FNUYBvpbWaA" ~ "FGH",
                               orgunituid == "EslaBcDD5mZ" ~ "ECHO",
                               TRUE ~ mech_name),
         site_type = case_when(mech_name %in% c("JHPIEGO-DoD", "CCS", "ECHO", "FGH", "EGPAF", "ICAP", "ARIEL") ~ "99-AJUDA",
                               mech_name == "MISAU" ~ "01-Sustainability",
                               TRUE ~ "non-clinical")) %>% 
  filter(!site_type == "non-clinical") %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = indicator,
                     values_from = value) %>% 
  left_join(ajuda_site_map, by = c("orgunituid" = "datim_uid")) %>% 
  select(datim_uid = orgunituid, 
         snu1, 
         psnu, 
         sitename, 
         site_type, 
         mech_name,
         partner_alt,
         HTS_TST_POS,
         TX_CURR,
         PMTCT_STAT,
         PMTCT_STAT_POS,
         PMTCT_HEI_POS,
         PMTCT_EID_D,
         -row) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% 
  group_by(datim_uid, snu1, psnu, sitename, site_type, mech_name, partner_alt) %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>%
  arrange_at(vars(TX_CURR), desc) %>% 
  mutate(TX_CURR_cum = cumsum(TX_CURR),
         TX_CURR_cum_per = TX_CURR_cum / sum(TX_CURR),
         PMTCT_HEI_POS_per = PMTCT_HEI_POS / PMTCT_EID_D,
         PMTCT_HEI_POS_per = replace_na(PMTCT_HEI_POS_per, 0)) %>% 
  relocate(contains("_cum"), .after = TX_CURR) %>% 
  glimpse()


dg_2 <- dg_1 %>% 
  filter(TX_CURR > 0) %>% 
  mutate(TX_CURR_SUS = case_when(site_type == "01-Sustainability" ~ TX_CURR, TRUE ~ 0),
         TX_CURR_AJUDA = case_when(site_type ==  "99-AJUDA" ~ TX_CURR, TRUE ~ 0),
         SITE_SUS = case_when(site_type == "01-Sustainability" ~ 1, TRUE ~ 0),
         SITE_AJUDA = case_when(site_type == "99-AJUDA" ~ 1, TRUE ~ 0),
         SITE_TOTAL = SITE_SUS + SITE_AJUDA) %>% 
  group_by(snu1, .drop = TRUE) %>% 
  summarize(across(c(TX_CURR, TX_CURR_SUS, TX_CURR_AJUDA, SITE_TOTAL, SITE_SUS, SITE_AJUDA), sum, na.rm = TRUE)) %>% 
  mutate(TX_CURR_AJUDA_PER = TX_CURR_AJUDA / TX_CURR,
         SITE_AJUDA_PER = SITE_AJUDA / SITE_TOTAL) %>% 
  view()

summary %>% count(mech_name, sort = TRUE)




# SAVE TO DISK ---------------------


wb = createWorkbook()
sht = addWorksheet(wb, "Data")
pct = createStyle(numFmt = "0.0%")
num <- createStyle(numFmt = "#,##0")

writeData(wb, sht, dg_1)
addStyle(wb, sht, style = pct, cols = c(11, 16), rows = 2:(nrow(dg_1)+1), gridExpand=TRUE)
addStyle(wb, sht, style = num, cols = c(8, 9, 10, 12, 13, 14, 15), rows = 2:(nrow(dg_1)+1), gridExpand=TRUE)
saveWorkbook(wb, 
             glue::glue("Dataout/ajuda_of_national/ajuda_of_national{file_date}.xlsx"),
             overwrite = TRUE)


wb_2 = createWorkbook()
sht_2 = addWorksheet(wb_2, "Data")
pct = createStyle(numFmt = "0.0%")
num <- createStyle(numFmt = "#,##0")

writeData(wb_2, sht_2, dg_2)
addStyle(wb_2, sht_2, style = num, cols = c(1:7), rows = 2:(nrow(dg_1)+1), gridExpand=TRUE)
addStyle(wb_2, sht_2, style = pct, cols = c(8, 9), rows = 2:(nrow(dg_1)+1), gridExpand=TRUE)
saveWorkbook(wb_2, 
             glue::glue("Dataout/ajuda_of_national/ajuda_of_national_snu{file_date}.xlsx"),
             overwrite = TRUE)



