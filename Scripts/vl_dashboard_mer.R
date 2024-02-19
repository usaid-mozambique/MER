
# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(glamr)
library(gophr)
library(glue)
library(fs)
library(janitor)
library(readxl)
library(openxlsx)
library(filenamer)
library(googlesheets4)
library(googledrive)
library(mozR)
load_secrets() 


# LOAD DATA & VALUES ---------------------------------------------------------------


df_mer <- read_tsv("Dataout/results_cumulative_new.txt")

df_sup <- read_tsv("Dataout/mer_supplemental_vl.txt")

df_disa <- read_delim("~/GitHub/Enhanced_Monitoring/Dataout/em_disa.txt", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)

df_mi <- read_delim("~/GitHub/Enhanced_Monitoring/Dataout/em_mi.txt", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE) %>% 
  select(datim_uid:program_mi, starts_with("cv")) %>% 
  pivot_longer(starts_with("cv"), names_to = "indicator", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(period = ymd(period))


df_sitemap <- pull_sitemap()
df_psnuuid <- pull_sitemap(sheetname = "list_psnu")


date_min_mer <- as.Date("2022-12-20") # this trims the mer dataset down to only more recent periods
date_max_mer <- max(df_mer$date, na.rm = TRUE) # this is used as a reference to trim the disa dataset
date_min_disa <- date_min_mer - months(11) 
date_max_disa <- date_max_mer


# MUNGE MER ---------------------------------------------------------------


df_mer_subset <- df_mer %>% # from FY23Q1 forward
  filter(date >= date_min_mer,
         !partner_recode == "MISAU") %>% 
  select(datim_uid,
         partner = partner_recode,
         snu = snu1,
         psnu,
         sitename,
         period,
         date,
         sex,
         ageasentered,
         otherdisaggregate,
         TX_CURR_6MO_PRIOR,
         TX_PVLS,
         TX_PVLS_D,
         TX_PVLS_PLW,
         TX_PVLS_PLW_D) %>% 
  pivot_longer(starts_with("TX_"), names_to = "indicator", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(
    indicator = str_remove_all(indicator, "_PLW"),
    pop_type = case_when(str_detect(otherdisaggregate, "Breastfeeding")  ~ "LW",
                         str_detect(otherdisaggregate, "Pregnant")  ~ "PW",
                         TRUE ~ "Age/Sex"),
    ageasentered = case_when(ageasentered %in% c("65+", "60-64", "55-59", "50-54") ~ "50+",
                             TRUE ~ ageasentered),
    source = "MER") %>% 
  select(!otherdisaggregate) %>% 
  relocate(value, .after = everything())


date_list <- unique(df_mer_subset$date)


# MUNGE SUPPLMENTAL -------------------------------------------------------


df_sup_subset <- df_sup %>% 
  filter(!pop_type == "KeyPop") %>% 
  select(datim_uid,
         partner = partner_pepfar_clinical,
         snu,
         psnu,
         sitename,
         period,
         sex,
         ageasentered = age,
         pop_type = pop_subtype,
         source,
         indicator,
         value) %>% 
  separate(period, c("fy", "quarter"), " ", remove = FALSE) %>% 
  mutate(fy = as.numeric(fy),
         d = case_when(str_detect(quarter, "Q") ~ 20),
         m = case_when(str_detect(quarter, "Q1") ~ 12,
                       str_detect(quarter, "Q2") ~ 03,
                       str_detect(quarter, "Q3") ~ 06,
                       str_detect(quarter, "Q4") ~ 09),
         y = case_when(str_detect(quarter, "Q1") ~ fy -1,
                       str_detect(quarter, "Q2") ~ fy,
                       str_detect(quarter, "Q3") ~ fy,
                       str_detect(quarter, "Q4") ~ fy),
         date = make_date(y, m, d),
         indicator = case_when(indicator == "TX_PVLS_N" ~ "TX_PVLS",
                               TRUE ~ indicator),
         pop_type = replace_na(pop_type, "Age/Sex"),
         ageasentered = case_when(ageasentered == "Unknown" ~ "Unknown Age",
                                  ageasentered %in% c("65+", "60-64", "55-59", "50-54") ~ "50+",
                                  ageasentered == "All" ~ NA_character_,
                                  TRUE ~ ageasentered),
         sex = case_when(sex == "Unknown" ~ "Unknown Sex",
                         TRUE ~ sex),
         source = case_when(source == "Clinical Module" ~ "EPTS-Clinical",
                            source == "Lab Module" ~ "EPTS-Lab",
                            TRUE ~ source)) %>% 
  filter(date >= date_min_mer,
         ) %>% 
  select(-c(fy, quarter, d, m, y)) %>% 
  glimpse()


check <- df_sup_subset %>% 
  filter(period == "2023 Q3")
  
unique(df_sup_subset$ageasentered)
unique(df_sup$age)

# MUNGE DISA ----------------------------------------------------------


df_disa_subset <- df_disa %>% 
  select(datim_uid, partner, snu, psnu, sitename, date = period, ageasentered = age, pop_type = group, sex, TX_PVLS_D = VL, TX_PVLS = VLS) %>% 
  pivot_longer(starts_with("TX_"), names_to = "indicator", values_to = "value") %>% 
  filter(date >= date_min_disa & date <= date_max_disa,
         value > 0) %>% 
  mutate(source = "DISA",
         pop_type = case_match(pop_type,
                               "Age" ~ "Age/Sex",
                               .default = pop_type),
         month = strftime(date, "%m"),
         year = strftime(date, "%Y"),
         quarter = case_match(month,
                              c("10", "11", "12") ~ "Q1",
                              c("01", "02", "03") ~ "Q2",
                              c("04", "05", "06") ~ "Q3",
                              c("07", "08", "09") ~ "Q4",
                              .default = month),
         year = if_else(quarter == "Q1",
                        as.numeric(year) + 1,
                        as.numeric(year)),
         period = str_c(as.character(year), quarter, sep = " "),
         ageasentered = case_when(pop_type %in% c("PW", "LW") ~ NA_character_,
                                  TRUE ~ ageasentered),
         sex = case_when(pop_type %in% c("PW", "LW") ~ NA_character_,
                         TRUE ~ sex)) %>% 
  select(!c(month, year, quarter)) %>%  
  inner_join(df_sitemap %>% select(datim_uid), by = "datim_uid") %>% 
  glimpse()


df_disa_subset_fy23q1 <- df_disa_subset %>% 
  filter(date >= as.Date("2022-12-20") - months(11)) %>% 
  filter(date <= as.Date("2022-12-20")) %>% 
  mutate(period = max(period),
         date = max(date))

df_disa_subset_fy23q2 <- df_disa_subset %>% 
  filter(date >= as.Date("2023-03-20") - months(11)) %>% 
  filter(date <= as.Date("2023-03-20")) %>% 
  mutate(period = max(period),
         date = max(date))

df_disa_subset_fy23q3 <- df_disa_subset %>% 
  filter(date >= as.Date("2023-06-20") - months(11)) %>% 
  filter(date <= as.Date("2023-06-20")) %>% 
  mutate(period = max(period),
         date = max(date))

df_disa_subset_fy23q4 <- df_disa_subset %>% 
  filter(date >= as.Date("2023-09-20") - months(11)) %>% 
  filter(date <= as.Date("2023-09-20")) %>% 
  mutate(period = max(period),
         date = max(date))



df_disa_subset_fy23q1 %>% 
  distinct(period, date)
df_disa_subset_fy23q2 %>% 
  distinct(period, date)
df_disa_subset_fy23q3 %>% 
  distinct(period, date)
df_disa_subset_fy23q4 %>% 
  distinct(period, date)


# MUNGE MI ----------------------------------------------------------------


df_mi_subset <- df_mi %>% 
  filter(period >= date_min_mer) %>% 
  select(!c(sisma_uid, sisma_uid_datim_map, site_nid, numdenom, program_ap3, program_mi)) %>% 
  rename(ageasentered = age) %>% 
  mutate(source = "MI",
         sex = NA_character_) %>% 
  left_join(df_sitemap %>% select(datim_uid, sisma_uid, site_nid, his_emr, his_epts, his_idart, his_disa), by = "datim_uid") %>% 
  left_join(df_psnuuid %>% select(psnu, psnuuid), by = "psnu") %>% 
  
  glimpse()
  


# COMPILE AND CLEAN -------------------------------------------------------


df_combine <- bind_rows(df_mer_subset, df_sup_subset, df_disa_subset_fy23q1, df_disa_subset_fy23q2, df_disa_subset_fy23q3, df_disa_subset_fy23q4) %>% 
  select(!c(partner, snu, psnu, sitename)) %>% 
  left_join(df_sitemap, by = "datim_uid") %>% 
  left_join(df_psnuuid %>% select(psnu, psnuuid), by = "psnu") %>% 
  relocate(psnuuid, .after = psnu) %>% 
  glimpse()

check <- df_combine %>% 
  filter(period == "2023 Q4") %>% 
  filter(str_detect(source, "EPTS"))
  
check %>% distinct(source, ageasentered) %>% print(n=100)
  


unique(df_combine$source)
unique(df_combine$pop_type)
unique(df_combine$sex)
unique(df_combine$ageasentered)


write_tsv(
  df_combine,
  "Dataout/vl_dashboard_crosswalk.txt",
  na = "")

write_tsv(
  df_mi_subset,
  "Dataout/vl_dashboard_mi.txt",
  na = "")

