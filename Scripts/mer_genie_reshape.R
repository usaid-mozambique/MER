

# DEPENDENCIES ------------------------------------------------------------


library(tidyverse)
library(mozR)
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
library(zoo)
load_secrets() 


# SET PATHS ---------------------------------------------------------------


path_dg <- "Data/Genie/dg_ref.rds"
path_dg_psnu <- "Data/Genie/dg_ref_psnu.rds"

path_dedup_map <- as_sheets_id("14asAzxiRf73ek2D5ImQ0Beu6oL635q7nbc2FgWPBxkg")
path_ajuda_site_map <- as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")
path_triangulation_output <- "~/GitHub/Supply_Chain/Data/tx_curr_"

path_pmtct_art_12mo_quarterly <- "Dataout/pmtct_art_12mo_cohort/"


# SET LISTS ------------------------------------------------------------
# Used for creating recoded partner name for clinical indicators

clinical_indicators <- c("TX_CURR", 
                         "TX_ML", 
                         "TX_NET_NEW", 
                         "TX_RTT", 
                         "TX_PVLS", 
                         "TX_NEW", 
                         "TX_TB", 
                         "TB_PREV", 
                         "TB_ART", 
                         "TB_STAT", 
                         "PMTCT_STAT", 
                         "PMTCT_STAT_POS", 
                         "PMTCT_ART", 
                         "PMTCT_EID", 
                         "PMTCT_HEI_POS")


# SITE SCRIPT CHAPTER -----------------------------------------------------
# 1.1 LOAD DATASETS, SELECT DATAGENIE COLUMNS, RENAME MECHS ---------------------------------------------


dg <- read_rds(path_dg)

dedup_map <- read_sheet(path_dedup_map) %>%
  select(indicator, standardizeddisaggregate, indicator_new)

ajuda_site_map <- pull_sitemap() %>% 
  select(
    datim_uid,
    ends_with("tude"),
    starts_with("partner_"),
    grm_sernap,
    starts_with("his_"),
    starts_with("program_"),
    security_insecure,
    lab_mpima_mch)


ajuda_meta_sub <- ajuda_site_map %>% 
  select(datim_uid,
         partner_pepfar_clinical)


# 1.2 DE-DUPLICATE MER, CREATE NEW MECH_NAME & PIVOT LONG ----------------------------------------


dg_1 <- dg %>% 
  left_join(dedup_map) %>% 
  drop_na(indicator_new) %>% 
  select(-c(indicator)) %>% 
  rename(indicator = indicator_new) %>% 
  left_join(ajuda_meta_sub, by = c("orgunituid" = "datim_uid")) %>% # create partner variable
  mutate(partner_recode = case_when(
    indicator %in% clinical_indicators & !is.na(partner_pepfar_clinical) ~ partner_pepfar_clinical,
    indicator %in% clinical_indicators & is.na(partner_pepfar_clinical) ~ "MISAU",
    TRUE ~ mech_name)) %>% 
  mutate(fy = as.numeric(fiscal_year)) %>% # reshape to long, generate quarter, & add denominator suffix to indicators
  reshape_msd(direction = c("long")) %>% 
  mutate(period = case_when(str_detect(period, "Q1") ~ "Q1",
                             str_detect(period, "Q2") ~ "Q2",
                             str_detect(period, "Q3") ~ "Q3",
                             str_detect(period, "Q4") ~ "Q4"),
         indicator_temp = paste0(indicator,
                                 if_else(numeratordenom %in% c("D"), "_D", "")))


# fix for FY24 Q1 and Q2 data errors in genie
dg_1 <- dg_1 |> 
  mutate(exclusion_filter = case_when(mech_name == "MISAU" & fy == 2024 & (!partner_recode %in% c("MISAU", "Ministry of Health Mozambique")) ~ "Exclude",
                            TRUE ~ "")) |> 
  filter(exclusion_filter != "Exclude") |> 
  select(!exclusion_filter)

# Exclude Catandica CS and Vilanculous CS from MISAU
dg_1 <- dg_1 |> 
  mutate(exclusion_filter = case_when(mech_name == "MISAU" & fy == 2024 & period == "Q2" & (indicator %in% clinical_indicators) & (orgunituid %in% c("ub0cVLa8j1i", "jjWur7wj2v8")) ~ "Exclude",
                                      TRUE ~ "")) |> 
  filter(exclusion_filter != "Exclude") |> 
  select(!exclusion_filter)
  
# exclude Tete_CP from MISAU
dg_1 <- dg_1 |> 
  mutate(exclusion_filter = case_when(mech_name == "MISAU" & fy == 2024 & period == "Q2" & (orgunituid %in% c("WAVIEeXwnV6")) ~ "Exclude",
                                      TRUE ~ "")) |> 
  filter(exclusion_filter != "Exclude") |> 
  select(!exclusion_filter)
  

# 1.3 RECODE DATE FOR COHORT INDICATORS & UNION TO MAIN DATAFRAME ----------------------------


cohorts_3mo_prior <- dg_1 %>% 
  filter(indicator_temp %in% c("TX_CURR", "TX_ML")) %>% 
  mutate(
    fy = case_when(period == "Q1" ~ fy,
                   period == "Q2" ~ fy,
                   period == "Q3" ~ fy,
                   period == "Q4" ~ fy +1),
    period = recode(period,
                    "Q1" = "Q2",
                    "Q2" = "Q3",
                    "Q3" = "Q4",
                    "Q4" = "Q1"),
    indicator_temp = recode(indicator_temp,
                     "TX_CURR" = "TX_CURR_3MO_PRIOR",
                     "TX_ML" = "TX_ML_3MO_PRIOR"))


cohorts_6mo_prior <- dg_1 %>% 
  filter(indicator_temp %in% c("PMTCT_EID_D", "TX_CURR", "TX_CURR_KP")) %>% 
  mutate(
    fy = case_when(period == "Q1" ~ fy,
                   period == "Q2" ~ fy,
                   period == "Q3" ~ fy +1,
                   period == "Q4" ~ fy +1),
    period = recode(period,
                     "Q1" = "Q3",
                     "Q2" = "Q4",
                     "Q3" = "Q1",
                     "Q4" = "Q2"),
    indicator_temp = recode(indicator_temp,
                            "PMTCT_EID_D" = "PMTCT_EID_D_6MO_PRIOR",
                            "TX_CURR" = "TX_CURR_6MO_PRIOR",
                            "TX_CURR_KP" = "TX_CURR_KP_6MO_PRIOR"))


cohorts_6mo_after <- dg_1 %>% 
  filter(indicator_temp %in% c("TB_PREV", "TB_PREV_D")) %>% 
  mutate(
    fy = case_when(period == "Q1" ~ fy -1,
                   period == "Q2" ~ fy -1,
                   period == "Q3" ~ fy,
                   period == "Q4" ~ fy),
    period = recode(period,
                     "Q1" = "Q3",
                     "Q2" = "Q4",
                     "Q3" = "Q1",
                     "Q4" = "Q2"),
    indicator_temp = recode(indicator_temp,
                            "TB_PREV" = "TB_PREV_6MO_AFTER",
                            "TB_PREV_D" = "TB_PREV_D_6MO_AFTER"))


# 1.4 BIND DF, CREATE DATE VARIABLE -------------------------------------------------


dg_2 <- bind_rows(dg_1, cohorts_3mo_prior, cohorts_6mo_prior, cohorts_6mo_after) %>% 
  mutate(period = str_c(fy, period, sep = " "),
         d = case_when(str_detect(period, "Q") ~ 20),
         m = case_when(str_detect(period, "Q1") ~ 12,
                       str_detect(period, "Q2") ~ 03,
                       str_detect(period, "Q3") ~ 06,
                       str_detect(period, "Q4") ~ 09),
         y = case_when(str_detect(period, "Q1") ~ fy -1,
                       str_detect(period, "Q2") ~ fy,
                       str_detect(period, "Q3") ~ fy,
                       str_detect(period, "Q4") ~ fy),
         date = make_date(y, m, d)) %>%  
  select(-c(indicator, 
            d, m, y,
            partner_pepfar_clinical))


# 1.5 CREATE MIN/MAX PERIOD TO FILTER DATAFRAME -------------------------------



period_limiter <- dg_2 %>% 
  filter(indicator_temp == "TX_CURR",
         period_type == "results")

min_period <- min(period_limiter$date)
max_period <- max(period_limiter$date)
max_period_quarter <- max(period_limiter$period)



# 1.6 CREATE PMTCT_ART 12MO COHORT ----------------------------------------

# MOST RECENT QUARTER
cohorts_12mo_combine <- dg_2 %>%
  filter(indicator_temp %in% c("PMTCT_ART"),
         date > max_period - months(12)) %>%
  mutate(date = max_period,
         period = max_period_quarter,
         indicator_temp = "PMTCT_ART_12MO")

# MOST RECENT QUARTER -1Q
cohorts_12mo_combine_m1 <- dg_2 %>%
  filter(indicator_temp %in% c("PMTCT_ART"),
         between(date, as.Date(max_period - months(14)), as.Date(max_period - months(3))))

max_period_m1 <- max(cohorts_12mo_combine_m1$date)
max_period_quarter_m1 <- max(cohorts_12mo_combine_m1$period) 

cohorts_12mo_combine_m1 <- cohorts_12mo_combine_m1 %>% 
  mutate(date = max_period_m1,
         period = max_period_quarter_m1,
         indicator_temp = "PMTCT_ART_12MO")

# MOST RECENT QUARTER -2Q
cohorts_12mo_combine_m2 <- dg_2 %>%
  filter(indicator_temp %in% c("PMTCT_ART"),
         between(date, as.Date(max_period - months(17)), as.Date(max_period - months(6))))

max_period_m2 <- max(cohorts_12mo_combine_m2$date)
max_period_quarter_m2 <- max(cohorts_12mo_combine_m2$period) 

cohorts_12mo_combine_m2 <- cohorts_12mo_combine_m2 %>% 
  mutate(date = max_period_m2,
         period = max_period_quarter_m2,
         indicator_temp = "PMTCT_ART_12MO")

# MOST RECENT QUARTER -3Q
cohorts_12mo_combine_m3 <- dg_2 %>%
  filter(indicator_temp %in% c("PMTCT_ART"),
         between(date, as.Date(max_period - months(20)), as.Date(max_period - months(9))))

max_period_m3 <- max(cohorts_12mo_combine_m3$date)
max_period_quarter_m3 <- max(cohorts_12mo_combine_m3$period) 

cohorts_12mo_combine_m3 <- cohorts_12mo_combine_m3 %>% 
  mutate(date = max_period_m3,
         period = max_period_quarter_m3,
         indicator_temp = "PMTCT_ART_12MO")

# MOST RECENT QUARTER -4Q
cohorts_12mo_combine_m4 <- dg_2 %>%
  filter(indicator_temp %in% c("PMTCT_ART"),
         between(date, as.Date(max_period - months(23)), as.Date(max_period - months(12))))

max_period_m4 <- max(cohorts_12mo_combine_m4$date)
max_period_quarter_m4 <- max(cohorts_12mo_combine_m4$period) 

cohorts_12mo_combine_m4 <- cohorts_12mo_combine_m4 %>% 
  mutate(date = max_period_m4,
         period = max_period_quarter_m4,
         indicator_temp = "PMTCT_ART_12MO")


unique(cohorts_12mo_combine$period)
unique(cohorts_12mo_combine_m1$period)
unique(cohorts_12mo_combine_m2$period)
unique(cohorts_12mo_combine_m3$period)
unique(cohorts_12mo_combine_m4$period)



dg_3 <- bind_rows(dg_2, cohorts_12mo_combine, cohorts_12mo_combine_m1, cohorts_12mo_combine_m2, cohorts_12mo_combine_m3, cohorts_12mo_combine_m4)



# CALCULATE ROLLING PMTCT_ART COHORT --------------------------------------
# 
# cohorts_12mo_combine <- dg_2


# cohorts_12mo_combine <- dg_2 %>%
#   filter(indicator_temp %in% c("PMTCT_ART"),
#          date <= max_period, 
#          orgunituid == "ZKVzRmYkKzQ",
#          ageasentered == "25-29", 
#          otherdisaggregate == "Life-long ART, Already") %>% 
#   arrange(date)
# 
# cohorts_12mo_combine <- dg_2 %>%
#   filter(indicator_temp %in% c("PMTCT_ART"),
#          date <= max_period) %>% 
#   arrange(orgunituid, ageasentered, otherdisaggregate, date)
# 
# test <- cohorts_12mo_combine %>% 
#   mutate(rollsum_r = rollsum(value, 4, fill = NA, align = "right"))
# 
# 
# test %>% 
#   count(date,
#         wt = rollsum_r)
# 
# cohorts_12mo_combine %>% 
#   count(date,
#         wt = value)
# 
# cohorts_12mo_combine %>% arrange(date) %>% distinct(date) %>% print(n=100)
# dg_2 %>% arrange(date) %>% distinct(date) %>% print(n=100)

# zoo package roll_sum

# 1.6 PIVOT WIDER ---------------------------------------------------------


dg_4 <- dg_3 %>% 
  mutate(row_n = row_number()) %>% 
  pivot_wider(names_from = indicator_temp, values_from = value) %>% 
  rename(fiscal_year = fy,
         agecoarse = trendscoarse) %>% 
  select(-c(row_n,
            # standardizeddisaggregate,
            numeratordenom)) %>% 
  filter(date >= min_period & date <= max_period | is.na(period))


rm(ajuda_meta_sub, period_limiter, dg, dg_1, dg_2, cohorts_3mo_prior, cohorts_6mo_after, cohorts_6mo_prior)
gc()


# 1.6 CREATE SITE VOLUME VARIABLE ---------------------------------------------


site_volume <- dg_4 %>% 
  select(date, orgunituid, sitetype, TX_CURR) %>% 
  filter(sitetype == "Facility",
         date == max_period) %>% 
  group_by(orgunituid) %>% 
  summarize(TX_CURR = sum(TX_CURR, na.rm = T)) %>% 
  mutate(site_volume = case_when(
    TX_CURR < 1000 ~ "Low",
    between(TX_CURR, 1000, 5000) ~ "Medium",
    TX_CURR > 5000 ~ "High",
    TRUE ~ "Not Reported")) %>% 
  select(orgunituid, site_volume)

site_volume %>% 
  count(site_volume,
        sort = TRUE)


# 1.7 CALCULATE COLUMN INDICATORS ---------------------------------------------


dg_5 <- dg_4 %>% 
  mutate(otherdisaggregate = recode(otherdisaggregate, 
                               "No Contact Outcome - Interruption in Treatment <3 Months Treatment" =  "IIT 0-3m ART",
                               "No Contact Outcome - Interruption in Treatment 3-5 Months Treatment" = "IIT 3-5m ART",
                               "No Contact Outcome - Interruption In Treatment 6+ Months Treatment" =  "IIT 6+m ART",
                               "No Contact Outcome - Interruption in Treatment 3+ Months Treatment" =  "IIT 3+m ART",
                               "No Contact Outcome - Transferred Out" = "Transferred Out",
                               "No Contact Outcome - Died" = "Died",
                               "No Contact Outcome - Refused Stopped Treatment" = "Refused/Stopped ART"),
         TX_MMD_D = TX_MMD,
         TX_MMD = case_when((otherdisaggregate %in% c("ARV Dispensing Quantity - 3 to 5 months", "ARV Dispensing Quantity - 6 or more months")) ~ TX_MMD),
         TX_CURR_FemaleAdult = case_when((sex == "Female" & agecoarse == "15+") ~ TX_CURR),
         TX_ML_IIT = case_when((otherdisaggregate %in% c("IIT 0-3m ART",  
                                                         "IIT 3-5m ART", 
                                                         "IIT 6+m ART", 
                                                         "IIT 3+m ART")) ~ TX_ML),
         TX_ML_IIT_3_Months_More = case_when((otherdisaggregate %in% c("IIT 3-5m ART", 
                                                                       "IIT 6+m ART", 
                                                                       "IIT 3+m ART")) ~ TX_ML), 
         TX_ML_IIT_3_Months_Less = case_when((otherdisaggregate == "IIT 0-3m ART") ~ TX_ML),
         TX_PatientLoss = case_when((!otherdisaggregate == "Transferred Out") ~ TX_ML),
         PMTCT_EID_MOD = case_when((otherdisaggregate == "EID First Test" | is.na(otherdisaggregate)) ~ PMTCT_EID), # new indicator
         PMTCT_EID_MOD_2_12MO = case_when((ageasentered == "02 - 12 Months") ~ PMTCT_EID_MOD), # new indicator
         
         HTS_TST_PostANC = case_when((fiscal_year < 2024 & modality == "Post ANC1" | fiscal_year > 2023 & modality %in% c("PMTCT Post ANC1 Breastfeeding", "PMTCT Post ANC1 Pregnant/L&D")) ~ HTS_TST), # new indicator
         HTS_TST_POS_PostANC = case_when((fiscal_year < 2024 & modality == "Post ANC1" | fiscal_year > 2023 & modality %in% c("PMTCT Post ANC1 Breastfeeding", "PMTCT Post ANC1 Pregnant/L&D")) ~ HTS_TST_POS), # new indicator
         PMTCT_STAT_NEG = case_when((statushiv == "Negative") ~ PMTCT_STAT), # new indicator
         
         PMTCT_ART_Already = case_when((otherdisaggregate == "Life-long ART, Already") ~ PMTCT_ART),
         HTS_TST_Female = case_when((sex == "Female") ~ HTS_TST),
         HTS_TST_Male = case_when((sex == "Male") ~ HTS_TST),
         HTS_TST_POS_Female = case_when((sex == "Female") ~ HTS_TST_POS),
         HTS_TST_POS_Male = case_when((sex == "Male") ~ HTS_TST_POS),
         HTS_TST_Index = case_when((modality %in% c("Index Facility", "Index Community")) ~ HTS_TST),
         HTS_TST_POS_Index = case_when((modality %in% c("Index Facility", "Index Community")) ~ HTS_TST_POS),
         OVC_SERV_Active = case_when((otherdisaggregate == "Active") ~ OVC_SERV),
         OVC_SERV_Graduated = case_when((otherdisaggregate == "Graduated") ~ OVC_SERV),
         TB_STAT_POS = case_when((statushiv == "Positive") ~ TB_STAT),
         TB_PREV_6MO_AFTER_Already = case_when((otherdisaggregate %in% c("IPT, Life-long ART, Already", "Life-long ART, Already")) ~ TB_PREV_6MO_AFTER),
         TB_PREV_6MO_AFTER_New = case_when((otherdisaggregate %in% c("IPT, Life-long ART, New", "Life-long ART, New")) ~ TB_PREV_6MO_AFTER),
         TB_PREV_D_6MO_AFTER_Already = case_when((otherdisaggregate %in% c("IPT, Life-long ART, Already", "Life-long ART, Already")) ~ TB_PREV_D_6MO_AFTER),
         TB_PREV_D_6MO_AFTER_New = case_when((otherdisaggregate %in% c("IPT, Life-long ART, New", "Life-long ART, New")) ~ TB_PREV_D_6MO_AFTER),
         TB_PREV_D_Already = case_when((otherdisaggregate %in% c("IPT, Life-long ART, Already", "Life-long ART, Already")) ~ TB_PREV_D),
         TB_PREV_D_New = case_when((otherdisaggregate %in% c("IPT, Life-long ART, New", "Life-long ART, New")) ~ TB_PREV_D),
         TX_TB_POS = case_when((statustb == "Positive") ~ TX_TB_D),
         TX_TB_NEG = case_when((statustb == "Negative") ~ TX_TB_D),
         TX_TB_NEG_Already = case_when((statustb == "Negative" & otherdisaggregate == "TB Screen - Negative, Life-long ART, Already") ~ TX_TB_D),
         TX_TB_NEG_New = case_when((statustb == "Negative" & otherdisaggregate == "TB Screen - Negative, Life-long ART, New") ~ TX_TB_D),
         TX_CIRA = if_else(!is.na(TX_RTT), TX_RTT,
                           if_else(!is.na(TX_ML_IIT), TX_ML_IIT, NA_integer_)),
         TX_GainLoss_Unexpected = if_else(!is.na(TX_NET_NEW), TX_NET_NEW,
                                          if_else(!is.na(TX_NEW), -TX_NEW, NA_integer_)),
         TX_CURR_Expected = if_else(!is.na(TX_CURR), TX_CURR,
                                    if_else(!is.na(TX_NET_NEW), -TX_NET_NEW, 
                                            if_else(!is.na(TX_NEW), TX_NEW, NA_integer_))))


rm(dg_4)
gc()

# test <- dg_5 %>%
#   filter(!is.na(HTS_TST_POS_PostANC),
#          period_type == 'results') %>%
#   select(orgunituid:psnu, standardizeddisaggregate, ageasentered, modality, otherdisaggregate, period, partner_recode, fiscal_year, HTS_TST_POS_PostANC)
# 
# test %>%
#   ggplot() +
#   geom_col(aes(x = period, HTS_TST_POS_PostANC))

# 1.8 JOIN AJUDA METADATA, RELOCATE & REMOVE UNNEEDED VARIABLES, FILTER PERIOD_TYPE ---------------


dg_6 <- dg_5 %>%
  filter(period_type %in% c("results", "cumulative")) %>% 
  select(-c(statushiv, statustb, statuscx, hiv_treatment_status)) %>%
  rename(datim_uid = orgunituid) %>%
  left_join(ajuda_site_map, by = "datim_uid") %>% 
  left_join(site_volume, by = c("datim_uid" = "orgunituid")) %>% 
  mutate(across(starts_with("his_"), ~replace_na(.x, 0)),
         across(starts_with("grm_"), ~replace_na(.x, 0)),
         across(starts_with("security_"), ~replace_na(.x, 0)),
         across(starts_with("program_"), ~replace_na(.x, 0)))


dg_7 <- dg_6 %>% 
  select(
    contains("_uid"),
    funding_agency,
    mech_name,
    starts_with("partner_"),
    grm_sernap,
    starts_with("snu"),
    starts_with("psnu"),
    dreams,
    sitename,
    sitetype,
    ends_with("tude"),
    period_type,
    fiscal_year,
    period,
    date,
    sex,
    ageasentered,
    agecoarse,
    modality,
    standardizeddisaggregate,
    otherdisaggregate,
    # disaggregate,
    categoryoptioncomboname,
    site_volume,
    starts_with("his_"),
    starts_with("program_"),
    starts_with("security_"),
    lab_mpima_mch,
    everything()
    ) %>% 
  glimpse()


# 1.9.1 CREATE TB_MER COVID DATAFRAME -------------------------------------------


dg_tb_covid <- dg_7 %>% 
  filter(period_type == "results") %>% 
  select(snu1, date, partner = partner_pepfar_clinical, period, TB_STAT, TB_STAT_POS) %>% 
  group_by(snu1, date, partner, period, .drop = TRUE) %>% 
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))


# 1.9.2 CREATE ARV CONSUMPTION TRIANGULATION DATAFRAME -----------------------------------------


dg_triagulation <- dg_7 %>% 
  filter(date == max_period & !is.na(date),
         !is.na(TX_CURR)) %>% 
  select(period, datim_uid, snu1, psnu, sitename, TX_CURR) %>% 
  group_by(period, datim_uid, snu1, psnu, sitename, .drop = TRUE) %>% 
  summarize(TX_CURR = sum(TX_CURR, na.rm = T)) %>% 
  rename_with(tolower, everything()) %>% 
  select(c(period, datim_uid, snu1, psnu, sitename, tx_curr))

max_period_tri <- unique(dg_triagulation$period) # used to name file when writing to disk


# 1.9.3 CREATE WATERFALL DATAFRAME ----------------------------------------------


df_waterfall <- dg_7 %>%
  filter(date >= "2022-06-20" & !is.na(date),
         !ageasentered %in% c("<15", "15+")) %>%
  select(datim_uid,
         period,
         funding_agency,
         partner_recode,
         snu1,
         psnu,
         sitename,
         sex,
         ageasentered,
         agecoarse,
         otherdisaggregate,
         TX_NEW,
         TX_RTT,
         TX_ML,
         TX_NET_NEW) %>%
  mutate(ageasentered = case_when((period %in% c("2022 Q3", "2022 Q4", "2023 Q1") & ageasentered %in% c("50-54", "55-59", "60-64", "65+")) ~ "50+",
                                  TRUE ~ ageasentered),
         TX_ML_Died = case_when(otherdisaggregate == "Died" ~ TX_ML), # here i believe we have an error "
         TX_ML_Interruption_u3 = case_when(otherdisaggregate == "IIT 0-3m ART" ~ TX_ML),
         TX_ML_Interruption_3_5 = case_when(otherdisaggregate == "IIT 3-5m ART" ~ TX_ML),
         TX_ML_Interruption_6o = case_when(otherdisaggregate == "IIT 6+m ART" ~ TX_ML),
         TX_ML_Interruption_Transfer_Out = case_when(otherdisaggregate == "Transferred Out" ~ TX_ML)) %>%
  pivot_longer(starts_with("TX_"), names_to = "indicator", values_to = "value") %>%
  filter(!indicator == "TX_ML") %>%
  drop_na(value)


# df_waterfall_lt <- dg_7 %>%
#   filter(date >= "2022-06-20" & !is.na(date),
#          !ageasentered %in% c("<15", "15+")) %>%
#   select(datim_uid,
#          period,
#          funding_agency,
#          partner_recode,
#          snu1,
#          psnu,
#          sitename,
#          sex,
#          ageasentered,
#          agecoarse,
#          otherdisaggregate,
#          TX_NEW,
#          TX_CURR, 
#          TX_CURR_3MO_PRIOR,
#          TX_RTT,
#          TX_ML,
#          TX_NET_NEW) %>%
#   mutate(ageasentered = case_when((period %in% c("2022 Q3", "2022 Q4", "2023 Q1") & ageasentered %in% c("50-54", "55-59", "60-64", "65+")) ~ "50+",
#                                   TRUE ~ ageasentered),
#          TX_ML_Died = case_when(otherdisaggregate == "Died" ~ TX_ML), # here i believe we have an error "
#          TX_ML_Interruption_u3 = case_when(otherdisaggregate == "IIT 0-3m ART" ~ TX_ML),
#          TX_ML_Interruption_3_5 = case_when(otherdisaggregate == "IIT 3-5m ART" ~ TX_ML),
#          TX_ML_Interruption_6o = case_when(otherdisaggregate == "IIT 6+m ART" ~ TX_ML),
#          TX_ML_Interruption_Transfer_Out = case_when(otherdisaggregate == "Transferred Out" ~ TX_ML)) %>%
#   pivot_longer(starts_with("TX_"), names_to = "indicator", values_to = "value") %>%
#   filter(!indicator == "TX_ML") %>%
#   drop_na(value)


df_waterfall %>%
  distinct(period, ageasentered) %>%
  arrange(period) %>%
  print(n = 100)

df_waterfall %>%
  distinct(partner_recode)

df_waterfall %>%
  distinct(datim_uid)

df_waterfall %>%
  distinct(sex)


# 1.10 PRINT DATAFRAMES TO DISK -------------------------------------------------


write_tsv(
  dg_7,
  "Dataout/results_cumulative_new.txt",
  na ="")

write_rds(
  dg_7,
  "Dataout/results_cumulative_new.rds")


write_tsv(
  dg_tb_covid,
  "Dataout/tb_covid_inputs.txt",
  na ="")


write_tsv(
  df_waterfall,
  glue("Dataout/supplemental_trn_in/mer_waterfall_cascade.txt"),
  na = "")

# write_tsv(
#   df_waterfall_lt,
#   glue("Dataout/supplemental_trn_in/mer_waterfall_cascade_lt.txt"),
#   na = "")

write.xlsx(dg_triagulation,
           glue(path_triangulation_output,{max_period_tri},'.xlsx'),
           overwrite = TRUE)


# PSNU SCRIPT CHAPTER -----------------------------------------------------

rm(dg_3, dg_5, dg_6, dg_7, dg_tb_covid, dg_triagulation, site_volume, ajuda_site_map)
gc()

dg_psnu <- read_rds(path_dg_psnu) 


# 2.1 DE-DUPLICATE, RESHAPE & CLEAN ----------------------------------------


dg_psnu_1 <- dg_psnu %>% 
  left_join(dedup_map) %>% 
  drop_na(indicator_new) %>% 
  select(-c(indicator)) %>% 
  rename(indicator = indicator_new) %>% 
  mutate(fy = as.numeric(fiscal_year)) %>%
  reshape_msd(direction = c("long")) %>% 
  filter(period_type %in% c("cumulative", "targets")) %>% 
  mutate(indicator_temp = paste0(indicator,
                                 if_else(numeratordenom %in% c("D"), "_D", "")))


# 2.2 CREATE TB INDICATORS TO USE IN TABLEAU ----------------------------

TB_STAT_POS <- dg_psnu_1 %>% 
  filter(indicator == "TB_STAT",
         statushiv == "Positive") %>% 
  mutate(indicator = recode(indicator,
                            "TB_STAT" = "TB_STAT_POS"),
         indicator_temp = recode(indicator_temp,
                                 "TB_STAT" = "TB_STAT_POS"))

TB_STAT_POS_NEW <- dg_psnu_1 %>% 
  filter(indicator == "TB_STAT",
         statushiv == "Positive",
         otherdisaggregate == "Newly Identified") %>% 
  mutate(indicator = recode(indicator,
                            "TB_STAT" = "TB_STAT_POS_NEW"),
         indicator_temp = recode(indicator_temp,
                                 "TB_STAT" = "TB_STAT_POS_NEW"))

TB_STAT_POS_ALREADY <- dg_psnu_1 %>% 
  filter(indicator == "TB_STAT",
         statushiv == "Positive",
         otherdisaggregate == "Known at Entry") %>% 
  mutate(indicator = recode(indicator,
                            "TB_STAT" = "TB_STAT_POS_ALREADY"),
         indicator_temp = recode(indicator_temp,
                                 "TB_STAT" = "TB_STAT_POS_ALREADY"))


TX_TB_D_POS <- dg_psnu_1 %>% 
  filter(indicator_temp == "TX_TB_D",
         statustb == "Positive") %>% 
  mutate(indicator = recode(indicator,
                            "TX_TB" = "TX_TB_D_POS"),
         indicator_temp = recode(indicator_temp,
                                 "TX_TB_D" = "TX_TB_D_POS"))


TX_TB_D_POS_ALREADY <- TX_TB_D_POS %>% 
  filter(indicator_temp == "TX_TB_D_POS",
         hiv_treatment_status == "Already") %>% 
  mutate(indicator = recode(indicator,
                            "TX_TB_D_POS" = "TX_TB_D_POS_ALREADY"),
         indicator_temp = recode(indicator_temp,
                                 "TX_TB_D_POS" = "TX_TB_D_POS_ALREADY"))


TX_TB_D_POS_NEW <- TX_TB_D_POS %>% 
  filter(indicator_temp == "TX_TB_D_POS",
         hiv_treatment_status == "New") %>% 
  mutate(indicator = recode(indicator,
                            "TX_TB_D_POS" = "TX_TB_D_POS_NEW"),
         indicator_temp = recode(indicator_temp,
                                 "TX_TB_D_POS" = "TX_TB_D_POS_NEW"))


TX_TB_D_NEG <- dg_psnu_1 %>% 
  filter(indicator_temp == "TX_TB_D",
         statustb == "Negative") %>% 
  mutate(indicator = recode(indicator,
                            "TX_TB" = "TX_TB_D_NEG"),
         indicator_temp = recode(indicator_temp,
                                 "TX_TB_D" = "TX_TB_D_NEG"))


TX_TB_D_NEG_ALREADY <- TX_TB_D_NEG %>% 
  filter(indicator_temp == "TX_TB_D_NEG",
         hiv_treatment_status == "Already") %>% 
  mutate(indicator = recode(indicator,
                            "TX_TB_D_NEG" = "TX_TB_D_NEG_ALREADY"),
         indicator_temp = recode(indicator_temp,
                                 "TX_TB_D_NEG" = "TX_TB_D_NEG_ALREADY"))


TX_TB_D_NEG_NEW <- TX_TB_D_NEG %>% 
  filter(indicator_temp == "TX_TB_D_NEG",
         hiv_treatment_status == "New") %>% 
  mutate(indicator = recode(indicator,
                            "TX_TB_D_NEG" = "TX_TB_D_NEG_NEW"),
         indicator_temp = recode(indicator_temp,
                                 "TX_TB_D_NEG" = "TX_TB_D_NEG_NEW"))

TB_PREV_NEW <- dg_psnu_1 %>% 
  filter(indicator_temp == "TB_PREV",
         hiv_treatment_status == "New") %>% 
  mutate(indicator = recode(indicator,
                            "TB_PREV" = "TB_PREV_NEW"),
         indicator_temp = recode(indicator_temp,
                                 "TB_PREV" = "TB_PREV_NEW"))

TB_PREV_ALREADY <- dg_psnu_1 %>% 
  filter(indicator_temp == "TB_PREV",
         hiv_treatment_status == "Already") %>% 
  mutate(indicator = recode(indicator,
                            "TB_PREV" = "TB_PREV_ALREADY"),
         indicator_temp = recode(indicator_temp,
                                 "TB_PREV" = "TB_PREV_ALREADY"))


TB_PREV_D_NEW <- dg_psnu_1 %>% 
  filter(indicator_temp == "TB_PREV_D",
         hiv_treatment_status == "New") %>% 
  mutate(indicator = recode(indicator,
                            "TB_PREV" = "TB_PREV_D_NEW"),
         indicator_temp = recode(indicator_temp,
                                 "TB_PREV_D" = "TB_PREV_D_NEW"))

TB_PREV_D_ALREADY <- dg_psnu_1 %>% 
  filter(indicator_temp == "TB_PREV_D",
         hiv_treatment_status == "Already") %>% 
  mutate(indicator = recode(indicator,
                            "TB_PREV" = "TB_PREV_D_ALREADY"),
         indicator_temp = recode(indicator_temp,
                                 "TB_PREV_D" = "TB_PREV_D_ALREADY"))


dg_psnu_2 <- bind_rows(dg_psnu_1, TB_STAT_POS, TB_STAT_POS_NEW, TB_STAT_POS_ALREADY, TX_TB_D_POS, TX_TB_D_POS_ALREADY, TX_TB_D_POS_NEW, TX_TB_D_NEG, TX_TB_D_NEG_ALREADY, TX_TB_D_NEG_NEW, TB_PREV_NEW, TB_PREV_ALREADY, TB_PREV_D_NEW, TB_PREV_D_ALREADY)
rm(dg_psnu_1, TB_STAT_POS, TB_STAT_POS_NEW, TB_STAT_POS_ALREADY, TX_TB_D_POS, TX_TB_D_POS_ALREADY, TX_TB_D_POS_NEW, TX_TB_D_NEG, TX_TB_D_NEG_ALREADY, TX_TB_D_NEG_NEW, TB_PREV_NEW, TB_PREV_ALREADY, TB_PREV_D_NEW, TB_PREV_D_ALREADY)
gc()

# 2.3 PIVOT INDICATORS WIDE & CLEAN-UP DATAFRAME -------------------------------------------------

dg_psnu_3 <- dg_psnu_2 %>% 
  mutate(row_n = row_number()) %>% 
  pivot_wider(names_from = period_type, values_from = value) %>% 
  select(-c(
    # standardizeddisaggregate,
    numeratordenom,
    hiv_treatment_status,
    row_n,
    indicator)) %>% 
  rename(fiscal_year = fy,
         agecoarse = trendscoarse,
         indicator = indicator_temp)

target_list <- dg_psnu_3 %>% 
  drop_na(targets) %>% 
  distinct(indicator)

dg_psnu_4 <- dg_psnu_3 %>% 
  inner_join(target_list, by = "indicator") 

 
# 2.4 PRINT DATAFRAME TO DISK -------------------------------------------------

write_tsv(
  dg_psnu_4,
  "Dataout/targets_cumulative.txt",
  na ="")

