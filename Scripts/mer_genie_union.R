

# LOAD LIBRARIES ------------------------------------


library(tidyverse)
library(lubridate)
library(gophr)
library(glamr)
library(readxl)
library(openxlsx)


# PATHS & VALUES ----------------------------------------------------------


# path_site_im_2018 <- "Data/genie/Genie_SITE_IM_2018.txt"
# path_site_im_2019 <- "Data/genie/Genie_SITE_IM_2019.txt"
# path_site_im_2020 <- "Data/genie/Genie_SITE_IM_2020.txt"
# path_site_im_2021 <- "Data/genie/Genie_SITE_IM_2021.txt"
path_site_im_2022 <- "Data/genie/Genie_SITE_IM_2022.txt"
path_site_im_2023_2024 <- "Data/genie/Genie_SITE_IM_2023_2024.txt"


# path_dg_psnu_2019_2020 <- "Data/Genie/Genie_PSNU_IM_2019_2020.txt"
# path_dg_psnu_2021 <- "Data/Genie/Genie_PSNU_IM_2021.txt"
path_dg_psnu_2022 <- "Data/Genie/Genie_PSNU_IM_2022.txt"
path_dg_psnu_2023_2024 <- "Data/Genie/Genie_PSNU_IM_2023_2024.txt"

path_output_im_site <- "Data/genie/dg_ref.txt"
path_output_im_site_rds <- "Data/genie/dg_ref.rds"
path_output_im_psnu <- "Data/genie/dg_ref_psnu.txt"
path_output_im_psnu_rds <- "Data/genie/dg_ref_psnu.rds"


# FUNCTIONS ---------------------------------------------------------------


clean_genie <- function(path){

df <- read_delim(path, # OK
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE) %>% 
  select(!c(countryname, 
            pre_rgnlztn_hq_mech_code, 
            communityprioritization, 
            facilityprioritization, 
            trendsfine, 
            trendssemifine,
            indicatortype,
            source_name,
            mech_code,
            typemilitary,
            primepartner,
            prime_partner_duns,
            award_number,
            community,
            communityuid,
            facilityuid,
            operatingunituid,
            facility,
            operatingunit,
            otherdisaggregate_sub,
            approvallevel,
            approvalleveldescription,
            dataelementuid,
            categoryoptioncombouid,
            snuprioritization
            )) %>% 
  rename(funding_agency = fundingagency) 

}


# LOAD DATASETS & ALIGN VARS ACROSS YEARS -----------------------------------------------------------


# dg_2018 <- clean_genie(path_site_im_2018)
# dg_2019 <- clean_genie(path_site_im_2019)
# dg_2020 <- clean_genie(path_site_im_2020)
# 
# dg_2021 <- read_delim(path_site_im_2021, # OK
#                       delim = "\t", escape_double = FALSE, 
#                       trim_ws = TRUE) %>% 
#   select(!c(country, 
#             prime_partner_uei, 
#             age_2018, 
#             age_2019,
#             indicatortype,
#             source_name,
#             mech_code,
#             typemilitary,
#             prime_partner_name,
#             prime_partner_duns,
#             award_number,
#             community,
#             communityuid,
#             facilityuid,
#             operatingunituid,
#             facility,
#             operatingunit,
#             otherdisaggregate_sub,
#             approvallevel,
#             approvalleveldescription,
#             dataelementuid,
#             categoryoptioncombouid,
#             snuprioritization))

dg_2022 <- read_delim(path_site_im_2022, # OK
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE) %>% 
  select(!c(country, 
            prime_partner_uei, 
            age_2018, 
            age_2019,
            indicatortype,
            source_name,
            mech_code,
            typemilitary,
            prime_partner_name,
            prime_partner_duns,
            award_number,
            community,
            communityuid,
            facilityuid,
            operatingunituid,
            facility,
            operatingunit,
            otherdisaggregate_sub,
            approvallevel,
            approvalleveldescription,
            dataelementuid,
            categoryoptioncombouid,
            snuprioritization,
            psnu,
            psnuuid,
            cop22_psnu,
            cop22_psnuuid,
            cop22_snuprioritization,
            is_indigenous_prime_partner,
            use_for_age,
            target_age_2024,
            target_modality_2024,
            safe_for_net_new,
            safe_for_vlc)) %>% 
  rename(psnu = snu2,
         psnuuid = snu2uid) 


dg_2023_2024 <- read_delim(path_site_im_2023_2024, # OK
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE) %>% 
  
  select(!c(country, 
            prime_partner_uei, 
            age_2018, 
            age_2019,
            indicatortype,
            source_name,
            mech_code,
            typemilitary,
            prime_partner_name,
            prime_partner_duns,
            award_number,
            community,
            communityuid,
            facilityuid,
            operatingunituid,
            facility,
            operatingunit,
            otherdisaggregate_sub,
            approvallevel,
            approvalleveldescription,
            dataelementuid,
            categoryoptioncombouid,
            snuprioritization,
            psnu,
            psnuuid,
            snu2,
            snu2uid,
            cop22_snuprioritization,
            is_indigenous_prime_partner,
            use_for_age,
            target_age_2024,
            target_modality_2024,
            safe_for_net_new,
            safe_for_vlc)) %>% 
  rename(psnu = cop22_psnu,
         psnuuid = cop22_psnuuid)

# setdiff(names(dg_2022), names(dg_2023_2024))
# setdiff(names(dg_2021), names(dg_2023_2024))

# BIND DATASETS & CLEAN MECH NAMES -----------------------------------------------------------


dg_2022_2024 <- bind_rows(dg_2022, dg_2023_2024) %>% 
  mutate(mech_name = recode(mech_name, 
                            
                            # clinical partner recoding
                            "Efficiencies for Clinical HIV Outcomes (ECHO)" = "ECHO", 
                            "Friends in Global Health" = "FGH", 
                            "Ariel" = "ARIEL", 
                            "Clinical Services System Strenghening (CHASS)" = "CHASS",
                            "Ministry of Health Mozambique"= "MISAU",
                            
                            # placeholder recoding
                            "[Placeholder - 160448 Mozambique HHS/CDC]" = "ARIEL",
                            "[Placeholder - 160450 Mozambique HHS/CDC]" = "EGPAF",
                            "[Placeholder - 160467 Mozambique USAID]" = "ICRH-KP",
                            "[Placeholder - 160472 Mozambique USAID]" = "VMMC Local Activity",
                            "[Placeholder - 160466 Mozambique USAID]" = "OCSIDA",
                      
                            # prevention partner recoding
                            "Integrated HIV Prevention and Health Services for Key and Priority Populations (HIS-KP)" = "Passos",
                            "Key & Priority Populations (KPP) Local Activity in Mozambique/PASSOS+" = "Passos",
                            "Service Delivery and Support for Orphans and Vulnerable Children" = "COVIDA",
                            "Community Based HIV Services for the Southern Region" = "Hlayissa",
                            "Community HIV Activity in Zambezia" = "Nweti",
                            "OVC Response (Sofala)" = "COMUSANAS",
                            "OVC Response (Manica)" = "ANDA",
                            "Strengthen Family and Community Support to Orphan and Vunerable Children (FORCA Project)" = "FORCA",
                            "VMMC Services in Manica and Tete" = "VMMC M/T",
                            "Voluntary Medical Male Circumcision (VMMC) Local Activity in Mozambique" = "VMMC M/T",
                            "Voluntary Medical Male Circumcision (VMMC) Local Activity" = "VMMC M/T",
                            "Communication for Improved Health Outcomes (CIHO)" = "CIHO",
                            "Food and Nutrition Technical Assistance III (FANTA-III)" = "FANTA-III",
                            "Strengthening High Impact Interventions for an AIDS-Free Generation (AIDSFree) Project" = "AIDSFree",
                            "YouthPower Implementation -  Task Order 1" = "YouthPower",
                            "SCIP Zambezia" = "SCIP",
                            "HIV Community-Based Services (Nampula)" = "HIV CBS (Nampula)",
                            "Facilitating Impact that Lasts for Orphans and Vulnerable Children (FILOVC)" = "FILOVC",

                            # dod jhpiego recoding
                            "FADM HIV Treatment Scale-Up Program" = "JHPIEGO-DoD",
                            "FADM Prevention and Circumcision Program" = "JHPIEGO-DoD",
                            "JHPIEGO CORPORATION" = "JHPIEGO-DoD",
                            "FADM PCP" = "JHPIEGO-DoD",
                            
                            # cdc jhpiego recoding
                            "JHPIEGO Corporation" = "JHPIEGO",
                            "JHPIEGO HRH" = "JHPIEGO",
                            "Johns Hopkins" = "JHPIEGO",
                            
                            # Capacity Building for Sustainable HIV Services (UW) recoding
                            "Capacity Building for Sustainable HIV Services" = "CBSHS",
                            
                            # state & peace corps recoding
                            "Integrating Early Child Development (ECD - GDA)" = "ECD-GDA",
                            "P/E Quick Impact Program" = "P/E QIP",
                            "United States Peace Corps/ Mozambique" = "USPC",
                            "Management and Operations" = "USPC"),
         
         modality = recode(modality, 
                            "Index" = "Index Facility", 
                            "IndexMod" = "Index Community"))
  


# dg_2022_2024 %>% 
#   distinct(fiscal_year, funding_agency, mech_name) %>% 
#   arrange(fiscal_year, funding_agency, mech_name) %>% 
#   select(fiscal_year, funding_agency, mech_name) %>% 
#   view()

# dg_2022_2024 %>% 
#   distinct(modality) %>% 
#   arrange(modality) %>% 
#   select(modality) %>% 
#   view()


# DATA CORRECTIONS ----------------------------------------------------------


# Clean Genie Issue That Links Ariel & Icap Cadeia Central To Military
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="siMZUtd2cJW" & dg_2022_2024$mech_name=="ARIEL"]<-"oX9C1fOEzSk"
dg_2022_2024$sitename[dg_2022_2024$orgunituid=="oX9C1fOEzSk" & dg_2022_2024$mech_name=="ARIEL"]<-"Cadeia Central PS"
dg_2022_2024$snu1[dg_2022_2024$orgunituid=="oX9C1fOEzSk" & dg_2022_2024$mech_name=="ARIEL"]<-"Maputo"
dg_2022_2024$psnu[dg_2022_2024$orgunituid=="oX9C1fOEzSk" & dg_2022_2024$mech_name=="ARIEL"]<-"Matola"

dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="siMZUtd2cJW" & dg_2022_2024$mech_name=="ICAP"]<-"oX9C1fOEzSk"
dg_2022_2024$sitename[dg_2022_2024$orgunituid=="oX9C1fOEzSk" & dg_2022_2024$mech_name=="ICAP"]<-"Cadeia Central PS"
dg_2022_2024$snu1[dg_2022_2024$orgunituid=="oX9C1fOEzSk" & dg_2022_2024$mech_name=="ICAP"]<-"Maputo"
dg_2022_2024$psnu[dg_2022_2024$orgunituid=="oX9C1fOEzSk" & dg_2022_2024$mech_name=="ICAP"]<-"Matola"


#Accounting for historical error in Chicavane and Chizavane: Converting EGPAF Chizavane to EGPAF Chicavane
dg_2022_2024$psnu[dg_2022_2024$orgunituid=="EjFYleP5G9K" & dg_2022_2024$mech_name=="EGPAF"]<-"Chonguene"
dg_2022_2024$sitename[dg_2022_2024$orgunituid=="EjFYleP5G9K" & dg_2022_2024$mech_name=="EGPAF"]<-"Chicavane CS"
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="EjFYleP5G9K" & dg_2022_2024$mech_name=="EGPAF"]<-"LqB6YZq9sG2"


#Accounting for historical error in Chicavane and Chizavane: Converting MISAU Chicavane to MISAU Chizavane
dg_2022_2024$psnu[dg_2022_2024$orgunituid=="LqB6YZq9sG2" & dg_2022_2024$mech_name=="MISAU"]<-"Mandlakaze"
dg_2022_2024$psnuuid[dg_2022_2024$orgunituid=="LqB6YZq9sG2" & dg_2022_2024$mech_name=="MISAU"]<-"VjJPal9jqUi"  # VERIFY THIS ADDRESSES ISSUE IDENTIFIED BY KIRSTIN
dg_2022_2024$sitename[dg_2022_2024$orgunituid=="LqB6YZq9sG2" & dg_2022_2024$mech_name=="MISAU"]<-"Chizavane CS"
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="LqB6YZq9sG2" & dg_2022_2024$mech_name=="MISAU"]<-"EjFYleP5G9K"

#Adjusting Sites Imported by PSM
dg_2022_2024$sitename[dg_2022_2024$orgunituid=="FWbL33AHdCC" & dg_2022_2024$mech_name=="PSM"]<-"Mandlakazi HR"
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="FWbL33AHdCC" & dg_2022_2024$mech_name=="PSM"]<-"xlYle4v5GZ8"

dg_2022_2024$sitename[dg_2022_2024$orgunituid=="qM7gh9auEmt" & dg_2022_2024$mech_name=="PSM"]<-"Massinga CS"
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="qM7gh9auEmt" & dg_2022_2024$mech_name=="PSM"]<-"wSW662GVwZP"

dg_2022_2024$sitename[dg_2022_2024$orgunituid=="W7CqwG3AuDh" & dg_2022_2024$mech_name=="PSM"]<-"Chicuque HR"
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="W7CqwG3AuDh" & dg_2022_2024$mech_name=="PSM"]<-"s28njgt1wYe"


#Setting up the HRSA/ ITECH at Alto Mae so it is not matched to CCS
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="jKg6rpNATKH" & dg_2022_2024$mech_name=="ITECH"]<-"jKg6rpNATKH_ITECH"

# Adjusting Inhamudima to Inhamuzia to align with new COP22 reporting 
dg_2022_2024$sitename[dg_2022_2024$orgunituid=="EslaBcDD5mZ"]<-"Inhamizua CS"
dg_2022_2024$orgunituid[dg_2022_2024$orgunituid=="EslaBcDD5mZ"]<-"wmXdfKNX0qT"




# PRINT IM SITE OUTPUT TO DISK ----------------------------------------------------


write_rds(
  dg_2022_2024,
  path_output_im_site_rds)


# MUNGE IM PSNU GENIE -----------------------------------------------------


# dg_psnu_2019_2020 <- read_msd(path_dg_psnu_2019_2020) %>% 
#   select(-c(operatingunit,
#             operatingunituid,
#             country,
#             snuprioritization,
#             typemilitary,
#             prime_partner_name,
#             mech_code,
#             prime_partner_duns,
#             award_number,
#             indicatortype,
#             categoryoptioncomboname,
#             age_2018,
#             age_2019,
#             otherdisaggregate_sub,
#             source_name))


# dg_psnu_2021 <- read_msd(path_dg_psnu_2021) %>% 
#   select(-c(operatingunit,
#             operatingunituid,
#             country,
#             snuprioritization,
#             typemilitary,
#             prime_partner_name,
#             mech_code,
#             prime_partner_duns,
#             award_number,
#             indicatortype,
#             categoryoptioncomboname,
#             age_2018,
#             age_2019,
#             otherdisaggregate_sub,
#             source_name))


dg_psnu_2022 <- read_psd(path_dg_psnu_2022) %>% 
  select(-c(operatingunit,
            operatingunituid,
            country,
            snuprioritization,
            typemilitary,
            prime_partner_name,
            mech_code,
            prime_partner_duns,
            award_number,
            indicatortype,
            categoryoptioncomboname,
            age_2018,
            age_2019,
            otherdisaggregate_sub,
            source_name,
            psnu,
            psnuuid)) %>% 
  rename(psnu = cop22_psnu,
         psnuuid = cop22_psnuuid) 



dg_psnu_2023_2024 <- read_psd(path_dg_psnu_2023_2024) %>% 
  select(-c(operatingunit,
            operatingunituid,
            country,
            snuprioritization,
            typemilitary,
            prime_partner_name,
            mech_code,
            prime_partner_duns,
            award_number,
            indicatortype,
            categoryoptioncomboname,
            age_2018,
            age_2019,
            otherdisaggregate_sub,
            source_name,
            cop22_psnu,
            cop22_psnuuid))

# setdiff(names(dg_psnu_2022), names(dg_psnu_2023_2024))
# setdiff(names(dg_psnu_2023_2024), names(dg_psnu_2022))

dg_psnu_2022_2024 <- bind_rows(dg_psnu_2022, dg_psnu_2023_2024) %>%
  mutate(mech_name = recode(mech_name, 
                            
                            # clinical partner recoding
                            "Efficiencies for Clinical HIV Outcomes (ECHO)" = "ECHO", 
                            "Friends in Global Health" = "FGH", 
                            "Ariel" = "ARIEL", 
                            "Clinical Services System Strenghening (CHASS)" = "CHASS",
                            "Ministry of Health Mozambique"= "MISAU",
                            
                            # placeholder recoding
                            "[Placeholder - 160448 Mozambique HHS/CDC]" = "ARIEL",
                            "[Placeholder - 160450 Mozambique HHS/CDC]" = "EGPAF",
                            "[Placeholder - 160467 Mozambique USAID]" = "ICRH-KP",
                            "[Placeholder - 160472 Mozambique USAID]" = "VMMC Local Activity",
                            "[Placeholder - 160466 Mozambique USAID]" = "OCSIDA",
                            
                            # prevention partner recoding
                            "Integrated HIV Prevention and Health Services for Key and Priority Populations (HIS-KP)" = "Passos",
                            "Key & Priority Populations (KPP) Local Activity in Mozambique/PASSOS+" = "Passos",
                            "Service Delivery and Support for Orphans and Vulnerable Children" = "COVIDA",
                            "Community Based HIV Services for the Southern Region" = "Hlayissa",
                            "Community HIV Activity in Zambezia" = "Nweti",
                            "OVC Response (Sofala)" = "COMUSANAS",
                            "OVC Response (Manica)" = "ANDA",
                            "Strengthen Family and Community Support to Orphan and Vunerable Children (FORCA Project)" = "FORCA",
                            "VMMC Services in Manica and Tete" = "VMMC M/T",
                            "Voluntary Medical Male Circumcision (VMMC) Local Activity in Mozambique" = "VMMC M/T",
                            "Voluntary Medical Male Circumcision (VMMC) Local Activity" = "VMMC M/T",
                            "Communication for Improved Health Outcomes (CIHO)" = "CIHO",
                            "Food and Nutrition Technical Assistance III (FANTA-III)" = "FANTA-III",
                            "Strengthening High Impact Interventions for an AIDS-Free Generation (AIDSFree) Project" = "AIDSFree",
                            "YouthPower Implementation -  Task Order 1" = "YouthPower",
                            "SCIP Zambezia" = "SCIP",
                            "HIV Community-Based Services (Nampula)" = "HIV CBS (Nampula)",
                            "Facilitating Impact that Lasts for Orphans and Vulnerable Children (FILOVC)" = "FILOVC",

                            
                            # dod jhpiego recoding
                            "FADM HIV Treatment Scale-Up Program" = "JHPIEGO-DoD",
                            "FADM Prevention and Circumcision Program" = "JHPIEGO-DoD",
                            "JHPIEGO CORPORATION" = "JHPIEGO-DoD",
                            "FADM PCP" = "JHPIEGO-DoD",
                            
                            # cdc jhpiego recoding
                            "JHPIEGO Corporation" = "JHPIEGO",
                            "JHPIEGO HRH" = "JHPIEGO",
                            "Johns Hopkins" = "JHPIEGO",
                            
                            # Capacity Building for Sustainable HIV Services (UW) recoding
                            "Capacity Building for Sustainable HIV Services" = "CBSHS",
                            
                            # state & peace corps recoding
                            "Integrating Early Child Development (ECD - GDA)" = "ECD-GDA",
                            "P/E Quick Impact Program" = "P/E QIP",
                            "United States Peace Corps/ Mozambique" = "USPC",
                            "Management and Operations" = "USPC"),
         
         modality = recode(modality, 
                           "Index" = "Index Facility", 
                           "IndexMod" = "Index Community"))


# dg_psnu_2022_2024 %>% 
#   distinct(fiscal_year, funding_agency, mech_name) %>% 
#   arrange(fiscal_year, funding_agency, mech_name) %>% 
#   select(fiscal_year, funding_agency, mech_name) %>% 
#   view()

# dg_psnu_2022_2024 %>% 
#   distinct(modality) %>% 
#   arrange(modality) %>% 
#   select(modality) %>% 
#   view()


# PRINT IM PSNU OUTPUT TO DISK --------------------------------------------


write_rds(
  dg_psnu_2022_2024,
  path_output_im_psnu_rds)

