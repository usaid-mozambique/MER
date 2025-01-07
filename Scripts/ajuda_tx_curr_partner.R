library(tidyverse)
library(readxl)


source_mer <- read_excel("Data/tx_curr_q3.xlsx")

soure_other_p <- read_excel("C:/Users/jlara/Downloads/ajuda_adapt.xlsx", 
                      sheet = "otherpartners_wGF (2)") |> 
  select(datim_uid, partner_nonpepfar_sustain)

source_case_finding <- read_excel("Data/Ajuda_sustainability_sites_2024Q2_6.14.24_w GF.xlsx",
                                  sheet = "sites_2024Q2") |> 
  select(datim_uid = orgunituid,
         cfp =`Case Finding Potential`) |> 
  mutate(cfp = as.numeric(cfp))



df <- source_mer |> 
  left_join(soure_other_p, by = join_by(datim_uid == datim_uid)) |> 
  left_join(source_case_finding, by = join_by(datim_uid == datim_uid))

df1 <- df %>%
  mutate(
    cfp = round(cfp),
    site_d = 1,
    partner_code = case_when(
      
      # sustainability
      is.na(partner_pepfar_clinical) & is.na(partner_nonpepfar_sustain) ~ "No Support", # ok
      
      # sustainability
      is.na(partner_pepfar_clinical) & !is.na(partner_nonpepfar_sustain) ~ "Other Donor Support", # ok
      
      # ajuda only
      partner_pepfar_clinical %in% c("ARIEL", "CCS", "ECHO", "EGPAF", "FGH", "ICAP", "ITECH", "JHPIEGO-DoD", "MISAU") & is.na(partner_nonpepfar_sustain) ~ "AJUDA", 
      
      # # ajuda graduation only
      # partner_pepfar_clinical %in% c("MISAU") & is.na(partner_nonpepfar_sustain) ~ "AJUDA Graduation", 
      
      # ajuda plus
      partner_pepfar_clinical %in% c("ARIEL", "CCS", "ECHO", "EGPAF", "FGH", "ICAP", "ITECH", "JHPIEGO-DoD", "MISAU") & !is.na(partner_nonpepfar_sustain) ~ "AJUDA", 
      
      # ajuda graduation plus
      partner_pepfar_clinical %in% c("MISAU") & is.na(partner_nonpepfar_sustain) ~ "AJUDA"
    
  )) |> 

  relocate(starts_with("partner"), .after = sitename) |> 
  select(!site_d)



df1 |> count(partner_pepfar_clinical, partner_nonpepfar_sustain)


test_no_support <- df1 |> 
  filter(partner_code == "No Support")
test_no_support |> 
  distinct(partner_pepfar_clinical, partner_nonpepfar_sustain, partner_code)


test_other_support <- df1 |> 
  filter(partner_code == "Other Support")
test_other_support |> 
  distinct(partner_pepfar_clinical, partner_nonpepfar_sustain, partner_code)


test_ajuda <- df1 |> 
  filter(partner_code == "AJUDA")
test_ajuda |> 
  distinct(partner_pepfar_clinical, partner_nonpepfar_sustain, partner_code)


test_ajuda_grad <- df1 |> 
  filter(partner_code == "AJUDA Graduation")
test_ajuda_grad |> 
  distinct(partner_pepfar_clinical, partner_nonpepfar_sustain, partner_code)


test_ajuda_other <- df1 |> 
  filter(partner_code == "AJUDA & Other")
test_ajuda_other |> 
  distinct(partner_pepfar_clinical, partner_nonpepfar_sustain, partner_code)


test_ajuda_grad_other <- df1 |> 
  filter(partner_code == "AJUDA Graduation & Other")
test_ajuda_grad_other |> 
  distinct(partner_pepfar_clinical, partner_nonpepfar_sustain, partner_code)


test_unknown <- df1 |> 
  filter(is.na(partner_code))
test_unknown |> 
  distinct(partner_pepfar_clinical, partner_nonpepfar_sustain, partner_code)


df1 |> 
  write_csv("Documents/ishani_ajuda.csv",
            na = "")


df2 <- df1 |> 
  
