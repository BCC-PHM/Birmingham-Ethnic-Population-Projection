setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 03_fertility_process.R
# process MSDS data 
# caculate ethnic age specific fertility rate
# obtain transition matrix of mixed babies 
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)

ward_map = st_read("data/boundaries/boundaries-wards-2022-birmingham/boundaries-wards-2022-birmingham.shp")

MSDS_data =  read_delim("data/fertiliy/Fertility_data.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

bham_eth_pop = read.csv("data/processed/bham_eth_pop.csv")

#=============================================================================

MSDS_data_filtered = MSDS_data %>% 
  filter(YearOfBirthBaby %in% c(2022,2023,2024,2025)) %>% 
  filter(ElectoralWardMother %in% ward_map$Ward_Code)%>%
  mutate(eth_code_mother = case_when(
    EthnicCategoryMother %in% c("A", "B")             ~ "WBI",
    EthnicCategoryMother == "C"                        ~ "WHO",
    EthnicCategoryMother %in% c("D", "E", "F", "G")  ~ "MIX",
    EthnicCategoryMother == "H"                        ~ "IND",
    EthnicCategoryMother == "J"                        ~ "PAK",
    EthnicCategoryMother == "K"                        ~ "BAN",
    EthnicCategoryMother == "R"                        ~ "CHI",
    EthnicCategoryMother == "L"                        ~ "OAS",
    EthnicCategoryMother == "N"                        ~ "BLA",
    EthnicCategoryMother == "M"                        ~ "BLC",
    EthnicCategoryMother == "P"                        ~ "OBL",
    EthnicCategoryMother == "S"                        ~ "OTH",
    EthnicCategoryMother %in% c("Z", "99")            ~ NA_character_
  )) %>% 
  filter(!is.na(eth_code_mother))%>%
  mutate( age_group_5yr = case_when(
    AgeRPEndDate >= 0  & AgeRPEndDate <= 4  ~ "0-4",
    AgeRPEndDate >= 5  & AgeRPEndDate <= 9  ~ "5-9",
    AgeRPEndDate >= 10 & AgeRPEndDate <= 14 ~ "10-14",
    AgeRPEndDate >= 15 & AgeRPEndDate <= 19 ~ "15-19",
    AgeRPEndDate >= 20 & AgeRPEndDate <= 24 ~ "20-24",
    AgeRPEndDate >= 25 & AgeRPEndDate <= 29 ~ "25-29",
    AgeRPEndDate >= 30 & AgeRPEndDate <= 34 ~ "30-34",
    AgeRPEndDate >= 35 & AgeRPEndDate <= 39 ~ "35-39",
    AgeRPEndDate >= 40 & AgeRPEndDate <= 44 ~ "40-44",
    AgeRPEndDate >= 45 & AgeRPEndDate <= 49 ~ "45-49",
    AgeRPEndDate >= 50 & AgeRPEndDate <= 54 ~ "50-54",
    AgeRPEndDate >= 55 & AgeRPEndDate <= 59 ~ "55-59",
    AgeRPEndDate >= 60 & AgeRPEndDate <= 64 ~ "60-64",
    AgeRPEndDate >= 65 & AgeRPEndDate <= 69 ~ "65-69",
    AgeRPEndDate >= 70 & AgeRPEndDate <= 74 ~ "70-74",
    AgeRPEndDate >= 75 & AgeRPEndDate <= 79 ~ "75-79",
    AgeRPEndDate >= 80 & AgeRPEndDate <= 84 ~ "80-84",
    AgeRPEndDate >= 85 & AgeRPEndDate <= 89 ~ "85-89",
    AgeRPEndDate >= 90             ~ "90+",
    TRUE ~ NA_character_
  )) %>% 
  filter(age_group_5yr %in%c (c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")))



MSDS_counts = MSDS_data_filtered %>% 
  group_by(eth_code_mother, age_group_5yr, YearOfBirthBaby) %>%
  summarise(n = n(), .groups = "drop")



#---------------------------------------------------------------------



MSDS_data_filtered_na = MSDS_data %>% 
  filter(YearOfBirthBaby %in% c(2022,2023,2024,2025)) %>% 
  filter(ElectoralWardMother %in% ward_map$Ward_Code)%>%
  mutate(eth_code_mother = case_when(
    EthnicCategoryMother %in% c("A", "B")             ~ "WBI",
    EthnicCategoryMother == "C"                        ~ "WHO",
    EthnicCategoryMother %in% c("D", "E", "F", "G")  ~ "MIX",
    EthnicCategoryMother == "H"                        ~ "IND",
    EthnicCategoryMother == "J"                        ~ "PAK",
    EthnicCategoryMother == "K"                        ~ "BAN",
    EthnicCategoryMother == "R"                        ~ "CHI",
    EthnicCategoryMother == "L"                        ~ "OAS",
    EthnicCategoryMother == "N"                        ~ "BLA",
    EthnicCategoryMother == "M"                        ~ "BLC",
    EthnicCategoryMother == "P"                        ~ "OBL",
    EthnicCategoryMother == "S"                        ~ "OTH",
    EthnicCategoryMother %in% c("Z", "99")            ~ NA_character_
  )) %>% 
  filter(is.na(eth_code_mother)) %>% 
  mutate( age_group_5yr = case_when(
    AgeRPEndDate >= 0  & AgeRPEndDate <= 4  ~ "0-4",
    AgeRPEndDate >= 5  & AgeRPEndDate <= 9  ~ "5-9",
    AgeRPEndDate >= 10 & AgeRPEndDate <= 14 ~ "10-14",
    AgeRPEndDate >= 15 & AgeRPEndDate <= 19 ~ "15-19",
    AgeRPEndDate >= 20 & AgeRPEndDate <= 24 ~ "20-24",
    AgeRPEndDate >= 25 & AgeRPEndDate <= 29 ~ "25-29",
    AgeRPEndDate >= 30 & AgeRPEndDate <= 34 ~ "30-34",
    AgeRPEndDate >= 35 & AgeRPEndDate <= 39 ~ "35-39",
    AgeRPEndDate >= 40 & AgeRPEndDate <= 44 ~ "40-44",
    AgeRPEndDate >= 45 & AgeRPEndDate <= 49 ~ "45-49",
    AgeRPEndDate >= 50 & AgeRPEndDate <= 54 ~ "50-54",
    AgeRPEndDate >= 55 & AgeRPEndDate <= 59 ~ "55-59",
    AgeRPEndDate >= 60 & AgeRPEndDate <= 64 ~ "60-64",
    AgeRPEndDate >= 65 & AgeRPEndDate <= 69 ~ "65-69",
    AgeRPEndDate >= 70 & AgeRPEndDate <= 74 ~ "70-74",
    AgeRPEndDate >= 75 & AgeRPEndDate <= 79 ~ "75-79",
    AgeRPEndDate >= 80 & AgeRPEndDate <= 84 ~ "80-84",
    AgeRPEndDate >= 85 & AgeRPEndDate <= 89 ~ "85-89",
    AgeRPEndDate >= 90             ~ "90+",
    TRUE ~ NA_character_
  )) %>% 
  filter(age_group_5yr %in%c (c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")))









#------------------------------------------------------
#load the composition data 
#------------------------------------------------------
bham_ward_ethnic_composition = read_csv("data/processed/bham_ward_ethnic_composition.csv")



MSDS_counts_redis = MSDS_data_filtered_na %>% 
  group_by(ElectoralWardMother, age_group_5yr, YearOfBirthBaby) %>%
  summarise(n = n(), .groups = "drop")



#----------------------------------------------
#cross join counts with ward proportions and allocate
#----------------------------------------------

redistributed = MSDS_counts_redis %>% 
  left_join(bham_ward_ethnic_composition, by = c("ElectoralWardMother" = "Ward_code" ),relationship = "many-to-many") %>% 
  mutate(mother_redistributed = n * Per_com) %>% 
  group_by(eth_code, age_group_5yr, YearOfBirthBaby) %>% 
  summarise(n = sum(mother_redistributed), .groups = "drop") %>% 
  rename(eth_code_mother = eth_code)


#==========================================================
# add redistributed mothers back into main counts
#==========================================================
MSDS_counts_final = MSDS_counts %>% 
  left_join(redistributed, 
            by = c("eth_code_mother", "age_group_5yr", "YearOfBirthBaby"),
            suffix = c("_observed", "_redistributed")) %>% 
  mutate(
    n_redistributed = replace_na(n_redistributed, 0),
    n_total = n_observed + n_redistributed
  )


MSDS_counts_final = MSDS_counts_final %>% 
  complete(
    eth_code_mother      = levels(MSDS_counts$eth_code_mother),
    age_group_5yr = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49"),
    YearOfBirthBaby         = c(2022,2023,2024,2025),
    fill          = list(n_observed = 0, n_redistributed = 0, n_total = 0)
  ) %>% 
  mutate(age_group_5yr = factor(age_group_5yr, levels = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")))




#==================================================================
#i found that there are too few births, some strata has 0 deaths_avg
#we need to use empirical bayes shrinkage


# create complete grid first
complete_grid = expand.grid(
  eth_code_mother = unique(MSDS_counts_final$eth_code_mother),
  age_group_5yr   = unique(MSDS_counts_final$age_group_5yr),
  stringsAsFactors = FALSE
)

#calculate raw fertility rates
prior_fertility = MSDS_counts_final %>% 
  group_by(eth_code_mother, age_group_5yr) %>% 
  summarise(fertility_avg = mean(n_total), .groups = "drop") %>% 
  left_join(bham_eth_pop %>% filter(DEC_SEX == "Female"), by = c("eth_code_mother" = "eth_code", "age_group_5yr")) %>% 
  mutate(raw_rate = fertility_avg / Observation) %>% 
  group_by(age_group_5yr) %>% 
  summarise(
    # Calculate the global mean of the rates
    mu = mean(raw_rate, na.rm = TRUE),
    v  = pmax(var(raw_rate, na.rm = TRUE), 1e-10),
    .groups = "drop") %>% 
  mutate(alpha = mu^2 / v,  #Calculate α and β
         beta  = mu / v)

#apply EB shrinkage
smoothed_data = complete_grid %>% 
  left_join(
    MSDS_counts_final %>% 
      group_by(eth_code_mother, age_group_5yr) %>% 
      summarise(fertility_avg = mean(n_total), .groups = "drop"),
    by = c("eth_code_mother", "age_group_5yr")
  ) %>% 
  mutate(fertility_avg = replace_na(fertility_avg, 0)) %>%   # CHI 15-19 gets 0
  left_join(bham_eth_pop %>% filter(DEC_SEX == "Female"), 
            by = c("eth_code_mother" = "eth_code", "age_group_5yr")) %>% 
  mutate(
    raw_rate = fertility_avg / Observation
  ) %>% 
  left_join(prior_fertility, by = "age_group_5yr") %>% 
  mutate(fx_EB_baseline  = (fertility_avg + alpha) / (Observation + beta)) %>% 
  select(
    eth_code_mother,
    age_group_5yr,
    fertility_avg,
    Observation,
    raw_rate,
    fx_EB_baseline,
    mu,
    v,
    alpha,
    beta
  )


################################################################################
# smoothed_data  %>%
#   group_by(eth_code_mother) %>%
#   summarise(tfr = 5 * sum(fx_EB)) %>%
#   arrange(desc(tfr))


################################################################################
#since we need to create future fertility rate
#it is diffcult cause there are no data for that no trend for that
#given that i will apply the ONS assumption of future fertility by age group
#obtain the rate of change instead of using it directly to create our bham version 

ONS_fertility_projection = read_excel("data/fertiliy/Figure_4__Fertility_rates_for_women_.xlsx", 
                                                                             skip = 6)
# 
# 
# #  Use 2021–2037 terminal ONS trend to project ASFR beyond 2047
# ONS_rate_change_2021_37 = ONS_fertility_projection %>% rename(
#   age_15_19  = `15 to 19`,
#   age20_24  = `20 to 24`,
#   age25_29  = `25 to 29`,
#   age30_34  = `30 to 34`,
#   age35_39  = `35 to 39`,
#   age40_44  = `40 to 46`) %>% 
#   mutate(across(everything(), as.numeric),
#          age_45_49 = age40_44) %>%
#   filter(Year %in% c(2021,2037)) %>% 
#   mutate(Year = as.character(Year)) %>% 
#   mutate(across(starts_with("age"), ~ .x / 1000)) %>% 
#   pivot_longer(-Year, 
#                names_to = "age_group_5yr", 
#                values_to = "ons_asfr")%>% 
#   pivot_wider(
#     names_from = Year,
#     values_from = ons_asfr,
#     names_prefix = "asfr_"
#   )%>% 
#   mutate(
#     n_years = 2037 - 2021,
#     annual_change = (asfr_2037 / asfr_2021)^(1 / n_years) - 1,
#     annual_change_percent = annual_change * 100
#   )



#  Use 2038–2047 terminal ONS trend to project ASFR beyond 2047
ONS_rate_change_2038_47 = ONS_fertility_projection %>% rename(
  age_15_19  = `15 to 19`,
  age20_24  = `20 to 24`,
  age25_29  = `25 to 29`,
  age30_34  = `30 to 34`,
  age35_39  = `35 to 39`,
  age40_44  = `40 to 46`) %>% 
  mutate(across(everything(), as.numeric),
         age_45_49 = age40_44) %>%
  filter(Year %in% c(2038,2047)) %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(across(starts_with("age"), ~ .x / 1000)) %>% 
  pivot_longer(-Year, 
               names_to = "age_group_5yr", 
               values_to = "ons_asfr")%>% 
  pivot_wider(
    names_from = Year,
    values_from = ons_asfr,
    names_prefix = "asfr_"
  )%>% 
  mutate(
    n_years = 2047 - 2038,
    annual_change = (asfr_2047 / asfr_2038)^(1 / n_years) - 1,
    annual_change_percent = annual_change * 100
  )

# calculate scaling factors at each 5-year projection step

ONS_projected_2048_2061= ONS_rate_change_2038_47%>% 
  select(age_group_5yr, annual_change) %>% 
  crossing(Year = 2048:2061) %>% 
  left_join(
    ONS_rate_change_2038_47%>% 
      select(age_group_5yr, asfr_2047),
    by = "age_group_5yr"
  ) %>% 
  mutate(
    years_after_start = Year - 2047,
    ons_asfr = asfr_2047 * (1 + annual_change)^years_after_start
  )%>% 
  select(Year, age_group_5yr, ons_asfr)

#Extend ONS ASFR from 2048 to 2061
ONS_fertility_projection %>% rename(
  age_15_19  = `15 to 19`,
  age20_24  = `20 to 24`,
  age25_29  = `25 to 29`,
  age30_34  = `30 to 34`,
  age35_39  = `35 to 39`,
  age40_44  = `40 to 46`) %>% 
  mutate(across(everything(), as.numeric),
         age_45_49 = age40_44)  %>% 
  mutate(Year = as.character(Year)) %>% 
  filter(Year >=2021) %>% 
  mutate(across(starts_with("age"), ~ .x / 1000)) %>% 
  pivot_longer(-Year, 
               names_to = "age_group_5yr", 
               values_to = "ons_asfr")



#Put actual ONS 2021–2047 and extended 2048–2061 together
ONS_full_2021_2061 = bind_rows(
  ONS_fertility_projection %>% rename(
    age_15_19  = `15 to 19`,
    age20_24  = `20 to 24`,
    age25_29  = `25 to 29`,
    age30_34  = `30 to 34`,
    age35_39  = `35 to 39`,
    age40_44  = `40 to 46`) %>% 
    mutate(across(everything(), as.numeric),
           age_45_49 = age40_44) %>%
    filter(Year >=2021) %>% 
    mutate(Year = as.character(Year)) %>% 
    mutate(across(starts_with("age"), ~ .x / 1000)) %>% 
    pivot_longer(
      cols = -Year,
      names_to = "age_group_5yr",
      values_to = "ons_asfr"
    ),
  ONS_projected_2048_2061 %>% 
    mutate(Year = as.character(Year))
)
  
  
projection_years = c(2026, 2031, 2036, 2041, 2046, 2051, 2056, 2061)

ONS_5yr = ONS_full_2021_2061 %>% 
  filter(Year %in% projection_years)


#since MSDS fx oi scalucalte by the avarage of multiple years, we do the same
ONS_2021_adjusted_match_MSDS =ONS_fertility_projection %>% rename(
  age_15_19  = `15 to 19`,
  age20_24  = `20 to 24`,
  age25_29  = `25 to 29`,
  age30_34  = `30 to 34`,
  age35_39  = `35 to 39`,
  age40_44  = `40 to 46`) %>% 
  mutate(across(everything(), as.numeric),
         age_45_49 = age40_44) %>%
  filter(Year %in% c(2022,2023,2024,2025)) %>% 
  mutate(Year = as.character(Year)) %>% 
  mutate(across(starts_with("age"), ~ .x / 1000)) %>% 
  pivot_longer(-Year, 
               names_to = "age_group_5yr", 
               values_to = "ons_asfr") %>% 
  group_by(age_group_5yr) %>% 
  summarise(ons_asfr = mean(ons_asfr), .groups = "drop") %>% 
  mutate(Year = "2021") %>% 
  select(Year, age_group_5yr,ons_asfr)


#ONS scaling factor 
ONS_scaling_factors = ONS_5yr %>% 
  bind_rows(ONS_2021_adjusted_match_MSDS) %>% 
  group_by(age_group_5yr) %>% 
  mutate(
    ons_2021 = ons_asfr[Year == 2021][1],
    ons_scaling_factor = ons_asfr / ons_2021
  ) %>% 
  ungroup() %>% 
  arrange(Year, age_group_5yr) %>% 
  mutate(age_group_5yr =case_when(
    age_group_5yr == "age_15_19" ~ "15-19",
    age_group_5yr == "age20_24"  ~ "20-24",
    age_group_5yr == "age25_29"  ~ "25-29",
    age_group_5yr == "age30_34"  ~ "30-34",
    age_group_5yr == "age35_39"  ~ "35-39",
    age_group_5yr == "age40_44"  ~ "40-44",
    age_group_5yr == "age_45_49" ~ "45-49",
    TRUE ~ age_group_5yr
  ))


#=============================================================
#apply the scaling factor to birmingham baesline 

bham_projected_asfr = MSDS_counts_final %>% 
  group_by(eth_code_mother, age_group_5yr) %>% 
  summarise(fertility_avg = mean(n_total), .groups = "drop") %>% 
  left_join(bham_eth_pop %>% filter(DEC_SEX == "Female"), by = c("eth_code_mother" = "eth_code", "age_group_5yr")) %>% 
  mutate(raw_rate = fertility_avg / Observation) %>% 
  group_by(age_group_5yr) %>% 
  summarise(
    # Calculate the global mean of the rates
    bham_baseline_asfr = mean(raw_rate, na.rm = TRUE),
    v  = pmax(var(raw_rate, na.rm = TRUE), 1e-10),
    .groups = "drop") %>% 
  crossing(Year = c(2021,2026, 2031, 2036, 2041, 2046, 2051, 2056, 2061))%>% 
  mutate(Year = as.character(Year))%>% 
  right_join(ONS_scaling_factors, by = c("Year", "age_group_5yr"))%>% 
  mutate(bham_scaled_asfr = bham_baseline_asfr*ons_scaling_factor) %>% 
  select(Year,age_group_5yr, ons_scaling_factor)



fertility_input_CCM =smoothed_data %>% 
  select(eth_code_mother,age_group_5yr,fx_EB_baseline) %>% 
  left_join(
    bham_projected_asfr %>% 
      select(Year, age_group_5yr,ons_scaling_factor
      ),
    by = "age_group_5yr") %>% 
  mutate(
    fx_EB_projected = fx_EB_baseline * ons_scaling_factor) %>% 
  arrange(
    eth_code_mother, age_group_5yr, Year) %>% 
  select(Year, eth_code_mother, age_group_5yr,fx = fx_EB_projected) 


write.csv(fertility_input_CCM, "data/processed/Birmingham_fertility_rates.csv")


