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

MSDS_data %>% count(YearOfBirthBaby)

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



#==========================================================
# new!
#since there are discrepancy between ons registered births and
#MSDS births, therefore we will have to obtain the ethnic share pattern
#and redistribute to ons births to maintain the total matching ons 
#==========================================================
nomis_Live_births_in_England_and_Wales = read_excel("data/fertiliy/nomis_Live births in England and Wales.xlsx", 
                                                     skip = 6)


ons_births_by_age = nomis_Live_births_in_England_and_Wales %>% 
  select(-`2021`) %>%
  filter(!`Age of mother` %in% c("Total",  "Age of mother unknown or not stated", "Mother aged under 18")) %>% 
  pivot_longer(
    cols = `2022`:`2025`,
    names_to = "year",
    values_to = "registered_births"
  ) %>%
  mutate(year = as.integer(year)) %>% 
  group_by(`Age of mother`) %>% 
  summarise(
    ons_births_avg =
      mean(registered_births),
    .groups = "drop"
  ) %>% 
  mutate(age_group_5yr = case_when(
    `Age of mother` == "Mother aged under 20"   ~ "15-19",
    `Age of mother` == "Mother aged 20-24"      ~ "20-24",
    `Age of mother` == "Mother aged 25-29"      ~ "25-29",
    `Age of mother` == "Mother aged 30-34"      ~ "30-34",
    `Age of mother` == "Mother aged 35-39"      ~ "35-39",
    `Age of mother` == "Mother aged 40-44"      ~ "40-44",
    `Age of mother` == "Mother aged 45 and over" ~ "45-49",
    TRUE                           ~ NA_character_)) %>% 
  select(
    age_group_5yr,
    ons_births_avg
  )
  
#------------------------------------------------
#obtain share 

msds_ethnic_shares_by_age = MSDS_counts_final %>% 
  group_by(eth_code_mother,age_group_5yr) %>% 
  summarise( msds_births_avg =mean(n_total, na.rm = TRUE),
            .groups = "drop")%>%
  group_by(age_group_5yr) %>% 
  mutate(
    ethnic_share_within_age =msds_births_avg / sum(msds_births_avg, na.rm = TRUE)
  ) %>%
  ungroup()
  

controlled_births_eth_age =msds_ethnic_shares_by_age %>% 
  left_join(ons_births_by_age, by = "age_group_5yr") %>% 
  mutate(
    controlled_births = ons_births_avg * ethnic_share_within_age
  )
  

MSDS_counts_final = controlled_births_eth_age %>% 
  select(age_group_5yr,eth_code_mother, n_total = controlled_births)



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
ONS_scaling_factors = ONS_full_2021_2061 %>%
  mutate(
    Year = as.integer(Year),
    
    age_group_5yr = case_when(
      age_group_5yr == "age_15_19" ~ "15-19",
      age_group_5yr == "age20_24"   ~ "20-24",
      age_group_5yr == "age25_29"   ~ "25-29",
      age_group_5yr == "age30_34"   ~ "30-34",
      age_group_5yr == "age35_39"   ~ "35-39",
      age_group_5yr == "age40_44"   ~ "40-44",
      age_group_5yr == "age_45_49"  ~ "45-49",
      TRUE ~ age_group_5yr
    )
  ) %>%
  group_by(age_group_5yr) %>%
  mutate(
    ons_asfr_2025 = ons_asfr[Year == 2025][1],
    
    ons_scaling_factor = case_when(
      Year <= 2025 ~ 1,
      Year >= 2026 ~ ons_asfr / ons_asfr_2025
    )
  ) %>%
  ungroup() %>%
  filter(Year >= 2021, Year <= 2061) %>%
  select(
    Year,
    age_group_5yr,
    ons_asfr,
    ons_asfr_2025,
    ons_scaling_factor
  ) %>% 
  mutate(Year = as.character(Year))


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

#========================================================================================
#turn the rates into a single year of age fertility rate by using the hadwiger function
# a = total fertility
# b = control the spread (higher values more narrow and centered)
# c = control the centre 
hadwiger = function(x, a, b, c) {
  (a * b / c) * (c / x)^1.5 * exp(-b^2 * (c/x + x/c - 2))
}


bands = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")

ages = seq(15, 49, 0.1)
plot(ages, hadwiger(ages, a = 2, b = 10, c = 28), type = "l")

age_lookup = tibble(single_age = 15:49) %>%
  mutate(age_group_5yr = bands[findInterval(single_age, seq(15, 50, 5))])

band_avg_from_params = function(a, b, c) {
  age_lookup %>%
    mutate(fx = hadwiger(single_age + 0.5, a, b, c)) %>%
    group_by(age_group_5yr) %>%
    summarise(fx_band = mean(fx), .groups = "drop")
}






#the observed fertility schedule

obs_pak = smoothed_data %>%
  filter(eth_code_mother == "PAK") %>%
  transmute(age_group_5yr = as.character(age_group_5yr),
            fx_obs = fx_EB_baseline)


mids = c(17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5)
start_a = 5 * sum(obs_pak$fx_obs)                          # TFR
start_c = sum(mids * obs_pak$fx_obs) / sum(obs_pak$fx_obs) # rate-weighted mean age
start_b = 3.5                                              #just a sensible initial value


predict_band_rates = function(a, b, c, band) {
  pred = band_avg_from_params(a, b, c)
  pred$fx_band[match(band, pred$age_group_5yr)]
}


predict_band_rates(start_a, start_b, start_c, obs_pak$age_group_5yr)




fit_pak = nls(
  fx_obs ~ predict_band_rates(a, b, c, age_group_5yr),
  data      = obs_pak,
  start     = list(a = start_a, b = start_b, c = start_c),
  algorithm = "port",
  lower     = c(a = 0.01, b = 0.5, c = 18),
  upper     = c(a = 10,   b = 10,  c = 42)
)


pred_pak = band_avg_from_params(
  coef(fit_pak)["a"],
  coef(fit_pak)["b"],
  coef(fit_pak)["c"]
)


pak_single_age = tibble(single_age = 15:49) %>%
  mutate(
    age_mid = single_age + 0.5,
    fx_single = hadwiger(
      age_mid,
      coef(fit_pak)["a"],
      coef(fit_pak)["b"],
      coef(fit_pak)["c"]
    )
  )

pak_single_age

ggplot(pak_single_age, aes(x = single_age, y = fx_single)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "PAK fitted single-year fertility rates",
    x = "Single year of age",
    y = "Annual fertility rate"
  ) +
  theme_minimal()




pak_corrected =pak_single_age %>%
  left_join(age_lookup, by = "single_age") %>%
  group_by(age_group_5yr) %>%
  summarise(band_mean = mean(fx_single)) %>%
  left_join(obs_pak, by = "age_group_5yr") %>%
  mutate(diff = band_mean - fx_obs,
         rel_diff = diff / fx_obs,
         correction = fx_obs/band_mean) %>% 
  select(age_group_5yr,correction) %>% 
  left_join(age_lookup, by = "age_group_5yr") %>% 
  left_join(pak_single_age, by = "single_age") %>% 
  select(single_age, fx_single, age_group_5yr, correction) %>% 
  mutate(corrected_fx_single = fx_single*correction) %>% 
  left_join(obs_pak,by = "age_group_5yr")
  


pak_corrected_plot = ggplot(pak_corrected, aes(x = single_age))+
  geom_line(aes(y = fx_obs, colour = "Grouped"),
            linewidth = 1) +
  geom_line(aes(y = corrected_fx_single, colour = "Single year"),
            linewidth = 1) +
  scale_color_manual(values = c("Single year" = "#D00070",
                                "Grouped"       = "#3c3c3b"))+
  scale_x_continuous(limits = c(15,49), breaks = seq(15,49,2))+
  labs(
    title = "Estimated singe year ASFRs from five year grouped ASFRs: \nPakistani women in Birmingham 2022-2025",
    x = "Single year of age",
    y = "Age-sepcific fertility rate"
  ) +
  labs(colour = "")+
  theme_minimal(base_size =12)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust=0.5))


#==================================================================================
#turn the above process into a function for the other ethnic group
#do a for loop as well

eth_names = c(
  WBI = "White British", WHO = "Other White", MIX = "Mixed",
  IND = "Indian",        PAK = "Pakistani",   BAN = "Bangladeshi",
  CHI = "Chinese",       OAS = "Other Asian", BLA = "Black African",
  BLC = "Black Caribbean", OBL = "Other Black", OTH = "Other ethnic group"
)

hadwiger = function(x, a, b, c) {
  (a * b / c) * (c / x)^1.5 * exp(-b^2 * (c/x + x/c - 2))
}


bands = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")

ages = seq(15, 49, 0.1)
plot(ages, hadwiger(ages, a = 2, b = 10, c = 28), type = "l")

age_lookup = tibble(single_age = 15:49) %>%
  mutate(age_group_5yr = bands[findInterval(single_age, seq(15, 50, 5))])

band_avg_from_params = function(a, b, c) {
  age_lookup %>%
    mutate(fx = hadwiger(single_age + 0.5, a, b, c)) %>%
    group_by(age_group_5yr) %>%
    summarise(fx_band = mean(fx), .groups = "drop")
}

#---------------------------------------------------------------------
hadwiger_nls = function(data,
                        eth_code = "PAK"){
  
  hadwiger = function(x, a, b, c) {
    (a * b / c) * (c / x)^1.5 * exp(-b^2 * (c/x + x/c - 2))
  }
  
  
  obs_eth= smoothed_data %>%
    filter(eth_code_mother == eth_code) %>%
    transmute(age_group_5yr = as.character(age_group_5yr),
              fx_obs = fx_EB_baseline)
    
  mids = c(17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5)
  start_a = 5 * sum(obs_eth$fx_obs)                          # TFR
  start_c = sum(mids *obs_eth$fx_obs) / sum(obs_eth$fx_obs) # rate-weighted mean age
  start_b = 3.5                                              #just a sensible initial value
  
  
  predict_band_rates = function(a, b, c, band) {
    pred = band_avg_from_params(a, b, c)
    pred$fx_band[match(band, pred$age_group_5yr)]
  }
  
  
  
  fit_eth = nls(
    fx_obs ~ predict_band_rates(a, b, c, age_group_5yr),
    data      = obs_eth,
    start     = list(a = start_a, b = start_b, c = start_c),
    algorithm = "port",
    lower     = c(a = 0.01, b = 0.5, c = 18),
    upper     = c(a = 10,   b = 10,  c = 42)
  )
  
  pred_eth = band_avg_from_params(
    coef(fit_eth)["a"],
    coef(fit_eth)["b"],
    coef(fit_eth)["c"]
  )
  
  
  eth_single_age = tibble(single_age = 15:49) %>%
    mutate(
      age_mid = single_age + 0.5,
      fx_single = hadwiger(
        age_mid,
        coef(fit_eth)["a"],
        coef(fit_eth)["b"],
        coef(fit_eth)["c"]
      ))
      
      
      
      
      eth_corrected =eth_single_age %>%
        left_join(age_lookup, by = "single_age") %>%
        group_by(age_group_5yr) %>%
        summarise(band_mean = mean(fx_single)) %>%
        left_join(obs_eth, by = "age_group_5yr") %>%
        mutate(diff = band_mean - fx_obs,
               rel_diff = diff / fx_obs,
               correction = fx_obs/band_mean) %>% 
        select(age_group_5yr,correction) %>% 
        left_join(age_lookup, by = "age_group_5yr") %>% 
        left_join(eth_single_age, by = "single_age") %>% 
        select(single_age, fx_single, age_group_5yr, correction) %>% 
        mutate(corrected_fx_single = fx_single*correction) %>% 
        left_join(obs_eth,by = "age_group_5yr")
      
      eth_corrected$eth_code = eth_code
      
      
      eth_label = if (eth_code %in% names(eth_names)) eth_names[[eth_code]] else eth_code
      
      eth_corrected_plot = ggplot(eth_corrected, aes(x = single_age))+
        geom_line(aes(y = fx_obs, colour = "Grouped"),
                  linewidth = 1) +
        geom_line(aes(y = corrected_fx_single, colour = "Single year"),
                  linewidth = 1) +
        scale_color_manual(values = c("Single year" = "#D00070",
                                      "Grouped"       = "#3c3c3b"))+
        scale_x_continuous(limits = c(15,49), breaks = seq(15,49,2))+
        labs(
          title = glue::glue("Estimated single-year ASFRs from five-year grouped ASFRs:\n{eth_label} women in Birmingham, 2022–2025"),
          x = "Single year of age",
          y = "Age-sepcific fertility rate"
        ) +
        labs(colour = "")+
        theme_minimal(base_size =12)+
        theme(legend.position = "bottom",
              plot.title = element_text(hjust=0.5))   
  
    
    
    return(list(data = eth_corrected, plot = eth_corrected_plot, params = coef(fit_eth)))
    
    
}

#---------------------------------------------------------------------
#loop over all ethnicity 
  
 all_single_year = list()
  
for (i in 1:length(eth_names)){
  eth_code = unique(smoothed_data$eth_code_mother)[i]
  all_single_year[[i]] = hadwiger_nls(data = smoothed_data, eth_code =eth_code)
  
}


tables = map(all_single_year, "data")           # list of 12 tibbles
stacked = data.table::rbindlist(tables, idcol = "eth_code_mother") %>% 
  select(-eth_code_mother) %>% 
  select(eth_code, everything())

#--------------------------------------------------------------------
# Project ethnicity-specific single-year ASFRs using the ONS TFR trajectory
#
# The baseline ethnic and age-specific ASFR schedules are retained.
# The same annual proportional adjustment is applied to every ethnicity and age.

ons_tfr_trajectory = tibble( Year = 2021:2061) %>% 
  mutate(
    ons_tfr = case_when(
      
      # Retain the pooled 2022–2025 Birmingham baseline
      Year <= 2025 ~ 1.40,
      
      # Gradual decline from 1.40 in 2025 to 1.38 in 2029
      Year <= 2029 ~ approx(
        x = c(2025, 2029),
        y = c(1.40, 1.38),
        xout = Year
      )$y,
      
      # Gradual increase from 1.38 in 2029 to 1.42 in 2049
      Year <= 2049 ~ approx(
        x = c(2029, 2049),
        y = c(1.38, 1.42),
        xout = Year
      )$y,
      
      # Hold the long-term assumption constant
      TRUE ~ 1.42
    ),
    
    tfr_scaling_factor = case_when(
      Year <= 2025 ~ 1,
      Year >= 2026 ~ ons_tfr / ons_tfr[Year == 2025]
    )
  )
  


fertility_projected_annual = stacked %>%
  select(eth_code,age_group_5yr,single_age,corrected_fx_single) %>%
  crossing(
    Year = 2021:2061) %>%
  left_join(ons_tfr_trajectory,by = "Year") %>%
  mutate(fx = corrected_fx_single * tfr_scaling_factor) %>%
  arrange(eth_code,single_age,Year) %>%
  select(Year,eth_code,age_group_5yr,single_age,fx,ons_tfr,tfr_scaling_factor
  )



fertility_projected_annual %>%
  filter(Year == 2021) %>%
  group_by(eth_code) %>%
  summarise(tfr = sum(fx)) %>%
  arrange(tfr) %>% 
  pull(tfr) %>% 
  mean()
# 
# fertility_projected_annual %>%
#   filter(Year == 2022) %>%
#   group_by(eth_code) %>%
#   summarise(tfr = sum(fx)) %>%
#   arrange(tfr)

write.csv(fertility_projected_annual, "data/processed/03_Birmingham_fertility_rates_single_year.csv")



#===============================================================
#make some plots

eth_order = c("WBI", "WHO", "MIX",           # White + Mixed
              "IND", "PAK", "BAN", "CHI", "OAS",  # Asian
              "BLA", "BLC", "OBL",           # Black
              "OTH")                          # Other

eth_lookup = tibble(eth_code = names(eth_names),
                    eth_name = unname(eth_names)) %>% 
  mutate(eth_name = factor(eth_name, levels = eth_names[eth_order]))

ASFR_stacked = stacked %>%
  left_join(eth_lookup, by = "eth_code") %>%
  ggplot(aes(x = single_age))+
  geom_line(aes(y = fx_obs, colour = "Grouped"),
            linewidth = 1) +
  geom_line(aes(y = corrected_fx_single, colour = "Single year"),
            linewidth = 1) +
  scale_color_manual(values = c("Single year" = "#D00070",
                                "Grouped"       = "#3c3c3b"))+
  scale_x_continuous(limits = c(15,49), breaks = seq(15,49,2))+
  labs(
    title = "Estimated singe year ASFRs from five year grouped ASFRs of women by ethnicity \nin Birmingham 2022-2025",
    x = "Single year of age",
    y = "Age-sepcific fertility rate",
    caption = "Rates pooled over 2022–2025 births (NHS MSDS) against mid-2021 Census female population.\nFive-year ASFRs transformed to single year of age via Hadwiger schedules."
  ) +
  labs(colour = "")+
  theme_minimal(base_size =14)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
        plot.caption = element_text(hjust=-0))+
  facet_wrap(~eth_name)

ggsave("fig/ASFR_Stacked.png", ASFR_stacked, dpi = 600)





stacked %>%
  left_join(eth_lookup, by = "eth_code") %>%
  ggplot(aes(x = single_age))+
  geom_line(aes(y = fx_obs, colour = "Grouped"),
            linewidth = 1) +
  geom_line(aes(y = corrected_fx_single, colour = "Single year"),
            linewidth = 1) +
  scale_color_manual(values = c("Single year" = "#D00070",
                                "Grouped"       = "#3c3c3b"))+
  scale_x_continuous(limits = c(15,49), breaks = seq(15,49,2))+
  labs(
    title = "Estimated singe year ASFRs from five year grouped ASFRs of women by ethnicity \nin Birmingham 2022-2025",
    x = "Single year of age",
    y = "Age-sepcific fertility rate",
    caption = "Rates pooled over 2022–2025 births (NHS MSDS) against mid-2021 Census female population.\nFive-year ASFRs transformed to single year of age via Hadwiger schedules."
  ) +
  labs(colour = "")+
  theme_minimal(base_size =14)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
        plot.caption = element_text(hjust=-0))+
  facet_wrap(~eth_name)



stacked %>%
  left_join(eth_lookup, by = "eth_code") %>%
  filter(eth_code %in% c("WBI", "CHI", "PAK", "IND", "BLA")) %>% 
  ggplot(aes(x = single_age,y = corrected_fx_single, colour = eth_code))+
  geom_line()+
  scale_colour_bcc(palette = "multi")+
  scale_x_continuous(limits = c(15,49), breaks = seq(15,49,2))+
  labs(
    title = "Estimated singe year ASFRs of women by ethnicity in Birmingham 2022-2025",
    x = "Single year of age",
    y = "Age-sepcific fertility rate",
    caption = "Rates pooled over 2022–2025 births (NHS MSDS) against mid-2021 Census female population.\nFive-year ASFRs transformed to single year of age via Hadwiger schedules."
  )+
  theme_bcc(legend_position = "bottom",
            gridline_x = F, # remove vertical gridlines
            gridline_y = F)+
  theme(plot.title = element_text(size = 15,
                                    color = bcc_cols("black")))




base = MSDS_data %>% filter(YearOfBirthBaby %in% 2022:2025)

tibble(
  step = c("raw", "ward ok", "ward + age 15-49", "eth known", "eth unknown"),
  n = c(nrow(base),
        sum(base$ElectoralWardMother %in% ward_map$Ward_Code),
        base %>% filter(ElectoralWardMother %in% ward_map$Ward_Code,
                        AgeRPEndDate >= 15, AgeRPEndDate <= 49) %>% nrow(),
        nrow(MSDS_data_filtered),
        nrow(MSDS_data_filtered_na))
) %>% mutate(per_year = n / 4)










