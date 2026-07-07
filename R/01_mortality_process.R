setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 01_mortality_process.R
# process death register data (joined with demographic data)
# apply empirical bayes shrinkage 
# obtain the survival ratios 
# ============================================================

library(tidyverse)
library(readr)
library(sf)
library(readxl)

ward_map = st_read("data/boundaries/boundaries-wards-2022-birmingham/boundaries-wards-2022-birmingham.shp")


death_data = read_delim("data/mortality/death_data.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)


age_levels = c("0","1-4","5-9","10-14","15-19","20-24","25-29",
                "30-34","35-39","40-44","45-49","50-54","55-59",
                "60-64","65-69","70-74","75-79","80-84","85-89","90+")

eth_pop = read_csv("data/RM032_LA_ethnic_pop_age_sex_2021.csv")

#=========================================================================
#group the ethnicity to the newethopop harmonised ethnic group
#=========================================================================

Bham_only_death_data = death_data %>% 
          filter(WARD_OF_RESIDENCE_CODE %in% ward_map$Ward_Code) %>% 
          mutate( DATE_OF_BIRTH = as.Date(paste0(DATE_OF_BIRTH, "-01")),
                  DATE_OF_DEATH = as.Date(paste0(DATE_OF_DEATH, "-01")),
                 DEC_SEX = ifelse(DEC_SEX == 1, "Male", "Female"),
                 age_group_5yr = case_when(
                   DEC_AGEC == 0                    ~ "0",
                   DEC_AGEC >= 1  & DEC_AGEC <= 4  ~ "1-4",
                   DEC_AGEC >= 5  & DEC_AGEC <= 9  ~ "5-9",
                   DEC_AGEC >= 10 & DEC_AGEC <= 14 ~ "10-14",
                   DEC_AGEC >= 15 & DEC_AGEC <= 19 ~ "15-19",
                   DEC_AGEC >= 20 & DEC_AGEC <= 24 ~ "20-24",
                   DEC_AGEC >= 25 & DEC_AGEC <= 29 ~ "25-29",
                   DEC_AGEC >= 30 & DEC_AGEC <= 34 ~ "30-34",
                   DEC_AGEC >= 35 & DEC_AGEC <= 39 ~ "35-39",
                   DEC_AGEC >= 40 & DEC_AGEC <= 44 ~ "40-44",
                   DEC_AGEC >= 45 & DEC_AGEC <= 49 ~ "45-49",
                   DEC_AGEC >= 50 & DEC_AGEC <= 54 ~ "50-54",
                   DEC_AGEC >= 55 & DEC_AGEC <= 59 ~ "55-59",
                   DEC_AGEC >= 60 & DEC_AGEC <= 64 ~ "60-64",
                   DEC_AGEC >= 65 & DEC_AGEC <= 69 ~ "65-69",
                   DEC_AGEC >= 70 & DEC_AGEC <= 74 ~ "70-74",
                   DEC_AGEC >= 75 & DEC_AGEC <= 79 ~ "75-79",
                   DEC_AGEC >= 80 & DEC_AGEC <= 84 ~ "80-84",
                   DEC_AGEC >= 85 & DEC_AGEC <= 89 ~ "85-89",
                   DEC_AGEC >= 90             ~ "90+",
                   TRUE ~ NA_character_
                 )) %>% 
          mutate( eth_newethpop = case_when(
            Ethnic_Description_National %in% c("British", "Irish")                          ~ "WBI",
            Ethnic_Description_National == "Any other white background"                     ~ "WHO",
            Ethnic_Description_National %in% c("White and Black Caribbean",
                                               "White and Black African",
                                               "White and Asian",
                                               "Any other mixed background")                ~ "MIX",
            Ethnic_Description_National == "Indian"                                         ~ "IND",
            Ethnic_Description_National == "Pakistani"                                      ~ "PAK",
            Ethnic_Description_National == "Bangladeshi"                                    ~ "BAN",
            Ethnic_Description_National == "Chinese"                                        ~ "CHI",
            Ethnic_Description_National == "Any other Asian background"                     ~ "OAS",
            Ethnic_Description_National == "African"                                        ~ "BLA",
            Ethnic_Description_National == "Caribbean"                                      ~ "BLC",
            Ethnic_Description_National == "Any other Black background"                     ~ "OBL",
            Ethnic_Description_National == "Any other ethnic group"                         ~ "OTH",
            Ethnic_Description_National == "NULL"                                           ~ NA_character_,
            .default = NA_character_
          ),
          eth_code = factor(
            eth_newethpop,
            levels = c("WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH")
          ),
          age_group_5yr = factor(age_group_5yr,
                                 levels = age_levels)) %>% 
  filter(!is.na(eth_code))
  

#===============================================
#death counts by eth x sex x age x year
#===============================================

death_counts = Bham_only_death_data %>% 
  mutate(year = year(DATE_OF_DEATH)) %>%
  filter(year %in% c(2022,2023,2024)) %>%
  group_by(eth_code, DEC_SEX, age_group_5yr, year) %>%
  summarise(deaths = n(), .groups = "drop")




#=========================================================================
#use proportion redistribution for missing ethnicity 
#seems to be what newethpop also used
#=========================================================================

Bham_only_death_data_na_eth = death_data %>% 
  filter(WARD_OF_RESIDENCE_CODE %in% ward_map$Ward_Code) %>% 
  mutate( DATE_OF_BIRTH = as.Date(paste0(DATE_OF_BIRTH, "-01")),
          DATE_OF_DEATH = as.Date(paste0(DATE_OF_DEATH, "-01")),
          DEC_SEX = ifelse(DEC_SEX == 1, "Male", "Female"),
          age_group_5yr = case_when(
            DEC_AGEC == 0                    ~ "0",
            DEC_AGEC >= 1  & DEC_AGEC <= 4  ~ "1-4",
            DEC_AGEC >= 5  & DEC_AGEC <= 9  ~ "5-9",
            DEC_AGEC >= 10 & DEC_AGEC <= 14 ~ "10-14",
            DEC_AGEC >= 15 & DEC_AGEC <= 19 ~ "15-19",
            DEC_AGEC >= 20 & DEC_AGEC <= 24 ~ "20-24",
            DEC_AGEC >= 25 & DEC_AGEC <= 29 ~ "25-29",
            DEC_AGEC >= 30 & DEC_AGEC <= 34 ~ "30-34",
            DEC_AGEC >= 35 & DEC_AGEC <= 39 ~ "35-39",
            DEC_AGEC >= 40 & DEC_AGEC <= 44 ~ "40-44",
            DEC_AGEC >= 45 & DEC_AGEC <= 49 ~ "45-49",
            DEC_AGEC >= 50 & DEC_AGEC <= 54 ~ "50-54",
            DEC_AGEC >= 55 & DEC_AGEC <= 59 ~ "55-59",
            DEC_AGEC >= 60 & DEC_AGEC <= 64 ~ "60-64",
            DEC_AGEC >= 65 & DEC_AGEC <= 69 ~ "65-69",
            DEC_AGEC >= 70 & DEC_AGEC <= 74 ~ "70-74",
            DEC_AGEC >= 75 & DEC_AGEC <= 79 ~ "75-79",
            DEC_AGEC >= 80 & DEC_AGEC <= 84 ~ "80-84",
            DEC_AGEC >= 85 & DEC_AGEC <= 89 ~ "85-89",
            DEC_AGEC >= 90             ~ "90+",
            TRUE ~ NA_character_
          )) %>% 
  mutate( eth_newethpop = case_when(
    Ethnic_Description_National %in% c("British", "Irish")                          ~ "WBI",
    Ethnic_Description_National == "Any other white background"                     ~ "WHO",
    Ethnic_Description_National %in% c("White and Black Caribbean",
                                       "White and Black African",
                                       "White and Asian",
                                       "Any other mixed background")                ~ "MIX",
    Ethnic_Description_National == "Indian"                                         ~ "IND",
    Ethnic_Description_National == "Pakistani"                                      ~ "PAK",
    Ethnic_Description_National == "Bangladeshi"                                    ~ "BAN",
    Ethnic_Description_National == "Chinese"                                        ~ "CHI",
    Ethnic_Description_National == "Any other Asian background"                     ~ "OAS",
    Ethnic_Description_National == "African"                                        ~ "BLA",
    Ethnic_Description_National == "Caribbean"                                      ~ "BLC",
    Ethnic_Description_National == "Any other Black background"                     ~ "OBL",
    Ethnic_Description_National == "Any other ethnic group"                         ~ "OTH",
    Ethnic_Description_National == "NULL"                                           ~ NA_character_,
    .default = NA_character_
  ),
  eth_code = factor(
    eth_newethpop,
    levels = c("WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH")
  )) %>% 
  filter(is.na(eth_code))




#------------------------------------------------------
#load the composition data 
#------------------------------------------------------
ward_ethnic_composition = read_excel("data/TS021-2021-ward_level_ethnic_composition.xlsx")


bham_ward_ethnic_composition = ward_ethnic_composition %>% 
  mutate(
    eth_newethpop = case_when(
      `Ethnic group (20 categories)` %in% c(
        "White: English, Welsh, Scottish, Northern Irish or British",
        "White: Irish",
        "White: Gypsy or Irish Traveller",
        "White: Roma"
      )                                                                          ~ "WBI",
      
      `Ethnic group (20 categories)` == "White: Other White"                    ~ "WHO",
      
      `Ethnic group (20 categories)` %in% c(
        "Mixed or Multiple ethnic groups: White and Black Caribbean",
        "Mixed or Multiple ethnic groups: White and Black African",
        "Mixed or Multiple ethnic groups: White and Asian",
        "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups"
      )                                                                          ~ "MIX",
      
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Indian"       ~ "IND",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Pakistani"    ~ "PAK",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Bangladeshi"  ~ "BAN",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Chinese"      ~ "CHI",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Other Asian"  ~ "OAS",
      
      `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: African"    ~ "BLA",
      `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: Caribbean"  ~ "BLC",
      `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: Other Black" ~ "OBL",
      
      `Ethnic group (20 categories)` %in% c(
        "Other ethnic group: Arab",
        "Other ethnic group: Any other ethnic group"
      )                                                                          ~ "OTH",
      
      `Ethnic group (20 categories)` == "Does not apply"                        ~ NA_character_,
      .default = NA_character_
    ),
    
    eth_code = factor(
      eth_newethpop,
      levels = c("WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH")
    )
  ) %>% 
  transmute(Ward_code = `Electoral wards and divisions Code`,
         Ward_name = `Electoral wards and divisions`,
         Ethnic20group = `Ethnic group (20 categories)`,
         eth_newethpop = eth_newethpop,
         eth_code = eth_code,
         Observation = Observation) %>% 
  filter(Ward_code  %in% ward_map$Ward_Code, Ethnic20group != "Does not apply") %>% 
  group_by(Ward_code , Ward_name,eth_newethpop,eth_code ) %>% 
  summarise(Observation = sum(Observation),
            .groups = "drop") %>% 
  group_by(Ward_code , Ward_name) %>% 
  mutate(Ward_pop = sum(Observation),
         Per_com = Observation/Ward_pop)
  


# write.csv(bham_ward_ethnic_composition, "data/processed/bham_ward_ethnic_composition.csv")
         
#----------------------------------------------
#death counts by ward x sex x age x year
#----------------------------------------------
death_counts_redis = Bham_only_death_data_na_eth %>% 
  mutate(year = year(DATE_OF_DEATH)) %>%
  filter(year %in% c(2022,2023,2024)) %>%
  group_by(WARD_OF_RESIDENCE_CODE, DEC_SEX, age_group_5yr, year) %>%
  summarise(deaths = n(), .groups = "drop")



#----------------------------------------------
#cross join counts with ward proportions and allocate
#----------------------------------------------

redistributed = death_counts_redis %>% 
  left_join(bham_ward_ethnic_composition, by = c("WARD_OF_RESIDENCE_CODE" = "Ward_code" ),relationship = "many-to-many") %>% 
  mutate(deaths_redistributed = deaths * Per_com) %>% 
  group_by(eth_code, DEC_SEX, age_group_5yr, year) %>% 
  summarise(deaths = sum(deaths_redistributed), .groups = "drop")



#==========================================================
# add redistributed deaths back into main counts
#==========================================================
death_counts_final = death_counts %>% 
  left_join(redistributed, 
            by = c("eth_code", "DEC_SEX", "age_group_5yr", "year"),
            suffix = c("_observed", "_redistributed")) %>% 
  mutate(
    deaths_redistributed = replace_na(deaths_redistributed, 0),
    deaths_total = deaths_observed + deaths_redistributed
  )


death_counts_final = death_counts_final %>% 
  complete(
    eth_code      = levels(Bham_only_death_data$eth_code),
    DEC_SEX       = c("Male", "Female"),
    age_group_5yr = age_levels,
    year          = c(2022,2023,2024),
    fill          = list(deaths_observed = 0, deaths_redistributed = 0, deaths_total = 0)
  ) %>% 
  mutate(age_group_5yr = factor(age_group_5yr, levels = age_levels))

###########################################################################################
#clean the ethpop to bham only

bham_eth_pop = eth_pop %>% 
  filter(`Upper tier local authorities` == "Birmingham") %>% 
  mutate(
    eth_newethpop = case_when(
      `Ethnic group (20 categories)` %in% c(
        "White: English, Welsh, Scottish, Northern Irish or British",
        "White: Irish",
        "White: Gypsy or Irish Traveller",
        "White: Roma"
      )                                                                          ~ "WBI",
      
      `Ethnic group (20 categories)` == "White: Other White"                    ~ "WHO",
      
      `Ethnic group (20 categories)` %in% c(
        "Mixed or Multiple ethnic groups: White and Black Caribbean",
        "Mixed or Multiple ethnic groups: White and Black African",
        "Mixed or Multiple ethnic groups: White and Asian",
        "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups"
      )                                                                          ~ "MIX",
      
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Indian"       ~ "IND",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Pakistani"    ~ "PAK",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Bangladeshi"  ~ "BAN",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Chinese"      ~ "CHI",
      `Ethnic group (20 categories)` == "Asian, Asian British or Asian Welsh: Other Asian"  ~ "OAS",
      
      `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: African"    ~ "BLA",
      `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: Caribbean"  ~ "BLC",
      `Ethnic group (20 categories)` == "Black, Black British, Black Welsh, Caribbean or African: Other Black" ~ "OBL",
      
      `Ethnic group (20 categories)` %in% c(
        "Other ethnic group: Arab",
        "Other ethnic group: Any other ethnic group"
      )                                                                          ~ "OTH",
      
      `Ethnic group (20 categories)` == "Does not apply"                        ~ NA_character_,
      .default = NA_character_
    ),
    
    eth_code = factor(
      eth_newethpop,
      levels = c("WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH")
    )
  ) %>% 
  transmute(
            Ethnic20group = `Ethnic group (20 categories)`,
            DEC_SEX = `Sex (2 categories)`,
            Age = `Age (101 categories) Code`,
            eth_newethpop = eth_newethpop,
            eth_code = eth_code,
            Observation = Observation) %>% 
  filter(Ethnic20group != "Does not apply") %>% 
  mutate( age_group_5yr = case_when(
    Age >= 1  & Age <= 4  ~ "1-4",
    Age >= 5  & Age <= 9  ~ "5-9",
    Age >= 10 & Age <= 14 ~ "10-14",
    Age >= 15 & Age <= 19 ~ "15-19",
    Age >= 20 & Age <= 24 ~ "20-24",
    Age >= 25 & Age <= 29 ~ "25-29",
    Age >= 30 & Age <= 34 ~ "30-34",
    Age >= 35 & Age <= 39 ~ "35-39",
    Age >= 40 & Age <= 44 ~ "40-44",
    Age >= 45 & Age <= 49 ~ "45-49",
    Age >= 50 & Age <= 54 ~ "50-54",
    Age >= 55 & Age <= 59 ~ "55-59",
    Age >= 60 & Age <= 64 ~ "60-64",
    Age >= 65 & Age <= 69 ~ "65-69",
    Age >= 70 & Age <= 74 ~ "70-74",
    Age >= 75 & Age <= 79 ~ "75-79",
    Age >= 80 & Age <= 84 ~ "80-84",
    Age >= 85 & Age <= 89 ~ "85-89",
    Age >= 90             ~ "90+",
    TRUE ~ NA_character_
  ),
  age_group_5yr = factor(age_group_5yr,
                         levels = age_levels)
  ) %>% 
  group_by(eth_code,DEC_SEX,age_group_5yr) %>% 
  summarise(Observation = sum(Observation),
            .groups = "drop")

#=======================================================================
#special treatment for age 0


MSDS_data = read_delim("data/fertiliy/Fertility_data.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)


births_eth_sex_avgpop = MSDS_data %>% 
  filter(YearOfBirthBaby %in% c(2022,2023,2024)) %>% 
  filter(ElectoralWardMother %in% ward_map$Ward_Code)%>%
  mutate(eth_code_baby = case_when(
    EthnicCategoryBaby %in% c("A", "B")             ~ "WBI",
    EthnicCategoryBaby == "C"                        ~ "WHO",
    EthnicCategoryBaby %in% c("D", "E", "F", "G")  ~ "MIX",
    EthnicCategoryBaby == "H"                        ~ "IND",
    EthnicCategoryBaby == "J"                        ~ "PAK",
    EthnicCategoryBaby == "K"                        ~ "BAN",
    EthnicCategoryBaby == "R"                        ~ "CHI",
    EthnicCategoryBaby == "L"                        ~ "OAS",
    EthnicCategoryBaby == "N"                        ~ "BLA",
    EthnicCategoryBaby == "M"                        ~ "BLC",
    EthnicCategoryBaby == "P"                        ~ "OBL",
    EthnicCategoryBaby == "S"                        ~ "OTH",
    EthnicCategoryBaby %in% c("Z", "99")            ~ NA_character_
  ),
  Sex = case_when(
    PersonPhenSex == "1" ~ "Male",
    PersonPhenSex == "2" ~ "Female",
    TRUE                 ~ NA_character_
  )) %>% 
  filter(!is.na(eth_code_baby)) %>% 
  group_by(YearOfBirthBaby,eth_code_baby,Sex)    %>% 
  summarise(births = n(), .groups = "drop") %>% 
  group_by(eth_code_baby,Sex ) %>% 
  summarise(
    births_avg = mean(births),
    .groups = "drop"
  )%>%
  filter(!is.na(Sex)) %>%
  transmute(
    eth_code = eth_code_baby,
    DEC_SEX = Sex,
    age_group_5yr=0,
    Observation = births_avg
  )

bham_eth_pop = rbind(births_eth_sex_avgpop,
                     bham_eth_pop) %>% 
  mutate(
    age_group_5yr = as.character(age_group_5yr),
    age_group_5yr = factor(age_group_5yr, levels = age_levels)
  )



# write.csv(bham_eth_pop, "data/processed/bham_eth_pop.csv")

##########################################################################
#==================================================================
#i found that there are too few deaths, some strata has 0 deaths_avg
#we need to use empirical bayes shrinkage

#calculate raw mortality rates
eth_mort_raw = death_counts_final %>% 
  group_by(eth_code, DEC_SEX, age_group_5yr) %>% 
  summarise(
    deaths_avg = mean(deaths_total, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  left_join(
    bham_eth_pop,
    by = c("eth_code", "DEC_SEX", "age_group_5yr")
  ) %>% 
  mutate(
    raw_rate = if_else(
      Observation > 0,
      deaths_avg / Observation,
      NA_real_
    )
  )

prior_deaths_weighted = eth_mort_raw %>%
  filter(
    is.finite(raw_rate),
    is.finite(Observation),
    Observation > 0
  ) %>% 
  group_by(DEC_SEX, age_group_5yr) %>%
  mutate(
    weight = Observation / sum(Observation, na.rm = TRUE),
    
    # population-weighted Birmingham sex-age rate
    mu = sum(weight * raw_rate, na.rm = TRUE)
  ) %>%
  summarise(
    total_deaths = sum(deaths_avg, na.rm = TRUE),
    total_exposure = sum(Observation, na.rm = TRUE),
    
    mu = first(mu),
    
    # population-weighted variance of ethnic raw rates
    v = sum(weight * (raw_rate - mu)^2, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    v = pmax(v, 1e-10),
    #Calculate α and β
    alpha = mu^2 / v,
    beta  = mu / v
  )


#apply EB shrinkage
smoothed_data = death_counts_final %>% 
  group_by(eth_code, DEC_SEX, age_group_5yr) %>% 
  summarise(deaths_avg = mean(deaths_total), .groups = "drop") %>% 
  left_join(bham_eth_pop, by = c("eth_code", "DEC_SEX", "age_group_5yr")) %>% 
  left_join(prior_deaths_weighted, by = c("DEC_SEX", "age_group_5yr")) %>% 
  mutate(mx_EB =(deaths_avg + alpha) /(Observation  + beta))


################################################################################


#===============================================================================
#attempt for our birmingham own life tables
#===============================================================================
#Convert mx to qx  (probability of dying in the interval)



life_table = smoothed_data %>% 
  mutate(# interval width
    n = case_when(
      age_group_5yr == "0"   ~ 1,
      age_group_5yr == "1-4" ~ 4,
      age_group_5yr == "90+" ~ NA_real_,
      TRUE                   ~ 5
    ),
         #ax is the average proportion of the age interval lived by those who die within that interval
         ax = case_when( age_group_5yr == "0"   ~ 0.07,
                         age_group_5yr == "1-4" ~ 2,
                         age_group_5yr == "90+" ~ NA_real_,
                         TRUE                   ~ n / 2),
         #probability of dying in interval
         qx = case_when(
           age_group_5yr == "0"   ~ mx_EB,
           age_group_5yr == "90+" ~ 1,
           TRUE                   ~ (n * mx_EB) / (1 + (n - ax) * mx_EB)
         ),
         
         # just for safety because probability cant exceed 1 
         qx = pmin(qx, 1),
    # create true mx column
    mx = case_when(
      age_group_5yr == "0" ~ qx / (1 - (1 - ax) * qx),
      TRUE                 ~ mx_EB
         ),
    px = 1 - qx,
    age_group_5yr = factor(age_group_5yr, levels = age_levels)) %>% 
  arrange(eth_code, DEC_SEX, age_group_5yr) %>% 
  group_by(eth_code, DEC_SEX) %>% 
  mutate(
    lx = 100000 * cumprod(lag(px, default = 1)),
    dx = lx * qx,
    lx_next = lead(lx),
    
    Lx = case_when(
      age_group_5yr == "90+" ~ lx / mx,
      TRUE                   ~ n * lx_next + ax * dx
    ),
    
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  ) %>% 
  ungroup()


# survival_ratios  = life_table %>%
#   group_by(eth_code, DEC_SEX) %>%
#   arrange(age_group_5yr, .by_group = TRUE) %>%
#   mutate(
#     Lx_next = lead(Lx),
#     Tx_90 = Tx[age_group_5yr == "90+"],
#     Tx_85 = Tx[age_group_5yr == "85-89"],
#     
#     Sx = case_when(
#       age_group_5yr == "85-89" ~ Tx_90 / Tx_85,
#       age_group_5yr == "90+"   ~ Tx_90 / Tx_85,
#       TRUE                     ~ Lx_next / Lx
#     ),
#     
#     Sx = pmin(Sx, 1)
#   ) %>%
#   ungroup()


write.csv(life_table, "data/processed/life_table.csv")
write.csv(survival_ratios, "data/processed/Birmingham_survival_ratios.csv")































