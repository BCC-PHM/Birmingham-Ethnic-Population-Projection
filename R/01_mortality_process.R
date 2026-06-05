setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")


library(tidyverse)
library(readr)
library(sf)

ward_map = st_read("data/boundaries/boundaries-wards-2022-birmingham/boundaries-wards-2022-birmingham.shp")


death_data = read_delim("data/death_data.txt", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)


age_levels = c("0-4","5-9","10-14","15-19","20-24","25-29",
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
                   DEC_AGEC >= 0  & DEC_AGEC <= 4  ~ "0-4",
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
  filter(year >= 2017, year <= 2024) %>%
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
            DEC_AGEC >= 0  & DEC_AGEC <= 4  ~ "0-4",
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
  
         
#----------------------------------------------
#death counts by ward x sex x age x year
#----------------------------------------------
death_counts_redis = Bham_only_death_data_na_eth %>% 
  mutate(year = year(DATE_OF_DEATH)) %>%
  filter(year >= 2017, year <= 2024) %>%
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
    year          = 2017:2024,
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
    Age >= 0  & Age <= 4  ~ "0-4",
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

##########################################################################
#==================================================================
#i found that there are too few deaths, some strata has 0 deaths_avg
#we need to use empirical bayes shrinkage

#calculate raw mortality rates
prior_deaths = death_counts_final %>% 
  group_by(eth_code, DEC_SEX, age_group_5yr) %>% 
  summarise(deaths_avg = mean(deaths_total), .groups = "drop") %>% 
  left_join(bham_eth_pop, by = c("eth_code", "DEC_SEX", "age_group_5yr")) %>% 
  mutate(raw_rate = deaths_avg / Observation) %>% 
  group_by(DEC_SEX, age_group_5yr) %>% 
  summarise(
    # Calculate the global mean of the rates
    mu = mean(raw_rate, na.rm = TRUE),
    v  = pmax(var(raw_rate, na.rm = TRUE), 1e-10),
    .groups = "drop")
  
#Calculate α and β
prior_deaths = prior_deaths %>% 
  mutate(alpha = mu^2 / v,
         beta  = mu / v)

#apply EB shrinkage
smoothed_data = death_counts_final %>% 
  group_by(eth_code, DEC_SEX, age_group_5yr) %>% 
  summarise(deaths_avg = mean(deaths_total), .groups = "drop") %>% 
  left_join(bham_eth_pop, by = c("eth_code", "DEC_SEX", "age_group_5yr")) %>% 
  left_join(prior_deaths, by = c("DEC_SEX", "age_group_5yr")) %>% 
  mutate(mx_EB =(deaths_avg + alpha) /(Observation  + beta))


################################################################################


#===============================================================================
#attempt for our birmingham own life tables
#===============================================================================
#Convert mx to qx  (probability of dying in the interval)

life_table = smoothed_data %>% 
  rename(mx = mx_EB) %>% 
  mutate(n =5,
         #ax is the average proportion of the age interval lived by those who die within that interval
         ax = case_when(age_group_5yr == "0-4" ~ 0.07,
                        age_group_5yr == "90+" ~ 1/mx,
                        TRUE ~ 2.5),
         #probability of dying in interval
         qx = case_when(
           age_group_5yr == "90+" ~ 1,   # everyone alive at 90+ will eventually die within the open-ended 90+ interval.
           TRUE                   ~ (n * mx) / (1 + (n - ax) * mx)
         ),
         
         # just for safety because probability cant exceed 1 
         qx = pmin(qx, 1)
         ) %>% 
  arrange(eth_code, DEC_SEX, age_group_5yr) %>% 
  group_by(eth_code, DEC_SEX) %>% 
  mutate(
    # lx: survivors at start of interval, radix = 100,000
    lx  = 100000 * cumprod(lag(1 - qx, default = 1)),
    # dx: deaths in interval
    dx  = lx * qx,
    # Lx: person-years lived in interval
    Lx  = case_when(
      age_group_5yr == "90+" ~ lx / mx,   # ONS open interval
      TRUE                   ~ n * (lx - dx) + ax * dx
    )
  ) %>% 
  mutate(
    # Tx: person-years lived above age x
    Tx  = rev(cumsum(rev(Lx))),
    # ex: life expectancy at age x
    ex  = Tx / lx
  ) %>% 
  ungroup()


survival_ratios  = life_table %>%
  group_by(eth_code, DEC_SEX) %>%
  mutate(
    Lx_next = lead(Lx),
    Tx_90 = Tx[age_group_5yr == "90+"],
    Tx_85 = Tx[age_group_5yr == "85-89"],
    
    Sx = case_when(
      age_group_5yr == "85-89" ~ Tx_90 / Tx_85,
      age_group_5yr == "90+"   ~ Tx_90 / Tx_85,
      TRUE ~ Lx_next / Lx
    ),
    
    Sx = pmin(Sx, 1)
  ) %>%
  ungroup()



write.csv(survival_ratios, "data/processed/Birmingham_survival_ratios.csv")




