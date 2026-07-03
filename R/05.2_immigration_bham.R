setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 05.2_international_immigration_demo_uk.R
# process the publicly available international immigration 
# using ethnic composition and redistribute for the rest of UK
#to support the use of bidirectional model
# ============================================================
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)

#-------------------------------------------------------------
#Bimringham international immigration broad ethnic profile from the
#internal in migration data where the code is 99999999
ODMG03EW_LTLA = read_csv("data/migration/ODMG03EW_LTLA.csv")


colnames(ODMG03EW_LTLA) = gsub(" ", "_", colnames(ODMG03EW_LTLA))


international_in = ODMG03EW_LTLA %>% 
  filter(
    Lower_tier_local_authorities_label == "Birmingham",
    Migrant_LTLA_one_year_ago_code %in% c("999999999")
  ) %>%
  group_by(Lower_tier_local_authorities_code, `Ethnic_group_(6_categories)_label`) %>%
  summarise(count = sum(Count), .groups = "drop") %>% 
  select(ethnic_group = `Ethnic_group_(6_categories)_label`, in_ = count)

#============================================================
# Option 1: propensity borrowing
# Disaggregate 5-group internal migration to 12 NEWETHPOP groups
# using Birmingham 2021 Census population shares as proxy propensities
# ============================================================
birmingham_base_pop_12grp = read_csv("data/processed/birmingham_base_pop_12grp.csv")
#map the 12 harmonised group to its broad 5 category 

broad_group_map = tibble(
  eth_code    = c("WBI","WHO","MIX",
                  "IND","PAK","BAN","CHI","OAS",
                  "BLA","BLC","OBL",
                  "OTH"),
  ethnic_group = c(
    "White","White","Mixed or Multiple ethnic groups",
    "Asian, Asian British or Asian Welsh",
    "Asian, Asian British or Asian Welsh",
    "Asian, Asian British or Asian Welsh",
    "Asian, Asian British or Asian Welsh",
    "Asian, Asian British or Asian Welsh",
    "Black, Black British, Black Welsh, Caribbean or African",
    "Black, Black British, Black Welsh, Caribbean or African",
    "Black, Black British, Black Welsh, Caribbean or African",
    "Other ethnic group"
  )
)

#---------------------------------------------------
#the propensity of small subgroup in each broad group

propensity = birmingham_base_pop_12grp %>%
  left_join(broad_group_map, by = "eth_code") %>%
  group_by(ethnic_group) %>%
  mutate(
    broad_pop  = sum(pop_2021),
    prop       = pop_2021 / broad_pop      # share within broad group
  ) %>%
  ungroup()

propensity


#---------------------------------------------------
#multiply the preopensity 


international_in_12grp = propensity %>%
  left_join(
    international_in %>% select(ethnic_group, in_),
    by = "ethnic_group"
  ) %>%
  mutate(
    in_12   = round(in_ * prop) 
  ) %>%
  select(eth_code, ethnic_group, pop_2021, prop, in_12) %>%
  mutate(
    eth_code = factor(eth_code,
                      levels = c("WBI","WHO","MIX","IND","PAK","BAN",
                                 "CHI","OAS","BLA","BLC","OBL","OTH"))
  ) %>%
  arrange(eth_code)


#---------------------------------------------------
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/internationalmigrationz7
# international immigration age sex count 

X2022_SNPP_International_in_females <- read_csv("data/migration/2022 SNPP International in females.csv")

X2022_SNPP_International_in_males <- read_csv("data/migration/2022 SNPP International in males.csv")

age_levels = c("0-4","5-9","10-14","15-19","20-24","25-29",
               "30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80-84","85-89","90+")


intl_immig_female = X2022_SNPP_International_in_females %>% 
  filter(AREA_NAME == "Birmingham",
         AGE_GROUP != "All ages") %>% 
  select(AREA_CODE, AREA_NAME, COMPONENT,
         SEX, AGE_GROUP, in_count = `2023`) %>% 
  mutate( Age = if_else(AGE_GROUP == "90 and over", "90", AGE_GROUP),
          Age = as.numeric(Age),
          age_group_5yr = case_when(
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
                                 levels = age_levels))%>%
  group_by(SEX, age_group_5yr) %>%
  summarise(in_count = sum(in_count), .groups = "drop") %>%
  group_by(SEX) %>%
  mutate(in_weight  = in_count  / mean(in_count)) %>%
  ungroup()




intl_immig_male = X2022_SNPP_International_in_males %>% 
  filter(AREA_NAME == "Birmingham",
         AGE_GROUP != "All ages") %>% 
  select(AREA_CODE, AREA_NAME, COMPONENT,
         SEX, AGE_GROUP, in_count = `2023`) %>% 
  mutate( Age = if_else(AGE_GROUP == "90 and over", "90", AGE_GROUP),
          Age = as.numeric(Age),
          age_group_5yr = case_when(
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
                                 levels = age_levels))%>%
  group_by(SEX, age_group_5yr) %>%
  summarise(in_count = sum(in_count), .groups = "drop") %>%
  group_by(SEX) %>%
  mutate(in_weight  = in_count  / mean(in_count)) %>%
  ungroup()



intl_immig_schedule = bind_rows(intl_immig_male,
                                intl_immig_female)



international_in_agesex = intl_immig_schedule %>% 
  mutate(sex = if_else(SEX == "males", "Male", "Female")) %>%
  select(sex, age_group_5yr, in_weight) %>%
  cross_join(
    international_in_12grp %>% select(eth_code, in_12)
  ) %>% 
  group_by(eth_code) %>%
  mutate(
    # convert weights to shares that sum to 1 across all 38 age-sex cells
    in_share = in_weight / sum(in_weight),
    # distribute the ethnic total across age-sex cells
    immig_count = in_12 * in_share
  ) %>% 
  mutate(
    eth_code = factor(eth_code,
                      levels = c("WBI","WHO","MIX","IND","PAK","BAN",
                                 "CHI","OAS","BLA","BLC","OBL","OTH")),
    age_group_5yr = factor(age_group_5yr, levels = age_levels)
  ) %>%
  arrange(eth_code, sex, age_group_5yr)

# Sense check: each ethnic group's distributed total = its in_12
international_in_agesex %>%
  group_by(eth_code) %>%
  summarise(check = round(sum(immig_count)), .groups = "drop") %>%
  left_join(international_in_12grp %>% select(eth_code, in_12), by = "eth_code") %>%
  mutate(diff = check - in_12)


