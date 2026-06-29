setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 05.1_migration_demo_uk.R
# process the publicly available migration 
# using ethnic composition and redistribute for the rest of UK
#to support the use of bidirectional model
# ============================================================
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)


# ============================================================
#England and Whales to be precise

uk_ethnic_composition = read_excel("~/R projects/PHM/BCC ethnic population projection/data/migration/nomis_2026_06_23_UK_ethnic_composition.xlsx", 
                                            skip = 9) %>%
  # Rename the auto-generated column name to 'Ethnic Group'
  rename(`Ethnic Group` = `...1`)



uk_ethnic_composition = uk_ethnic_composition %>%
  mutate(
    eth_code = case_when(
      # White British & Irish 
      `Ethnic Group` %in% c("White: English, Welsh, Scottish, Northern Irish or British", 
                            "White: Irish", 
                            "White: Gypsy or Irish Traveller", 
                            "White: Roma") ~ "WBI",
      
      # White Other 
      `Ethnic Group` == "White: Other White" ~ "WHO",
      
      # Mixed 
      `Ethnic Group` %in% c("Mixed or Multiple ethnic groups: White and Black Caribbean",
                            "Mixed or Multiple ethnic groups: White and Black African",
                            "Mixed or Multiple ethnic groups: White and Asian",
                            "Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups") ~ "MIX",
      
      # Asian, Asian British or Asian Welsh 
      `Ethnic Group` == "Asian, Asian British or Asian Welsh: Indian" ~ "IND",
      `Ethnic Group` == "Asian, Asian British or Asian Welsh: Pakistani" ~ "PAK",
      `Ethnic Group` == "Asian, Asian British or Asian Welsh: Bangladeshi" ~ "BAN",
      `Ethnic Group` == "Asian, Asian British or Asian Welsh: Chinese" ~ "CHI",
      `Ethnic Group` == "Asian, Asian British or Asian Welsh: Other Asian" ~ "OAS",
      
      # Black, Black British, Black Welsh, Caribbean or African 
      `Ethnic Group` == "Black, Black British, Black Welsh, Caribbean or African: African" ~ "BLA",
      `Ethnic Group` == "Black, Black British, Black Welsh, Caribbean or African: Caribbean" ~ "BLC",
      `Ethnic Group` == "Black, Black British, Black Welsh, Caribbean or African: Other Black" ~ "OBL",
      
      # Other Ethnic Group 
      `Ethnic Group` %in% c("Other ethnic group: Arab",
                            "Other ethnic group: Any other ethnic group") ~ "OTH",
      
      # Catch-all for top-level categories (e.g., "Total: All usual residents", "White") and NAs
      TRUE ~ NA_character_
    )
  )



uk_base_pop_12grp = uk_ethnic_composition %>%
  # Keep only leaf nodes that have been assigned an eth_code
  filter(!is.na(eth_code)) %>%
  # Aggregate to 12 NEWETHPOP groups (handles WBI from 4 rows, MIX from 4, OTH from 2)
  group_by(eth_code) %>%
  summarise(pop_2021 = sum(number), .groups = "drop") %>%
  mutate(
    eth_code   = factor(eth_code,
                        levels = c("WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH")),
    pct_2021   = round(pop_2021 / sum(pop_2021) * 100, 2)
  ) %>%
  arrange(eth_code)

# ============================================================
birmingham_base_pop_12grp = read_csv("data/processed/birmingham_base_pop_12grp.csv")

internal_net_12grp = read_csv("data/processed/internal_net_12grp_bham.csv")

ruk_base_pop_12grp = uk_base_pop_12grp %>% 
  left_join(birmingham_base_pop_12grp, by = "eth_code") %>% 
  mutate(pop_rest = pop_2021.x - pop_2021.y) %>% 
  mutate(pop_all = sum(pop_rest),
         pct = round(pop_rest/pop_all*100,2)) %>% 
  select(eth_code, pop_rest, pct) 


# All-ages internal migration rates
# out_rate = out_12 / Birmingham population at risk
# in_rate  = in_12  / RUK population at risk


migration_rates_allages  = internal_net_12grp %>%
  select(eth_code, out_12, in_12, net_12) %>%
  left_join(
    birmingham_base_pop_12grp %>% select(eth_code, bham_pop = pop_2021),
    by = "eth_code"
  )%>%
  left_join(
    ruk_base_pop_12grp,
    by = "eth_code"
  ) %>%
  mutate(
    out_rate = out_12 / bham_pop,   # probability of leaving Birmingham
    in_rate  = in_12  / pop_rest     # probability of entering Birmingham from RUK
  )


# =============================================================================================
#get age-sex schedule 
#outla = Nine-digit code for the local authority which is the origin of an internal migration flow  
#inla = Nine-digit code for the local authority which is the destination of an internal migration flow
#This part can be reused once we comission a table from ONS to obtain the observed subgroup ethnic migration 😉
# =============================================================================================
age_levels = c("0-4","5-9","10-14","15-19","20-24","25-29",
               "30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80-84","85-89","90+")

bham_migration_agesex = read_excel("data/migration/age_sex_migration_schedule_2022.xlsx", 
                                   sheet = "IM2022 on 2023 LAs")

#-----------------------------------------------------------------
#get the birmingham out 
age_sex_bham_out = bham_migration_agesex %>% 
  filter(outla == "E08000025",
         inla  != "E08000025") %>% 
  pivot_longer(cols = c(-outla,-inla,-sex,-year),
               values_to = "count",
               names_to = "Age") %>% 
  mutate(Age = str_replace(Age, "Age_", ""),
         Age = if_else(Age == "100+", "100", Age),
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
                                levels = age_levels)) %>% 
  group_by(sex,age_group_5yr) %>% 
  summarise(out_count = sum(count), .groups = "drop") 


#get the birmingham in
age_sex_bham_in = bham_migration_agesex %>% 
  filter(inla  == "E08000025",
         outla != "E08000025") %>% 
  pivot_longer(cols = c(-outla,-inla,-sex,-year),
               values_to = "count",
               names_to = "Age") %>% 
  mutate(Age = str_replace(Age, "Age_", ""),
         Age = if_else(Age == "100+", "100", Age),
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
                                levels = age_levels)) %>% 
  group_by(sex,age_group_5yr) %>% 
  summarise(in_count = sum(count), .groups = "drop") 


# Sense checks
stopifnot(nrow(age_sex_bham_out) == 38)    # 19 age groups × 2 sexes
stopifnot(nrow(age_sex_bham_in)  == 38)


#-----------------------------------------------------------------
# Build ratios of the profiles
#We converted single year of age profiles for men and women for UK migrants as a whole into ratios of the profile means. 
#These ratios were then multiplied by the mean probabilities generated.
#p.67 of the Leeds paper

bham_schedule = age_sex_bham_out %>%
  left_join(age_sex_bham_in, by = c("sex", "age_group_5yr")) %>%
  group_by(sex) %>%
  mutate(
    out_weight = out_count / mean(out_count),
    in_weight  = in_count  / mean(in_count)
  ) %>%
  ungroup()

# Apply to ethnic rates
migration_rates_ethagesex = migration_rates_allages %>% 
  select(eth_code, out_rate, in_rate) %>% 
  cross_join(bham_schedule %>% select(sex, age_group_5yr, out_weight, in_weight)) %>% 
  mutate(
    out_rate_as = out_rate * out_weight,
    in_rate_as  = in_rate  * in_weight,
    sex = ifelse(sex == "F", "Female", "Male")
  ) 



#-----------------------------------------------------------------
#paper section 9 (p.67-68) and section 10.2 (p.71):
# "Preliminary analysis of the time series at NUTS2 and LA scale did not reveal systematic trends 
# in direction of internal migration, so we adopted the assumption that the estimated 2007/8 probabilities
# would remain constant to 2050/51, the end of our projection period."
# Convert annual rates to 5-year period rates
# Formula: 1 - (1 - annual_rate)^5



migration_rates_agesex_rates_5yr = migration_rates_ethagesex  %>%
  mutate(
    out_rate_5yr = 1 - (1 - out_rate_as)^5,
    in_rate_5yr  = 1 - (1 - in_rate_as)^5
  ) %>%
  select(eth_code, sex, age_group_5yr, 
         out_rate_as, in_rate_as,        # keep annual for reference
         out_rate_5yr, in_rate_5yr)      # 5-year rates for CCM engine


write_csv(migration_rates_agesex_rates_5yr, "data/processed/Birmingham_internal_migration_rates.csv")
















