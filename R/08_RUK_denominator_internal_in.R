setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 08_RUK_denominator_internal_in.R
# use 2022 SNPP for male and female
# use RM032
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)


# ============================================================
#Create the 2021 Rest of England and Wales population
#using RM032
# ============================================================
eth_pop = read_csv("data/RM032_LA_ethnic_pop_age_sex_2021.csv")


UK_eth_pop = eth_pop %>% 
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
    la_name = `Upper tier local authorities`,
    Ethnic20group = `Ethnic group (20 categories)`,
    SEX = `Sex (2 categories)`,
    Age = `Age (101 categories) Code`,
    eth_newethpop = eth_newethpop,
    eth_code = eth_code,
    Observation = Observation) %>% 
  filter(Ethnic20group != "Does not apply") %>% 
  group_by(la_name,eth_code,SEX,Age) %>% 
  summarise(Observation = sum(Observation),
            .groups = "drop")




rest_uk_2021 = UK_eth_pop %>%
  filter(la_name != "Birmingham") %>%
  group_by(eth_code,SEX,Age) %>%
  summarise(
    population_2021 = sum(Observation),
    .groups = "drop"
  )


# ============================================================
#Calculate ethnic shares for ages 0–89
#because 2022 snpp has 90 and over group
# ============================================================

rest_uk_ethnic_shares_0_89 = rest_uk_2021 %>%
  filter(Age <= 89) %>%
  group_by(SEX, Age) %>%
  mutate(
    ethnic_share = population_2021 / sum(population_2021)
  ) %>%
  ungroup() %>%
  select(SEX, Age, eth_code, ethnic_share)

# ============================================================
#Process the male and female SNPP files
# ============================================================
snpp_female = read_csv("data/migration/2022 SNPP Population females.csv")

snpp_male = read_csv("data/migration/2022 SNPP Population males.csv")



snpp_population = bind_rows(
  snpp_female,
  snpp_male
) %>%
  mutate(
    sex = case_when(
      SEX == "females" ~ "Female",
      SEX == "males"   ~ "Male"
    )
  ) %>%
  filter(
    COMPONENT == "Population",
    AGE_GROUP != "All ages"
  ) %>%
  pivot_longer(
    cols = matches("^20\\d{2}$"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.integer(year),
    age_group = as.character(AGE_GROUP)
  )

wales_snpp = read_csv(
  "data/migration/2022-based-population-projections-by-local-authority-age-sex-year-and-variant-wales.csv"
)


wales_snpp = wales_snpp%>%
  filter( Variant == "Principal projection",
          Sex %in% c("Male", "Female"),
          `Data description` == "Population projections",
          Age != "All ages") %>% 
  transmute(
    year = as.integer(Year_reference),
    area = Area_reference,
    sex = Sex,
    age = as.integer(Age_reference),
    population = `Data values`
  )


wales_total = wales_snpp %>%
  group_by(year, sex, age) %>%
  summarise(
    w_population = sum(population),
    .groups = "drop"
  ) %>% 
  mutate(age = ifelse(age == 90, "90 and over", age)) %>% 
  rename(age_group = age)



# ============================================================
#Create Rest of England
#since snpp is the whole england 
#we need snpp from statswales for wales 
# ============================================================

rest_england_snpp = snpp_population %>%
  filter(
    str_sub(AREA_CODE, 1, 3) %in% c("E06", "E07", "E08", "E09"),
    AREA_CODE != "E08000025"
  ) %>%
  group_by(year, sex, age_group) %>%
  summarise(
    rest_england_population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )



ruk_snpp = rest_england_snpp %>% 
  full_join(
    wales_total,
    by = c("year", "sex", "age_group")
  ) %>% 
  mutate(ruk_population = rest_england_population+w_population)%>%
  select(
    year,
    sex,
    age_group,
    ruk_population
  )


# ============================================================
#Create Rest of England 0-89
#snpp age-sex X rm032 ethnic share
#holding constant for projection
# ============================================================

rest_uk_snpp_0_89 = ruk_snpp %>%
  filter(age_group != "90 and over") %>%
  mutate(
    Age = as.integer(age_group)
  ) %>% 
  left_join(rest_uk_ethnic_shares_0_89, by = c("sex" = "SEX",
                                               "Age"), relationship = "many-to-many") %>% 
  mutate(ruk_ethnic_population = ruk_population * ethnic_share) %>% 
  select(
    year,
    sex,
    Age,
    eth_code,
    ethnic_share,
    ruk_ethnic_population
  )

# ============================================================
#Create Rest of England 90+
#snpp age-sex X rm032 ethnic share
#holding constant for projection
# ============================================================

#alculate the ethnic share within the 90+ population
ruk_ethnic_shares_90plus = rest_uk_2021 %>%
  filter(Age >= 90)%>%
  group_by(SEX, eth_code) %>%
  summarise(
    population_2021 = sum(population_2021, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(SEX) %>%
  mutate(
    ethnic_share = population_2021 / sum(population_2021)
  ) %>%
  ungroup()

#Calculate how each ethnic 90+ population is distributed across ages 90–100
ruk_age_weights_90plus = rest_uk_2021 %>%
  filter(Age >= 90, Age <= 100) %>%
  group_by(SEX, eth_code) %>%
  mutate(
    age_weight_90plus =
      population_2021 / sum(population_2021, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(
    SEX,
    eth_code,
    Age,
    age_weight_90plus
  )

#Split the SNPP 90+ total into ethnic groups
rest_uk_snpp_90plus_total = ruk_snpp %>%
  filter(age_group == "90 and over") %>%
  left_join(
    ruk_ethnic_shares_90plus,
    by = c("sex" = "SEX"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    ruk_ethnic_population_90plus =
      ruk_population * ethnic_share
  ) %>%
  select(
    year,
    sex,
    eth_code,
    ethnic_share,
    ruk_ethnic_population_90plus
  )

#Split each ethnic 90+ total into single ages 90–100
rest_uk_snpp_90_100 = rest_uk_snpp_90plus_total %>%
  left_join(
    ruk_age_weights_90plus,
    by = c(
      "sex" = "SEX",
      "eth_code"
    ),
    relationship = "many-to-many"
  ) %>%
  mutate(
    ruk_ethnic_population =
      ruk_ethnic_population_90plus * age_weight_90plus
  ) %>%
  select(
    year,
    sex,
    Age,
    eth_code,
    ethnic_share,
    age_weight_90plus,
    ruk_ethnic_population
  )

# ============================================================
#bind 0-100 together 2022-2047
# ============================================================



rest_uk_ethnic_population = bind_rows(
  rest_uk_snpp_0_89 %>%
    select(
      year,
      sex,
      Age,
      eth_code,
      ethnic_share,
      ruk_ethnic_population
    ),
  
  rest_uk_snpp_90_100 %>%
    select(
      year,
      sex,
      Age,
      eth_code,
      ethnic_share,
      ruk_ethnic_population
    )
) %>%
  arrange(
    year,
    sex,
    Age,
    eth_code
  )


# ============================================================
#add the 2021 back from rm032 on top
# ============================================================
rest_uk_2021_final = rest_uk_2021 %>%
  transmute(
    year = 2021,
    sex = SEX,
    Age,
    eth_code,
    ruk_ethnic_population = population_2021
  )



rest_uk_ethnic_population = bind_rows(
  rest_uk_2021_final,
  rest_uk_ethnic_population
) %>%
  arrange(
    year,
    sex,
    Age,
    eth_code
  )

write_rds(rest_uk_ethnic_population, "data/processed/08_RUK_internal_in_denominator.rds")













