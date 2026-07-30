setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 06_International_immigration.R
# process the publicly available migration 
# using ethnic composition and redistribute
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)

# ============================================================
# Birmingham international immigration ethnic profile
# Census 2021 ODMG03EW provides composition, not future level
# ============================================================
internal_n_international_in_bham = read_rds("data/processed/Birmingham_international_immigration_ethnic_profile.rds")

bham_immigration_ethnic_share  = internal_n_international_in_bham %>%
  transmute(
    eth_code = as.character(eth_code),
    census_immigration_count = INt_B
  ) %>%
  mutate(
    ethnic_share =
      census_immigration_count /
      sum(census_immigration_count, na.rm = TRUE)
  ) %>%
  arrange(eth_code)

# ============================================================
# Birmingham international immigration ethnic profile
# From flag4 
# ============================================================

flag4_immigration_share = read_rds("data/processed/061a_immigration_shares.rds")


flag4_immigration_share =flag4_immigration_share %>% 
  arrange(year,eth_code) %>% 
  select(year,eth_code,ethnic_share=ethnic_shares)


# ============================================================
# ONS migration-category international immigration
# Birmingham, age × sex × projection year
# ============================================================
ons_immigration_males = read_csv("data/migration/2022 SNPP International in males.csv")

ons_immigration_females = read_csv("data/migration/2022 SNPP International in females.csv")


ons_bham_immigration_raw = bind_rows(
  ons_immigration_males,
  ons_immigration_females)


ons_bham_immigration_long = ons_bham_immigration_raw %>% 
  filter(AREA_NAME == "Birmingham") %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "Year",
               values_to = "Count") %>% 
  filter(AGE_GROUP != "All ages") %>% 
  mutate(Age = ifelse(AGE_GROUP == "90 and over", 90, AGE_GROUP),
         Age = as.integer(Age),
         Year = as.integer(Year),
         sex = case_when(
           str_detect(str_to_lower(SEX), "female") ~ "Female",
           str_detect(str_to_lower(SEX), "male")   ~ "Male",
           TRUE ~ NA_character_
         ))%>%
  select(
    Year,
    sex,
    Age,
    Count
  ) %>%
  arrange(
    Year,
    sex,
    Age
  )
# ============================================================
# create the 90–100 distribution
# ============================================================
from_90_100grid = expand.grid(
  Year = 2023:2047,
  sex = c("Female", "Male"),
  Age = 90:100
)

from_90_100grid_average = ons_bham_immigration_long %>% 
  filter(Age == 90) %>% 
  select(-Age) %>% 
  right_join(from_90_100grid, by = c("Year", "sex")) %>% 
  mutate(Count = Count / 11) %>% 
  select(Year, sex, Age, Count) %>%
  arrange(Year, sex, Age)


ons_bham_immigration_long_0_100 =bind_rows(ons_bham_immigration_long %>% filter(Age != 90),
          from_90_100grid_average )%>%
  arrange(
    Year,
    sex,
    Age
  )

# ============================================================
# Birmingham international immigration:
# year × ethnicity × sex × single age
# ============================================================
bham_international_immigration = ons_bham_immigration_long_0_100 %>%
  left_join(
    flag4_immigration_share %>%
      filter(year %in% 2023:2047) %>% 
      select(
        year,
        eth_code,
        ethnic_share
      ), by = c("Year" = "year")) %>%
  mutate(
    immigration_count = Count * ethnic_share) %>% 
  select(
    Year,
    eth_code,
    sex,
    Age,
    ethnic_share,
    Count,
    immigration_count) %>%
  arrange(
    Year,
    eth_code,
    sex,
    Age )



# ============================================================
# Turn bham_international_immigration  age-sex data into shares instead of counts
# ============================================================
share_table = bham_international_immigration %>% 
  filter(Year == "2023") %>% 
  mutate(all_in = sum(immigration_count)) %>% 
  group_by(sex,Age,ethnic_share) %>% 
  mutate(share = immigration_count/all_in) %>% 
  ungroup() %>%
  select(eth_code, sex, Age, ethnic_share, share)

# ============================================================
#load MYE3 data of 2021 and 2022

MYE11_23 =  read_excel("data/migration/myebtablesenglandwales20112023.xlsx", 
                       sheet = "MYEB3", skip = 1)


MYE11_23 = MYE11_23%>% 
  filter(laname23 == "Birmingham") %>% 
  select(international_in_2021, international_in_2022)

#pull the count
mye_inflow_2021 = MYE11_23 %>% pull(international_in_2021)
mye_inflow_2022 = MYE11_23 %>% pull(international_in_2022)

# ============================================================
# Birmingham international immigration totals from MYE
# ============================================================
mye_inflow_totals = tibble(
  Year = c(2021L, 2022L),
  total_immigration = c(
    mye_inflow_2021,
    mye_inflow_2022 ))

# ============================================================
# Use the 2023 SNPP age-sex pattern only
# ============================================================

age_sex_share_2023 = ons_bham_immigration_long_0_100 %>%
  filter(Year == 2023) %>%
  mutate(
    age_sex_share = Count / sum(Count)) %>%
  select(sex,Age, age_sex_share)


backfill_2021_22 = mye_inflow_totals %>%
   # Add the 2023 age-sex distribution to each year
  cross_join(age_sex_share_2023) %>%
  # Add the correct ethnic shares for 2021 and 2022
  left_join(
    flag4_immigration_share %>%
      filter(year %in% 2021:2022) %>%
      select( Year = year,eth_code,ethnic_share),
    by = "Year")  %>%
  mutate(
    # Total immigrants within each age-sex cell
    Count = total_immigration * age_sex_share,
    # Divide each age-sex cell between ethnic groups
    immigration_count = Count * ethnic_share) %>%
  select(Year,eth_code,sex,Age,ethnic_share,Count,immigration_count) %>%
  arrange(Year,eth_code,sex,Age)



# ============================================================
# Extend 2048–2061: hold the 2047 schedule constant
# (SNPP flows flat from 2026; continuing ONS's own assumption)
# ============================================================
extension_2048_61 = bham_international_immigration %>%
  filter(Year == 2047) %>%
  select(-Year) %>%
  cross_join(tibble(Year = 2048:2061)) %>%
  select(Year, eth_code, sex, Age, ethnic_share, Count, immigration_count) %>% 
  arrange(
    Year,
    eth_code,
    sex,
    Age )

# ============================================================
# Assemble full 2021–2061 immigration array
# ============================================================
bham_international_immigration_full = bind_rows(
  backfill_2021_22,
  bham_international_immigration,
  extension_2048_61
) %>%
  arrange(Year, eth_code, sex, Age)




bham_international_immigration_full %>%
  select(Year, eth_code, sex, Age, immigration_count) %>%
  write_csv("data/processed/06_Birmingham_international_immigration_flow_single_year.csv")



bham_international_immigration_full %>%
  group_by(Year) %>%
  summarise(
    immigration = sum(immigration_count),
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = Year,
      y = immigration
    )
  ) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0,40000))+
  labs(
    title = "Projected international immigration into Birmingham",
    subtitle = "ONS 2022-based migration-category variant",
    x = "Projection year",
    y = "International immigrants"
  ) +
  theme_minimal()






