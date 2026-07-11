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
  cross_join(
    bham_immigration_ethnic_share %>%
      select(
        eth_code,
        ethnic_share
      )) %>%
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







bham_international_immigration %>%
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
MYE3_2021 = read_excel("data/migration/mye22tablesew2021geogs.xlsx", 
                       sheet = "MYE3", skip = 7)

MYE3_2022 = read_excel("data/migration/mye22tablesew2023geogs.xlsx", 
                       sheet = "MYE3", skip = 7)

MYE3_2021 = MYE3_2021 %>% 
  filter(Name == "Birmingham") %>% 
  select(`International Migration Inflow`)

#pull the count
mye_inflow_2021_22 = MYE3_2021 %>% pull(`International Migration Inflow`)

backfill_2021_22 = share_table %>%
  cross_join(tibble(Year = 2021:2022)) %>%
  mutate(
    Count             = NA_real_,               # no SNPP count for these years
    immigration_count = share * mye_inflow_2021_22
  ) %>%
  select(Year, eth_code, sex, Age, ethnic_share, Count, immigration_count) %>% 
  arrange(
    Year,
    eth_code,
    sex,
    Age )

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
  write_csv("data/processed/Birmingham_international_immigration_flow_single_year.csv")










