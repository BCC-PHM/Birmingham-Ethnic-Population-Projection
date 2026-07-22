setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 07_international_emmigration.R
# process the publicly available international immigration 
# using ethnic composition and redistribute for the rest of UK
#to support the use of bidirectional model
# ============================================================
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)


# ============================================================
#Long-term international migration, provisional: year ending June 2025
#borrow the uk emmigration british and non-brit split
# ============================================================

Long_term_international_migration = read_excel("~/R projects/PHM/BCC ethnic population projection/data/migration/longterminternationalmigrationprovisionalyejune2020toyejune2022.xlsx", 
                                               sheet = "1. LTIM by nationality", skip = 2)



brit_nobrit_prop = Long_term_international_migration %>% 
  filter(Flow == "Outflow", Period == "YE Jun 2022 P") %>% 
  mutate(brit_prop = British/`All Nationalities`,
         nonbrit_prop = 1-brit_prop)
  

brit_prop = brit_nobrit_prop %>% pull(brit_prop)
nonbrit_prop = brit_nobrit_prop %>% pull(nonbrit_prop)

# ============================================================
# Birmingham international immigration ethnic profile for non brit
# Birmingham internal out migration ethnic profile for brit
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



internal_out_bham = read_rds("data/processed/Birmingham_internal_out_ethnic_profile.rds")

bham_internal_out_share = internal_out_bham %>% 
  transmute(
    eth_code = as.character(eth_code),
    census_internal_out_count = OUT_B
  ) %>% 
  mutate(ethnic_share = census_internal_out_count/sum(census_internal_out_count)) %>% 
  arrange(eth_code)


# ============================================================
# ONS migration-category international immigration
# Birmingham, age × sex × projection year
# ============================================================

ons_emigration_males =  read_csv("~/R projects/PHM/BCC ethnic population projection/data/migration/2022 SNPP International out males.csv")

ons_emigration_females =  read_csv("~/R projects/PHM/BCC ethnic population projection/data/migration/2022 SNPP International out females.csv")

ons_bham_emigration_raw = bind_rows(
  ons_emigration_males,
  ons_emigration_females
)



ons_bham_emmigration_long = ons_bham_emigration_raw%>% 
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

from_90_100grid_average = ons_bham_emmigration_long %>% 
  filter(Age == 90) %>% 
  select(-Age) %>% 
  right_join(from_90_100grid, by = c("Year", "sex")) %>% 
  mutate(Count = Count / 11) %>% 
  select(Year, sex, Age, Count) %>%
  arrange(Year, sex, Age)


ons_bham_emmigration_long_0_100 =bind_rows(ons_bham_emmigration_long %>% filter(Age != 90),
                                           from_90_100grid_average )%>%
  arrange(
    Year,
    sex,
    Age
  )

# ============================================================
# Birmingham international emmigration:
# splitting to brit and non brit first
# ============================================================

ons_bham_emmigration_long_0_100_brit = ons_bham_emmigration_long_0_100 %>% 
  mutate(brit_share = brit_prop,
         Count = Count*brit_share) %>% 
  select(-brit_share)

ons_bham_emmigration_long_0_100_nonbrit = ons_bham_emmigration_long_0_100 %>% 
  mutate(nonbrit_share = nonbrit_prop,
         Count = Count*nonbrit_prop) %>% 
  select(-nonbrit_share)

# ============================================================
# Birmingham international emmigration:
# year × ethnicity × sex × single age for each
# brit and non brit corresponding ethnic share
# ============================================================

bham_international_emmigration_brit = ons_bham_emmigration_long_0_100_brit %>%
  cross_join(
    bham_internal_out_share%>%
      select(
        eth_code,
        ethnic_share
      )) %>%
  mutate(
    emmigration_count = Count * ethnic_share) %>% 
  select(
    Year,
    eth_code,
    sex,
    Age,
    ethnic_share,
    Count,
    emmigration_count) %>%
  arrange(
    Year,
    eth_code,
    sex,
    Age )




bham_international_emmigration_nonbrit = ons_bham_emmigration_long_0_100_nonbrit %>%
  cross_join(
    bham_immigration_ethnic_share%>%
      select(
        eth_code,
        ethnic_share
      )) %>%
  mutate(
    emmigration_count = Count * ethnic_share) %>% 
  select(
    Year,
    eth_code,
    sex,
    Age,
    ethnic_share,
    Count,
    emmigration_count) %>%
  arrange(
    Year,
    eth_code,
    sex,
    Age )




# ============================================================
# Turn bham_international_emmigration  age-sex data into shares instead of counts
# ============================================================
share_table_brit = bham_international_emmigration_brit %>% 
  filter(Year == "2023") %>% 
  mutate(all_out = sum(emmigration_count)) %>% 
  group_by(sex,Age,ethnic_share) %>% 
  mutate(share = emmigration_count/all_out) %>% 
  ungroup() %>%
  select(eth_code, sex, Age, ethnic_share, share)



share_table_nonbrit = bham_international_emmigration_nonbrit %>% 
  filter(Year == "2023") %>% 
  mutate(all_out = sum(emmigration_count)) %>% 
  group_by(sex,Age,ethnic_share) %>% 
  mutate(share = emmigration_count/all_out) %>% 
  ungroup() %>%
  select(eth_code, sex, Age, ethnic_share, share)



# ============================================================
#the birmingham 202021 202122 migration out
#202021: 10,908 
#202122: 7548


bham_outflow_2021 = 10908
bham_outflow_2022 = 7548

bham_outflow_2021_brit = bham_outflow_2021*brit_prop
bham_outflow_2021_nonbrit = bham_outflow_2021*nonbrit_prop

bham_outflow_2022_brit = bham_outflow_2022*brit_prop
bham_outflow_2022_nonbrit = bham_outflow_2022*nonbrit_prop




backfill_2021_22_brit = share_table_brit %>%
  cross_join(tibble(Year = 2021:2022)) %>%
  mutate(
    Count             = NA_real_,               # no SNPP count for these years
    emmigration_count = ifelse(Year == 2021, share * bham_outflow_2021_brit, share*bham_outflow_2022_brit)
  ) %>%
  select(Year, eth_code, sex, Age, ethnic_share, Count, emmigration_count) %>% 
  arrange(
    Year,
    eth_code,
    sex,
    Age )


backfill_2021_22_nonbrit = share_table_nonbrit %>% 
  cross_join(tibble(Year = 2021:2022)) %>% 
  mutate(
    Count = NA_real_,
    emmigration_count = ifelse(Year == 2021, share*bham_outflow_2021_nonbrit, share*bham_outflow_2022_nonbrit)
  ) %>% 
  select(Year, eth_code, sex, Age, ethnic_share, Count, emmigration_count) %>% 
  arrange(
    Year,
    eth_code,
    sex,
    Age
  )


# ============================================================
# Extend 2048–2061: hold the 2047 schedule constant
# (SNPP flows flat from 2026; continuing ONS's own assumption)
# ============================================================
extension_2048_61_brit = bham_international_emmigration_brit %>%
  filter(Year == 2047) %>%
  select(-Year) %>%
  cross_join(tibble(Year = 2048:2061)) %>%
  select(Year, eth_code, sex, Age, ethnic_share, Count, emmigration_count) %>% 
  arrange(
    Year,
    eth_code,
    sex,
    Age )



extension_2048_61_nonbrit = bham_international_emmigration_nonbrit %>%
  filter(Year == 2047) %>%
  select(-Year) %>%
  cross_join(tibble(Year = 2048:2061)) %>%
  select(Year, eth_code, sex, Age, ethnic_share, Count, emmigration_count) %>% 
  arrange(
    Year,
    eth_code,
    sex,
    Age )



# ============================================================
# Assemble full 2021–2061 immigration array
# ============================================================
bham_international_emmigration_full_brit = bind_rows(
  backfill_2021_22_brit,
  bham_international_emmigration_brit,
  extension_2048_61_brit
) %>%
  arrange(Year, eth_code, sex, Age) %>% 
  rename(emmigration_count_brit = emmigration_count)



bham_international_emmigration_full_nonbrit = bind_rows(
  backfill_2021_22_nonbrit,
  bham_international_emmigration_nonbrit,
  extension_2048_61_nonbrit
) %>%
  arrange(Year, eth_code, sex, Age) %>% 
  rename(emmigration_count_nonbrit = emmigration_count)




bham_international_emmigration_full = bham_international_emmigration_full_brit %>% 
  left_join(bham_international_emmigration_full_nonbrit, by = c("Year", "eth_code", "sex", "Age")) %>% 
  mutate(emmigration_count = emmigration_count_brit+emmigration_count_nonbrit) %>% 
  select(Year, eth_code, sex, Age, emmigration_count)




bham_international_emmigration_full %>% 
  write_csv("data/processed/07_Birmingham_international_emmigration_flow_single_year.csv")


bham_international_emmigration_full %>%
  group_by(Year) %>%
  summarise(
    emmigration = sum(emmigration_count),
    .groups = "drop"
  ) %>%
  ggplot(
    aes(
      x = Year,
      y = emmigration
    )
  ) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0,12000))+
  labs(
    title = "Projected international emmigration into Birmingham",
    subtitle = "ONS 2022-based migration-category variant",
    x = "Projection year",
    y = "International emmigrants"
  ) +
  theme_minimal()






