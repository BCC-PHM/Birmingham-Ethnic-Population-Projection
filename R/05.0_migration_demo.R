setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 05.0_migration_demo.R
# process the publicly available migration 
# using ethnic composition and redistribute
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)


# ============================================================


birmingham_ethnic_composition = read_excel("~/R projects/PHM/BCC ethnic population projection/data/migration/nomis_2026_06_23_birmingham_ethnic_composition.xlsx", 
                                            skip = 9) %>%
  # Rename the auto-generated column name to 'Ethnic Group'
  rename(`Ethnic Group` = `...1`)


birmingham_ethnic_composition = birmingham_ethnic_composition %>%
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


birmingham_base_pop_12grp = birmingham_ethnic_composition %>%
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

#for 0.5.1
write_csv(birmingham_base_pop_12grp, "data/processed/birmingham_base_pop_12grp.csv")

# ============================================================

ODMG03EW_LTLA = read_csv("data/migration/ODMG03EW_LTLA.csv")


colnames(ODMG03EW_LTLA) = gsub(" ", "_", colnames(ODMG03EW_LTLA))


internal_out = ODMG03EW_LTLA %>% 
  filter(Migrant_LTLA_one_year_ago_label == "Birmingham",
         Lower_tier_local_authorities_label != "Birmingham") %>% 
  group_by(Migrant_LTLA_one_year_ago_code, `Ethnic_group_(6_categories)_label`) %>% 
  summarise(count = sum(Count),.groups = "drop")




internal_in = ODMG03EW_LTLA %>% 
  filter(
    Lower_tier_local_authorities_label == "Birmingham",
    !Migrant_LTLA_one_year_ago_code %in% c("-8", "999999999"),
    Migrant_LTLA_one_year_ago_label != "Birmingham"   # exclude within-Bham movers
  ) %>%
  group_by(Lower_tier_local_authorities_code, `Ethnic_group_(6_categories)_label`) %>%
  summarise(count = sum(Count), .groups = "drop")



internal_net <- internal_out %>%
  ungroup() %>%
  select(ethnic_group = `Ethnic_group_(6_categories)_label`, out = count) %>%
  left_join(
    internal_in %>%
      select(ethnic_group = `Ethnic_group_(6_categories)_label`, in_ = count),
    by = "ethnic_group"
  ) %>%
  mutate(
    net         = in_ - out,
    efficiency  = round(net / (in_ + out) * 100, 1)   # migration efficiency index
  )


# ============================================================
# Option 1: propensity borrowing
# Disaggregate 5-group internal migration to 12 NEWETHPOP groups
# using Birmingham 2021 Census population shares as proxy propensities
# ============================================================
#---------------------------------------------------
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

internal_net_12grp = propensity %>%
  left_join(
    internal_net %>% select(ethnic_group, out, in_, net),
    by = "ethnic_group"
  ) %>%
  mutate(
    out_12  = round(out * prop),
    in_12   = round(in_ * prop),
    net_12  = in_12 - out_12              # recompute net from rounded components
  ) %>%
  select(eth_code, ethnic_group, pop_2021, prop, out_12, in_12, net_12) %>%
  mutate(
    eth_code = factor(eth_code,
                      levels = c("WBI","WHO","MIX","IND","PAK","BAN",
                                 "CHI","OAS","BLA","BLC","OBL","OTH"))
  ) %>%
  arrange(eth_code)

write_csv(internal_net_12grp, "data/processed/internal_net_12grp_bham.csv")

#---------------------------------------------------


