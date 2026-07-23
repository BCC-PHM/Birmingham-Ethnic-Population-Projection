setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 05.0a_internal_migration.R
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
#Birmingham internal in- and out-
ODMG03EW_LTLA = read_csv("data/migration/ODMG03EW_LTLA.csv")


colnames(ODMG03EW_LTLA) = gsub(" ", "_", colnames(ODMG03EW_LTLA))

#---------------------------------------------
#equation 9.8 OUT_B
internal_out_bham = ODMG03EW_LTLA %>% 
  filter(Migrant_LTLA_one_year_ago_label == "Birmingham",
         Lower_tier_local_authorities_label != "Birmingham") %>% 
  group_by(Migrant_LTLA_one_year_ago_code, `Ethnic_group_(6_categories)_label`) %>% 
  summarise(OUT_B = sum(Count),.groups = "drop") %>% 
  rename(ethnic_group = `Ethnic_group_(6_categories)_label`)


#---------------------------------------------
#equation 9.6 IN_B  group by ethnicity only, add E&W origin restriction
internal_in_bham = ODMG03EW_LTLA %>% 
  filter(Lower_tier_local_authorities_label == "Birmingham",
         Migrant_LTLA_one_year_ago_label != "Birmingham",
         !Migrant_LTLA_one_year_ago_code %in% c("-8", "999999999"),
         str_sub(Migrant_LTLA_one_year_ago_code, 1, 1) %in% c("E", "W"))%>%
  group_by(Lower_tier_local_authorities_code, `Ethnic_group_(6_categories)_label`) %>%
  summarise(IN_B = sum(Count), .groups = "drop") %>% 
  rename(ethnic_group = `Ethnic_group_(6_categories)_label`)


international_in_bham = ODMG03EW_LTLA %>% 
  filter(
  Lower_tier_local_authorities_label == "Birmingham",
  Migrant_LTLA_one_year_ago_code %in% c("999999999"),
  Migrant_LTLA_one_year_ago_label != "Birmingham"   # exclude within-Bham movers
) %>%
  group_by(Lower_tier_local_authorities_code, `Ethnic_group_(6_categories)_label`) %>%
  summarise(INt_B = sum(Count), .groups = "drop") %>% 
  rename(ethnic_group = `Ethnic_group_(6_categories)_label`)


internal_n_international_in_bham = internal_in_bham %>% 
  left_join(international_in_bham, by = c("Lower_tier_local_authorities_code",
                                          "ethnic_group"))


#------------------------------------------------------------
#we need the same for the rest of england and wales 
#9.10 and 9.11

international_in_RUK= ODMG03EW_LTLA %>% 
  filter(Migrant_LTLA_one_year_ago_code %in% c("999999999"),
         Lower_tier_local_authorities_label !="Birmingham") %>% 
  mutate(Lower_tier_local_authorities_code = "RUK") %>% 
  group_by(Lower_tier_local_authorities_code, `Ethnic_group_(6_categories)_label`) %>% 
  summarise(INt_RUK = sum(Count),.groups = "drop") %>% 
  rename(ethnic_group = `Ethnic_group_(6_categories)_label`)


# ============================================================
# Option 1: propensity borrowing
# Disaggregate 5-group internal migration to 12 NEWETHPOP groups
# using Birmingham 2021 Census population shares as proxy propensities
# ============================================================
#propensity borrowing from Rees instead 
#using the ethnic population shares just feel too wrong

#this is for birmingham internal in 
Rees_internal_in_shares = read_csv("data/migration/InMig_2021_2022_LEEDS2.csv")

#this is for birmingham internal out
Rees_internal_out_shares = read_csv("data/migration/OutMig_2012_2013_LEEDS1.csv")

#this is for RUK immigrants
Immig_2011_2012_LEEDS1 = read_csv("data/migration/Immig_2011_2012_LEEDS1.csv")

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
#----------------------------------------
#birmingham in-ethnic-shares
Rees_internal_in_prop = Rees_internal_in_shares %>% 
  filter(LAD.name == "Birmingham") %>% 
  select(-...1) %>% 
  pivot_longer(cols = c(-LAD.name,-LAD.code,-ETH.group),
               names_to = "SexAge",
               values_to = "count") %>% 
  mutate(Sex = ifelse(str_detect(SexAge, "^M"),
                      "Male",
                      "Female"),
         Age = str_extract(SexAge, "\\.\\d+$"),
         Age = as.integer(str_replace(Age, "\\.", ""))) %>% 
  drop_na() %>% 
  group_by(ETH.group) %>% 
  summarise(count = sum(count),
            .groups = "drop") %>% 
  rename(eth_code = ETH.group) %>% 
  left_join(broad_group_map, by = "eth_code") %>% 
  group_by(ethnic_group) %>% 
  mutate(prop = count/sum(count)*100) %>% 
  ungroup()

#----------------------------------------
#birmingham out-ethnic-share
Rees_internal_out_prop = Rees_internal_out_shares %>% 
  filter(LAD.name == "Birmingham") %>% 
  select(-...1) %>% 
  pivot_longer(cols = c(-LAD.name,-LAD.code,-ETH.group),
               names_to = "SexAge",
               values_to = "count") %>% 
  mutate(Sex = ifelse(str_detect(SexAge, "^M"),
                      "Male",
                      "Female"),
         Age = str_extract(SexAge, "\\.\\d+$"),
         Age = as.integer(str_replace(Age, "\\.", ""))) %>% 
  drop_na() %>%
  group_by(ETH.group) %>% 
  summarise(count = sum(count),
            .groups = "drop") %>% 
  rename(eth_code = ETH.group) %>% 
  left_join(broad_group_map, by = "eth_code") %>% 
  group_by(ethnic_group) %>% 
  mutate(prop = count/sum(count)*100) %>% 
  ungroup()

#----------------------------------------
#RUK immigration ethnic share
Rees_international_in_RUK_prop = Immig_2011_2012_LEEDS1 %>% 
  filter(LAD.name != "Birmingham") %>% 
  select(-...1) %>% 
  pivot_longer(cols = c(-LAD.name,-LAD.code,-ETH.group),
               names_to = "SexAge",
               values_to = "count") %>% 
  mutate(Sex = ifelse(str_detect(SexAge, "^M"),
                      "Male",
                      "Female"),
         Age = str_extract(SexAge, "\\.\\d+$"),
         Age = as.integer(str_replace(Age, "\\.", ""))) %>% 
  drop_na() %>% 
  group_by(ETH.group) %>% 
  summarise(count = sum(count),
            .groups = "drop") %>% 
  rename(eth_code = ETH.group) %>% 
  left_join(broad_group_map, by = "eth_code") %>% 
  group_by(ethnic_group) %>% 
  mutate(prop = count/sum(count)*100) %>% 
  ungroup()

#----------------------------------------
#Birmingham immigration ethnic share

Rees_international_in_bham_prop = Immig_2011_2012_LEEDS1 %>% 
  filter(LAD.name == "Birmingham") %>% 
  select(-...1) %>% 
  pivot_longer(cols = c(-LAD.name,-LAD.code,-ETH.group),
               names_to = "SexAge",
               values_to = "count") %>% 
  mutate(Sex = ifelse(str_detect(SexAge, "^M"),
                      "Male",
                      "Female"),
         Age = str_extract(SexAge, "\\.\\d+$"),
         Age = as.integer(str_replace(Age, "\\.", ""))) %>% 
  drop_na() %>% 
  group_by(ETH.group) %>% 
  summarise(count = sum(count),
            .groups = "drop") %>% 
  rename(eth_code = ETH.group) %>% 
  left_join(broad_group_map, by = "eth_code") %>% 
  group_by(ethnic_group) %>% 
  mutate(prop = count/sum(count)*100) %>% 
  ungroup()


# ============================================================
#multiply the propensity calculated from above
# ============================================================
internal_n_international_in_bham = Rees_internal_in_prop %>%
  left_join(Rees_international_in_bham_prop %>% select(eth_code,prop),
            by = "eth_code") %>% 
  rename(In_prop =prop.x, Int_in_prop = prop.y) %>% 
  left_join(
    internal_n_international_in_bham,
    by = "ethnic_group"
  ) %>% 
  mutate(
    IN_B   = IN_B * In_prop/100,
    INt_B = INt_B * Int_in_prop/100
  ) %>% 
  select(eth_code, ethnic_group,  In_prop, Int_in_prop, IN_B, INt_B) %>%
  mutate(
    eth_code = factor(eth_code,
                      levels = c("WBI","WHO","MIX","IND","PAK","BAN",
                                 "CHI","OAS","BLA","BLC","OBL","OTH"))
  ) %>%
  arrange(eth_code)

# write_rds(internal_n_international_in_bham, "data/processed/Birmingham_international_immigration_ethnic_profile.rds")

internal_out_bham = Rees_internal_out_prop %>%
  left_join(
    internal_out_bham,
    by = "ethnic_group"
  ) %>% 
  mutate(
    OUT_B   = OUT_B * prop/100
  ) %>% 
  select(eth_code, ethnic_group, prop, OUT_B) %>%
  mutate(
    eth_code = factor(eth_code,
                      levels = c("WBI","WHO","MIX","IND","PAK","BAN",
                                 "CHI","OAS","BLA","BLC","OBL","OTH"))
  ) %>%
  arrange(eth_code)

# write_rds(internal_out_bham, "data/processed/Birmingham_internal_out_ethnic_profile.rds")


international_in_RUK = Rees_international_in_RUK_prop %>% 
  left_join(
    international_in_RUK,
    by = "ethnic_group"
  ) %>% 
  mutate(
    INt_RUK   = INt_RUK* prop/100
  ) %>% 
  select(eth_code, ethnic_group, prop, INt_RUK) %>%
  mutate(
    eth_code = factor(eth_code,
                      levels = c("WBI","WHO","MIX","IND","PAK","BAN",
                                 "CHI","OAS","BLA","BLC","OBL","OTH"))
  ) %>%
  arrange(eth_code)




#==========================================================================================
#England and Whales to be precise
#we need the the denominator
#Two census populations, both at 12 groups, all-age. 
#==========================================================================================
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
#now we have both base pop
# ============================================================
birmingham_base_pop_12grp = read_csv("data/processed/birmingham_base_pop_12grp.csv")


ruk_base_pop_12grp = uk_base_pop_12grp %>% 
  left_join(birmingham_base_pop_12grp, by = "eth_code") %>% 
  mutate(
    pop_bham = pop_2021.y,
    pop_rest = pop_2021.x - pop_2021.y) %>% 
  mutate(pop_all = sum(pop_rest),
         pct = round(pop_rest/pop_all*100,2)) %>% 
  select(eth_code,pop_bham, pop_rest) 

#--------------------------------------------------------------
#birmingham_internal_out_rate
bham_internal_out_rate = internal_n_international_in_bham %>% 
  left_join(internal_out_bham %>% select(eth_code,OUT_B),
            by = "eth_code") %>% 
  left_join(birmingham_base_pop_12grp,
            by = "eth_code") %>% 
  mutate(SS_B = pop_2021 - IN_B-INt_B, #survivng stayers 
         WS_B = SS_B+OUT_B,            #everyone who started in the year in Bham and still alive    
         out_rate = OUT_B/WS_B) %>% 
  select(eth_code,
         OUT_B,
         WS_B,
         out_rate)
  


#birmingham_internal_in_rate
bham_internal_in_rate = internal_out_bham %>% 
  left_join(international_in_RUK %>% select(eth_code, INt_RUK),
            by = "eth_code") %>% 
  left_join(internal_n_international_in_bham %>% select(eth_code, IN_B),
            by = "eth_code") %>% 
  left_join(ruk_base_pop_12grp, by = "eth_code") %>% 
  mutate(
         in_rate = IN_B / pop_rest) %>% 
  select(eth_code,
         IN_B,
         pop_rest,
         in_rate)

#we cannot use this anymore becuase we are unable to ge the within survivor of RUK
#instead we now will just use population stock so we use poprest denominator to match 
# SS_R = pop_rest-OUT_B-INt_RUK, #survivng stayers of the rest of UK
# WS_R = SS_R+IN_B,              #everyone who started in the year in RUK and still alive
# in_rate = IN_B/WS_R,

migration_rates_allages = bham_internal_in_rate %>% 
  select(
    eth_code,
    in_rate
  ) %>%
  left_join(bham_internal_out_rate%>%
              select(
                eth_code,
                out_rate
              ), by = "eth_code")

#===========================================================================================
#get singel year of age-sex schedule 
#outla = Nine-digit code for the local authority which is the origin of an internal migration flow  
#inla = Nine-digit code for the local authority which is the destination of an internal migration flow
#This part can be reused once we comission a table from ONS to obtain the observed subgroup ethnic migration 😉
# =============================================================================================
bham_migration_agesex = read_excel("data/migration/age_sex_migration_schedule_2021.xlsx", 
                                   sheet = "2021 on 2021 LAs")

bham_migration_agesex = bham_migration_agesex %>% rename(year=Year)

bham_migration_agesex2022 = read_excel("data/migration/age_sex_migration_schedule_2022.xlsx",
                                   sheet = "IM2022 on 2023 LAs")

bham_migration_agesex2023 = read_excel("data/migration/age_sex_migration_schedule_2023.xlsx", 
                                       sheet = "IM2023 on 2023 LAs")

bham_migration_agesex2024 = read_excel("data/migration/age_sex_migration_schedule_2024.xlsx", 
                                       sheet = "IM2024 on 2023 LAs")
# Combine ONS internal migration matrices, 2021–2024


bham_migration_agesex_series = bind_rows(
  bham_migration_agesex %>%
    mutate(year = 2021),
  
  bham_migration_agesex2022 %>%
    mutate(year = 2022),
  
  bham_migration_agesex2023 %>%
    mutate(year = 2023),
  
  bham_migration_agesex2024 %>%
    mutate(year = 2024)
)

#-----------------------------------------------------------------
#get the birmingham out 
age_sex_bham_out_series = bham_migration_agesex_series %>% 
  filter(outla == "E08000025",
         inla  != "E08000025") %>% 
  pivot_longer(cols = starts_with("Age_"),
               values_to = "out_count",
               names_to = "Age") %>% 
  mutate(Age = str_replace(Age, "Age_", ""),
         Age = if_else(Age == "100+", "100", Age),
         Age = as.integer(Age),
         sex = case_when(
           sex == "F" ~ "Female",
           sex == "M" ~ "Male",
           TRUE ~ as.character(sex))) %>% 
  group_by(year,sex,Age) %>% 
  summarise(out_count = sum(out_count), .groups = "drop") 


#get the birmingham in
age_sex_bham_in_series = bham_migration_agesex_series %>% 
  filter(inla  == "E08000025",
         outla != "E08000025")%>% 
  pivot_longer(cols = starts_with("Age_"),
               values_to = "in_count",
               names_to = "Age") %>% 
  mutate(Age = str_replace(Age, "Age_", ""),
         Age = if_else(Age == "100+", "100", Age),
         Age = as.integer(Age),
         sex = case_when(
           sex == "F" ~ "Female",
           sex == "M" ~ "Male",
           TRUE ~ as.character(sex))) %>% 
  group_by(year,sex,Age) %>% 
  summarise(in_count = sum(in_count), .groups = "drop") 


age_sex_bham_migration_series =
  full_join(
    age_sex_bham_out_series,
    age_sex_bham_in_series,
    by = c("year", "sex", "Age")
  ) %>%
  mutate(
    out_count = replace_na(out_count, 0),
    in_count = replace_na(in_count, 0)
  )

#-----------------------------------------------------------------
# Build ratios of the profiles
#We converted single year of age profiles for men and women for UK migrants as a whole into ratios of the profile means. 
#These ratios were then multiplied by the mean probabilities generated.
#p.67 of the Leeds paper

raw_pop = read_csv("data/RM032_LA_ethnic_pop_age_sex_2021.csv")

#the population denominator for age-sex profile for Birmingham
birmingham_age_sex_ethnic_pop = raw_pop %>% 
  filter(`Upper tier local authorities` == "Birmingham") %>% 
  group_by(sex = `Sex (2 categories)`,
           Age = `Age (101 categories) Code`) %>%
  summarise(pop = sum(Observation), .groups = "drop") %>%
  mutate(sex = if_else(sex == "Female", "Female", "Male"), 
         Age = as.integer(Age))


#the population denominator for age-sex profile for RUK
RUK_age_sex_ethnic_pop = raw_pop %>% 
  filter(`Upper tier local authorities` != "Birmingham") %>% 
  group_by(sex = `Sex (2 categories)`,
           Age = `Age (101 categories) Code`) %>%
  summarise(pop = sum(Observation), .groups = "drop") %>%
  mutate(sex = if_else(sex == "Female", "Female", "Male"), 
         Age = as.integer(Age))



bham_schedule = age_sex_bham_migration_series %>%
  filter(year == 2021) %>% 
  left_join(birmingham_age_sex_ethnic_pop, by = c("sex", "Age")) %>% 
  left_join(RUK_age_sex_ethnic_pop %>% rename(pop_ruk = pop),
            by = c("sex","Age")) %>%  
  mutate(
    out_rate_raw = out_count / pop,        # Birmingham origin
    in_rate_raw  = in_count  / pop_ruk     # RUK origin  ← was pop
  ) %>%
  mutate(
    out_mean = sum(out_count,na.rm = TRUE) / sum(pop,na.rm = TRUE),
    in_mean  = sum(in_count,na.rm = TRUE)  / sum(pop_ruk,na.rm = TRUE),   # ← was pop
    out_weight = out_rate_raw / out_mean,
    in_weight  = in_rate_raw  / in_mean
  ) %>%
  group_by(sex) %>%
  mutate(
    out_weight = if_else(Age > 90, mean(out_weight[Age %in% 80:90]), out_weight),
    in_weight  = if_else(Age > 90, mean(in_weight[Age %in% 80:90]),  in_weight)
  ) %>%
  ungroup()

#---------------------------------------------------------
# pure arrival-share allocation (kept alongside Option A)
bham_in_schedule =  bham_schedule %>%
  select(sex,Age,in_count) %>% 
  mutate(arrival_share = in_count / sum(in_count, na.rm = TRUE))
stopifnot(abs(sum(bham_in_schedule$arrival_share) - 1) < 1e-10)

internal_in_counts_ethagesex = internal_n_international_in_bham %>%
  select(eth_code, IN_B) %>%
  cross_join(bham_in_schedule %>% select(sex, Age, arrival_share)) %>%
  mutate(IN_B_as = IN_B * arrival_share) %>%
  select(eth_code, sex, Age, IN_B, arrival_share, IN_B_as)

# allocation preserves each group's total inflow
chk = internal_in_counts_ethagesex %>%
  group_by(eth_code) %>%
  summarise(d = sum(IN_B_as) - first(IN_B), .groups = "drop")
stopifnot(all(abs(chk$d) < 1e-8))


write_csv(internal_in_counts_ethagesex,
          "data/processed/Birmingham_internal_in_counts_single_year.csv")
#---------------------------------------------------------
# ============================================================
# Out-migration probability profile (Birmingham origin)
# ============================================================
internal_out_rates_ethagesex = migration_rates_allages %>%
  select(eth_code, out_rate) %>%
  cross_join(bham_schedule %>% select(sex, Age, out_weight)) %>%
  mutate(out_rate_as = out_rate * out_weight) %>%
  select(eth_code, sex, Age, out_rate_as)

# ============================================================
# In-migration probability profile (RUK origin) — Rees rate
# in_weight already RUK-denominated, so this IS the Rees input
# ============================================================
internal_in_rates_ethagesex = migration_rates_allages %>%
  select(eth_code, in_rate) %>%
  cross_join(bham_schedule %>% select(sex, Age, in_weight)) %>%
  mutate(in_rate_as = in_rate * in_weight) %>%
  select(eth_code, sex, Age, in_rate_as)

# ============================================================
# Combine — final projection inputs (both are rates, Rees-style)
# ============================================================
migration_ethagesex = internal_out_rates_ethagesex %>%
  full_join(internal_in_rates_ethagesex, by = c("eth_code","sex","Age")) %>%
  # keep Option B counts alongside, for the engine-time ethnic-denominator refinement
  left_join(internal_in_counts_ethagesex %>% select(eth_code, sex, Age, IN_B_as),
            by = c("eth_code","sex","Age")) %>%
  arrange(eth_code, sex, Age)



# ============================================================
# All-group Census base probabilities
# These are the denominators of the ONS updating indexes
# ============================================================

census_out_probability =sum(bham_internal_out_rate$OUT_B,na.rm = TRUE) /sum(bham_internal_out_rate$WS_B,na.rm = TRUE)

census_in_probability =sum(bham_internal_in_rate$IN_B,na.rm = TRUE) / sum(bham_internal_in_rate$pop_rest,na.rm = TRUE)

census_base_probabilities = tibble(
  direction = c("Internal in", "Internal out"),
  probability = c(
    census_in_probability,
    census_out_probability
  )
)
#rename for clearity
census_in_rate_rew_to_bham =
  census_in_probability

census_out_rate_bham_to_rew =
  census_out_probability

#now calculate the 2022 ONS probabilities

ons_2021_probabilities = bham_schedule %>%
  summarise(
    ons_out_flow =sum(out_count, na.rm = TRUE),
    ons_in_flow = sum(in_count, na.rm = TRUE),
    bham_exposure =sum(pop, na.rm = TRUE),
    rew_exposure =sum(pop_ruk, na.rm = TRUE),
    ons_out_probability =ons_out_flow / bham_exposure,
    ons_in_probability =ons_in_flow / rew_exposure
  )

ons_2021_probabilities


internal_migration_index_2021 =ons_2021_probabilities %>%
  transmute(
    year = 2021,
    
    census_out_probability =census_out_rate_bham_to_rew,
    ons_out_probability,
    out_index =ons_out_probability /census_out_rate_bham_to_rew,
    census_in_probability = census_in_rate_rew_to_bham,
    ons_in_probability,
    in_index =ons_in_probability /census_in_rate_rew_to_bham
  )

internal_migration_index_2021










write_csv(migration_ethagesex, "data/processed/05_Birmingham_internal_migration_rates_single_year.csv")

# ============================================================

# ggplot(bham_schedule, aes(Age, out_weight, colour = sex)) +
#   geom_line() + geom_vline(xintercept = c(19,23), linetype = 2)
# 
# ggplot(migration_rates_ethagesex , aes(Age, out_rate_as, colour = sex))+
#   geom_line() +
#   facet_wrap(~eth_code)
# 
#   
# ggplot(migration_rates_ethagesex , aes(Age, in_rate_as, colour = sex))+
#   geom_line() +
#   facet_wrap(~eth_code)
# 
# 
# eth_order = c("WBI", "WHO", "MIX",           # White + Mixed
#               "IND", "PAK", "BAN", "CHI", "OAS",  # Asian
#               "BLA", "BLC", "OBL",           # Black
#               "OTH")                          # Other
# 
# migration_rates_allages %>% 
#   pivot_longer(cols = -eth_code,
#                values_to = "Rate",
#                names_to = "Type_of_rate") %>% 
#   mutate(eth_code = factor(eth_code, levels = rev(eth_order))) %>% 
#   ggplot(aes(x=eth_code, y = Rate, fill= Type_of_rate))+
#   geom_col()+
#   coord_flip()+
#   theme_minimal()+
#   facet_wrap(~Type_of_rate,scales = "free")+
#   theme(legend.position = "none")












