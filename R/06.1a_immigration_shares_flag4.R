setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 06.1a_immigration_shares_flag4.R
#the cohort component engine the project the population
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(bcctheme)
library(sf)

#full flag4 data
all_flag4_2013_2022 = read_csv("data/migration/flag4_related/all_flag4_2013_2022.csv")
#lsoa11
lsoa11 = read_sf("data/boundaries/Birmingham LSOA map.json")
#====================================================================================
#modified country has no missing so i will use that to map it to ons 22 categroy 
check_missing = all_flag4_2013_2022 %>%summarise(across(everything(), ~sum(is.na(.))))


#output to this to map to matching the ons 22 category 
distinct_country_modified = all_flag4_2013_2022 %>% 
   distinct(birth_modified_country = all_flag4_2013_2022$`PLACE OF BIRTH_MODIFIED_COUNTRY`)

write_csv(distinct_country_modified, "data/migration/flag4_related/distinct_country_modified.csv")
#====================================================================================
#customised data from ons, need to calucalte the shares 
country_of_birth_birm = read_csv("data/migration/flag4_related/country-of-birth_birm.csv")



country_of_birth_birm_shares = country_of_birth_birm %>% 
  mutate(eth_newethpop = case_when(
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
  )) %>% 
  drop_na() %>% 
  group_by(`Country of birth (22 categories)`,`Country of birth (22 categories) Code`,eth_code) %>% 
  summarise(Observation = sum(Observation),
            .groups = "drop") %>% 
  group_by(`Country of birth (22 categories)`,`Country of birth (22 categories) Code`) %>% 
  mutate(denominator = sum(Observation)) %>% 
  ungroup() %>% 
  mutate(shares = Observation/denominator) %>% 
  rename(ons_country_of_birth_22_code=`Country of birth (22 categories) Code`,
         ons_country_of_birth_22 = `Country of birth (22 categories)`)




#====================================================================================
matched_distinct_country_modified = read_csv("data/migration/flag4_related/distinct_country_modified_with_ons_22.csv")

colnames(all_flag4_2013_2022) = paste0("X", colnames(all_flag4_2013_2022))
colnames(all_flag4_2013_2022) = gsub(" ", "_", colnames(all_flag4_2013_2022))

processed_all_flag4_2013_2022 = all_flag4_2013_2022 %>% 
  filter(X2011_LSOA_CODE %in% c(lsoa11$LSOA11CD)|XLA_Code == "E08000025") %>% 
  left_join(matched_distinct_country_modified, by = c("XPLACE_OF_BIRTH_MODIFIED_COUNTRY" = "birth_modified_country")) %>% 
  group_by(XYEAR,ons_country_of_birth_22_code,ons_country_of_birth_22) %>% 
  summarise(count =n(),
            .groups = "drop") %>% 
  group_by(XYEAR) %>% 
  mutate(flag4denominator = sum(count)) %>% 
  ungroup() %>% 
  left_join(country_of_birth_birm_shares , by =c("ons_country_of_birth_22_code", "ons_country_of_birth_22") ) %>% 
  arrange(XYEAR,ons_country_of_birth_22,eth_code) %>% 
  mutate(redistributed_count = count*shares) %>% 
  group_by(XYEAR,eth_code) %>% 
  summarise(redistributed_count = sum(redistributed_count),
            .groups = "drop") %>% 
  group_by(XYEAR) %>% 
  mutate(denominator_check = sum(redistributed_count)) %>% 
  ungroup() %>% 
  mutate(ethnic_shares = redistributed_count/denominator_check) 
  
processed_all_flag4_2013_2022 %>% 
  ggplot(aes(
    x = XYEAR,
    y = ethnic_shares,
    group = eth_code,
    colour = eth_code
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = sort(unique(processed_all_flag4_2013_2022$XYEAR))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Estimated ethnic distribution of Flag 4 migrants",
    subtitle = "Birmingham, by year",
    x = "Year",
    y = "Ethnic share",
    colour = "Ethnic group"
  ) +
  theme_minimal()

#=========================================================
#pooling 2017 to 2022 to be a long run shares 


long_run_shares = processed_all_flag4_2013_2022  %>%
  filter(XYEAR >= 2019, XYEAR <= 2022) %>%
  group_by(eth_code) %>%
  summarise(
    redistributed_count = sum(redistributed_count),
    .groups = "drop"
  ) %>%
  mutate(
    long_run_share = redistributed_count / sum(redistributed_count)
  )

start_shares = processed_all_flag4_2013_2022 %>%
  filter(XYEAR == 2022) %>%
  select(eth_code, redistributed_count, start_share = ethnic_shares)


share_targets = start_shares %>%
  left_join(long_run_shares,
            by = "eth_code")
#---------------------------------------------------
#produce annual shares
#converge in 2028
years = tibble(XYEAR = 2022:2061)

projected_ethnic_shares = share_targets %>%
  tidyr::crossing(XYEAR = 2022:2061) %>%
  mutate(lambda = ifelse(XYEAR %in% c(2022:2028),
                         (XYEAR-2022)/6,
                         1),
         projected_share = (1 - lambda)*start_share+lambda*long_run_share ) %>% 
  select(eth_code,year = XYEAR,projected_share)

projected_ethnic_shares_final =processed_all_flag4_2013_2022 %>%
  filter(XYEAR == 2021) %>%
  select(eth_code, year=XYEAR, projected_share = ethnic_shares) %>% 
  rbind(projected_ethnic_shares) %>% 
  arrange(eth_code,year)

projected_ethnic_shares_final %>% 
  filter(year %in% c(2019:2029)) %>% 
  mutate(year = as.character(year)) %>% 
  ggplot(aes(
    x = year,
    y = projected_share,
    group = eth_code,
    colour = eth_code
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Estimated ethnic distribution of Flag 4 migrants",
    subtitle = "Birmingham, by year",
    x = "Year",
    y = "Ethnic share",
    colour = "Ethnic group"
  ) +
  theme_bcc()

#---------------------------------------------------
#for now without further newer flag 4 i will hold 
#every future year constant at 2022 level

projected_ethnic_shares_constant = processed_all_flag4_2013_2022  %>%
  filter(XYEAR ==2022) %>% 
  select(-XYEAR) %>% 
  crossing(year = 2022:2061) %>% 
  select(eth_code, year, ethnic_shares) %>% 
  rbind(processed_all_flag4_2013_2022  %>%
          filter(XYEAR ==2021) %>% 
          select(eth_code, year = XYEAR, ethnic_shares)) %>% 
  arrange(eth_code, year)



flag_4_immigration_shares =
  projected_ethnic_shares_final %>%
  transmute(
    eth_code,
    year,
    ethnic_shares = projected_share
  ) %>%
  arrange(eth_code, year)



write_rds(flag_4_immigration_shares, "data/processed/061a_immigration_shares.rds")







