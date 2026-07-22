setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 09_bham_starting_pop.R
# use RM032
# ============================================================

eth_pop = read_csv("data/RM032_LA_ethnic_pop_age_sex_2021.csv")



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


write_rds(bham_eth_pop, "data/processed/09_bham_eth_pop.rds")








