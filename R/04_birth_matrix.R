setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 04_birth_matrix.R
# process MSDS data 
# caculate birth transition matrix 
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(sf)


ward_map = st_read("data/boundaries/boundaries-wards-2022-birmingham/boundaries-wards-2022-birmingham.shp")

MSDS_data =  read_delim("data/fertiliy/Fertility_data.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)


MSDS_data_filtered = MSDS_data %>% 
  filter(YearOfBirthBaby %in% c(2022,2023,2024,2025)) %>% 
  filter(ElectoralWardMother %in% ward_map$Ward_Code)%>%
  mutate(eth_code_mother = case_when(
    EthnicCategoryMother %in% c("A", "B")             ~ "WBI",
    EthnicCategoryMother == "C"                        ~ "WHO",
    EthnicCategoryMother %in% c("D", "E", "F", "G")  ~ "MIX",
    EthnicCategoryMother == "H"                        ~ "IND",
    EthnicCategoryMother == "J"                        ~ "PAK",
    EthnicCategoryMother == "K"                        ~ "BAN",
    EthnicCategoryMother == "R"                        ~ "CHI",
    EthnicCategoryMother == "L"                        ~ "OAS",
    EthnicCategoryMother == "N"                        ~ "BLA",
    EthnicCategoryMother == "M"                        ~ "BLC",
    EthnicCategoryMother == "P"                        ~ "OBL",
    EthnicCategoryMother == "S"                        ~ "OTH",
    EthnicCategoryMother %in% c("Z", "99")            ~ NA_character_
  ),
  eth_code_baby = case_when(
    EthnicCategoryBaby %in% c("A", "B")             ~ "WBI",
     EthnicCategoryBaby == "C"                        ~ "WHO",
     EthnicCategoryBaby %in% c("D", "E", "F", "G")  ~ "MIX",
     EthnicCategoryBaby == "H"                        ~ "IND",
     EthnicCategoryBaby == "J"                        ~ "PAK",
     EthnicCategoryBaby == "K"                        ~ "BAN",
     EthnicCategoryBaby == "R"                        ~ "CHI",
     EthnicCategoryBaby == "L"                        ~ "OAS",
     EthnicCategoryBaby == "N"                        ~ "BLA",
     EthnicCategoryBaby == "M"                        ~ "BLC",
     EthnicCategoryBaby == "P"                        ~ "OBL",
     EthnicCategoryBaby == "S"                        ~ "OTH",
     EthnicCategoryBaby %in% c("Z", "99")            ~ NA_character_
  )) %>% 
  filter(!is.na(eth_code_mother), !is.na(eth_code_baby))%>%
  mutate( age_group_5yr = case_when(
    AgeRPEndDate >= 0  & AgeRPEndDate <= 4  ~ "0-4",
    AgeRPEndDate >= 5  & AgeRPEndDate <= 9  ~ "5-9",
    AgeRPEndDate >= 10 & AgeRPEndDate <= 14 ~ "10-14",
    AgeRPEndDate >= 15 & AgeRPEndDate <= 19 ~ "15-19",
    AgeRPEndDate >= 20 & AgeRPEndDate <= 24 ~ "20-24",
    AgeRPEndDate >= 25 & AgeRPEndDate <= 29 ~ "25-29",
    AgeRPEndDate >= 30 & AgeRPEndDate <= 34 ~ "30-34",
    AgeRPEndDate >= 35 & AgeRPEndDate <= 39 ~ "35-39",
    AgeRPEndDate >= 40 & AgeRPEndDate <= 44 ~ "40-44",
    AgeRPEndDate >= 45 & AgeRPEndDate <= 49 ~ "45-49",
    AgeRPEndDate >= 50 & AgeRPEndDate <= 54 ~ "50-54",
    AgeRPEndDate >= 55 & AgeRPEndDate <= 59 ~ "55-59",
    AgeRPEndDate >= 60 & AgeRPEndDate <= 64 ~ "60-64",
    AgeRPEndDate >= 65 & AgeRPEndDate <= 69 ~ "65-69",
    AgeRPEndDate >= 70 & AgeRPEndDate <= 74 ~ "70-74",
    AgeRPEndDate >= 75 & AgeRPEndDate <= 79 ~ "75-79",
    AgeRPEndDate >= 80 & AgeRPEndDate <= 84 ~ "80-84",
    AgeRPEndDate >= 85 & AgeRPEndDate <= 89 ~ "85-89",
    AgeRPEndDate >= 90             ~ "90+",
    TRUE ~ NA_character_
  )) %>% 
  filter(age_group_5yr %in%c (c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")))



# complete the matrix with zeros for missing combinations
mixing_matrix_complete = MSDS_data_filtered %>%
  count(eth_code_mother,
        eth_code_baby) %>% 
  complete(eth_code_mother, eth_code_baby, 
           fill = list(n = 0, percentage = 0)) %>%
  group_by(eth_code_mother) %>%
  mutate(
    mother_base = sum(n, na.rm=TRUE),
    percentage  = n / mother_base
  ) %>% 
  print(n = 144)


write.csv(mixing_matrix_complete, "data/processed/04_birth_transition_matrix.csv")


# mixing_matrix_complete %>%
#   group_by(eth_code_mother) %>%
#   summarise(total = round(sum(percentage), 6)) %>%
#   filter(total != 1)

bcctheme::bcc_pal("pink")(5)[5]

mixing_matrix_complete = mixing_matrix_complete %>% 
  mutate(percentage = round(percentage*100,1),
         group = case_when(
           percentage < 1  ~ "<1%",
           percentage >= 1  & percentage < 25 ~ "1% - <25%",
           percentage >= 25 & percentage < 50 ~ "25% - <50%",
           percentage >= 50 & percentage < 80 ~ "50% - <80%",
           percentage >= 80 ~ ">=80%"
         ),
         group = factor(group, levels = c("<1%", "1% - <25%", "25% - <50%", "50% - <80%", ">=80%")),
         eth_code_mother = factor(eth_code_mother, levels = rev(c(
           "WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH"))),
         eth_code_baby = factor(eth_code_baby, levels = rev(c(
           "WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH"))))


ggplot(mixing_matrix_complete, aes(eth_code_baby,eth_code_mother, fill=group,label = paste0(percentage, "%")))+
  geom_tile(color = "grey92")+
  geom_text()+
  scale_fill_manual(values = c("<1%" = "white",
                               "1% - <25%"  = bcctheme::bcc_pal("purple")(5)[5],
                               "25% - <50%" = bcctheme::bcc_pal("purple")(5)[4],
                               "50% - <80%" = bcctheme::bcc_pal("purple")(5)[3],
                               ">=80%" = bcctheme::bcc_pal("purple")(5)[2])) +
  labs(x = "12 harmonised ethnic group of mother", y = "12 harmonised ethnic group of newborn", 
       fill = "Percentage", 
       title="Mother-to-child ethnic transfer matrix"
      ) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_fixed()+
  theme_bcc(gridline_x = F,
            gridline_y = F,
            base_size = 11)+
  theme(legend.title = element_text(size = 11,
                                    color = bcc_cols("black")))













