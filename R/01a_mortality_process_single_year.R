setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 01a_mortality_process_single_year.R
#relational method the smooth the mortality rate to single year of age 
# ============================================================
library(tidyverse)
library(readr)
library(sf)
library(readxl)

national_lifetable = read_excel("data/mortality/UK_national_life_table_2022_24.xlsx")
bham_age_sex_life_table_2022_2024_90plus = read_csv("data/processed/bham_age_sex_life_table_2022_2024_90plus.csv")

abridged_life_table = read_csv("data/processed/life_table.csv")

#------------------------------------------
#prepare national single-age standard
bham_reference_qx = national_lifetable %>%
  transmute(
    DEC_SEX = Gender,
    age = as.integer(age),
    qx_standard = qx
  ) %>%
  filter(age >= 1, age <= 89)

#------------------------------------------
#Prepare ethnic abridged qx
eth_abridged_qx = abridged_life_table %>%
  transmute(
    eth_code,
    DEC_SEX,
    age_group_5yr = as.character(age_group_5yr),
    qx_n_local = qx,
    mx_abridged = mx
  )


#------------------------------------------
#separate age =0
eth_q0 = eth_abridged_qx %>%
  filter(age_group_5yr == "0") %>%
  transmute(
    eth_code,
    DEC_SEX,
    age = 0,
    qx = qx_n_local,
    qx_n_local,
    age_group_5yr,
    K = NA_real_,
    qx_standard = NA_real_,
    mx_open = NA_real_,
    source = "ethnic_q0_EB"
  )
#-----------------------------------------
#Prepare closed intervals for relational expansion

eth_closed_intervals = eth_abridged_qx %>%
  filter(!age_group_5yr %in% c("0", "90+")) %>%
  mutate(
    age_start = case_when(
      age_group_5yr == "1-4" ~ 1,
      TRUE ~ as.integer(str_extract(age_group_5yr, "^[0-9]+"))
    ),
    #interval
    n = case_when(
      age_group_5yr == "1-4" ~ 4,
      TRUE ~ 5
    ),
    age_end = age_start + n - 1
  ) %>%
  select(
    eth_code,
    DEC_SEX,
    age_group_5yr,
    age_start,
    age_end,
    n,
    qx_n_local
  ) %>% 
  mutate(log_sx = log(1-qx_n_local))


options(scipen = 999)

relational_expand = bham_reference_qx %>% 
  mutate(age_group_5yr = case_when(
    age %in% 1:4 ~ "1-4",
    age %in% 5:9 ~ "5-9",
    age %in% 10:14 ~ "10-14",
    age %in% 15:19 ~ "15-19",
    age %in% 20:24 ~ "20-24",
    age %in% 25:29 ~ "25-29",
    age %in% 30:34 ~ "30-34",
    age %in% 35:39 ~ "35-39",
    age %in% 40:44 ~ "40-44",
    age %in% 45:49 ~ "45-49",
    age %in% 50:54 ~ "50-54",
    age %in% 55:59 ~ "55-59",
    age %in% 60:64 ~ "60-64",
    age %in% 65:69 ~ "65-69",
    age %in% 70:74 ~ "70-74",
    age %in% 75:79 ~ "75-79",
    age %in% 80:84 ~ "80-84",
    age %in% 85:89 ~ "85-89"
  ),
  sx = 1-qx_standard)%>%
  group_by(DEC_SEX,age_group_5yr) %>% 
  mutate(log_prod_sx = sum(log(sx))) %>% 
  ungroup() %>% 
  left_join(eth_closed_intervals, by = c("DEC_SEX", "age_group_5yr"), relationship = "many-to-many") %>% 
  arrange(DEC_SEX, age,desc(eth_code))%>% 
  mutate(nKx = log_sx/log_prod_sx,
         qx = 1-(1-qx_standard)^nKx) %>% 
  select(DEC_SEX, eth_code , age,qx)
    
  
#------------------------------------------
# relational output as age 0–89 only

eth_single_qx_0_89 = bind_rows(
  eth_q0 %>% select(DEC_SEX, eth_code , age,qx),
  relational_expand 
) %>%
  arrange(eth_code, DEC_SEX, age)


ggplot()+
  geom_line(data=eth_single_qx_0_89, aes(x=as.numeric(age), y=qx, colour = eth_code))+
  geom_line(data=national_lifetable, aes(x=as.numeric(age),y=qx))+
  facet_wrap(~DEC_SEX)

ggplot() +
  geom_line(data = eth_single_qx_0_89,aes(age, qx, colour = eth_code, group = interaction(eth_code, DEC_SEX))) +
  geom_line(data = national_lifetable %>% mutate(DEC_SEX = Gender), aes(age, qx, group = Gender)) +
  facet_wrap(~DEC_SEX)

#-==========================================================
#apply Kannisto model to extrapolate qx for 90 and above
#-==========================================================
library(MortCast)

groups  = eth_single_qx_0_89 %>%
  distinct(eth_code, DEC_SEX)

# empty list to collect each group's tail
tail_list = list()


for (i in 1:nrow(groups)){
  
  this_eth = groups$eth_code[i]
  this_sex = groups$DEC_SEX[i]
  
  one = eth_single_qx_0_89 %>% 
    filter(DEC_SEX == this_sex, eth_code == this_eth) %>% 
    arrange(age) %>%
    mutate(mx = -log(1 - qx))
  
  mx_vec = one$mx
  names(mx_vec) = one$age
  
  # run Kannisto  returns mx extended to 100
  out = kannisto(mx_vec, est.ages = 80:89, proj.ages = 90:100)
  
  # keep only 90:100
  tail_df = data.frame(
    eth_code = this_eth,
    DEC_SEX  = this_sex,
    age      = 90:100,
    mx_kan       = 1 - exp(-out[as.character(90:100)])
  )%>%
    mutate(qx = if_else(age == 100, 1, 1 - exp(-mx_kan)))
  
  tail_list[[i]] =  tail_df
}

# stack all 24 tails into one data frame
kannisto_tail = bind_rows(tail_list)


# join relational 0-89 with Kannisto 90-100
eth_single_qx_0_100 = bind_rows(eth_single_qx_0_89%>% mutate(mx_kan = NA_real_), kannisto_tail) %>%
  arrange(eth_code, DEC_SEX, age)

#=======================================================================
#project future morality rate, gradual decline for first 25 year
# then held constant for 1  like ONS


improvement_rate = data.frame(
  year = 2021:2061,
  steps = 0:40
)

improvement_rate = improvement_rate %>% 
  mutate(r = ifelse(steps<=25,
                    0.01*steps/25,
                    0.01),
         cum_factor = cumprod(1 - r) )


eth_single_qx_0_100_future = eth_single_qx_0_100 %>%
  mutate(mx_base = if_else(age == 100, mx_kan, -log(1 - qx))) %>%
  cross_join(improvement_rate) %>%
  mutate(
    mx_future = mx_base * cum_factor,
    qx_future = if_else(age == 100, 1, 1 - exp(-mx_future))
  ) %>%
  select(DEC_SEX, eth_code, age, year, qx_future, mx_future) %>%
  group_by(eth_code, DEC_SEX, year) %>%
  arrange(age, .by_group = TRUE) %>%
  mutate(
    ax = case_when(age == 0 ~ 0.07,
                   age == max(age) ~ NA_real_,
                   TRUE ~ 0.5),
    lx = 100000 * cumprod(lag(1 - qx_future, default = 1)),
    dx = lx * qx_future,
    mx = mx_future,
    Lx = if_else(age == max(age),
                 lx / mx_future,
                 lead(lx) + ax * dx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  ) %>%
  ungroup()
  

eth_single_qx_0_100_future %>% 
  filter(age ==0) %>% 
  ggplot(aes(x=year,y=ex,colour = eth_code))+
  geom_line()+
  facet_wrap(~DEC_SEX )





birth_survival = eth_single_qx_0_100_future %>%
  filter(age == 0) %>%
  transmute(
    year, eth_code, DEC_SEX,
    birth_survival = Lx / 100000
  )




mortality_input = eth_single_qx_0_100_future %>%
  arrange(year, eth_code, DEC_SEX, age) %>%
  group_by(year, eth_code, DEC_SEX) %>%
  mutate(
    survival_probability = case_when(
      age <= 98  ~ lead(Lx) / Lx,
      age == 99  ~ lead(lx) / lx,
      age == 100 ~ 1 - lx / Lx
    ),
    mx_for_deaths = mx
  ) %>%
  ungroup() %>%
  select(year, eth_code, DEC_SEX, age, survival_probability, mx_for_deaths) %>%
  left_join(birth_survival, by = c("year", "eth_code", "DEC_SEX"))




write_csv(mortality_input, "data/processed/01_02_Birmingham_mortality_rate_0_100_projected.csv")



broad_group_map = tibble::tribble(
  ~eth_code, ~ethnic_group,          ~broad_group, ~broad_colours, 
  "WBI",     "White British",        "White",           "blue",
  "WHO",     "White Other",          "White",           "blue",
  "MIX",     "Mixed",                "Mixed",           "green",
  "IND",     "Indian",               "Asian",           "orange",
  "PAK",     "Pakistani",            "Asian",           "orange",
  "BAN",     "Bangladeshi",          "Asian",           "orange",
  "CHI",     "Chinese",              "Asian",           "orange",
  "OAS",     "Other Asian",          "Asian",           "orange",
  "BLA",     "Black African",        "Black",           "purple",
  "BLC",     "Black Caribbean",      "Black",           "purple",
  "OBL",     "Other Black",          "Black",           "purple",
  "OTH",     "Other ethnic group",   "Other",            "pink"
)



national_ref = national_lifetable %>% 
  mutate(DEC_SEX = Gender) %>% 
  select(DEC_SEX, age, qx)

#---------------------------------------------------
#base map
#---------------------------------------------------
broad_group_map = tibble::tribble(
  ~eth_code, ~ethnic_group,          ~broad_group, ~broad_colours, 
  "WBI",     "White British",        "White",           "blue",
  "WHO",     "White Other",          "White",           "blue",
  "MIX",     "Mixed",                "Mixed",           "green",
  "IND",     "Indian",               "Asian",           "orange",
  "PAK",     "Pakistani",            "Asian",           "orange",
  "BAN",     "Bangladeshi",          "Asian",           "orange",
  "CHI",     "Chinese",              "Asian",           "orange",
  "OAS",     "Other Asian",          "Asian",           "orange",
  "BLA",     "Black African",        "Black",           "purple",
  "BLC",     "Black Caribbean",      "Black",           "purple",
  "OBL",     "Other Black",          "Black",           "purple",
  "OTH",     "Other ethnic group",   "Other",            "pink"
)

#---------------------------------------------------
#add harmonised shades + factor ordering
#---------------------------------------------------
broad_group_map = broad_group_map %>% 
  group_by(broad_colours) %>% 
  mutate(shade_number = row_number(),
         harmonised_colour = bcc_pal(palette = first(broad_colours))(n() + 2)[shade_number]) %>% 
  ungroup() %>% 
  mutate(broad_colours_hex = bcc_cols(broad_colours)) %>% 
  mutate(broad_group = factor(broad_group, levels = c("White","Mixed","Asian","Black","Other")),
         eth_code = factor(eth_code, levels = c(
           "WBI","WHO","MIX","IND","PAK","BAN","CHI","OAS","BLA","BLC","OBL","OTH"))) %>% 
  arrange(broad_group, eth_code)
#---------------------------------------------------
#build named colour + label vectors from the broad-group map
#---------------------------------------------------
eth_colours = setNames(broad_group_map$harmonised_colour, broad_group_map$eth_code)
eth_labels  = setNames(broad_group_map$ethnic_group,      broad_group_map$eth_code)
eth_order   = levels(broad_group_map$eth_code)

ggplot() +
  geom_line(data = mortality_input %>% 
              filter(year == 2021) %>% 
              mutate(eth_code = factor(eth_code, levels = eth_order)),
            aes(age, 1 - survival_probability, colour = eth_code,
                group = interaction(eth_code, DEC_SEX)),
            linewidth = 0.45, alpha = 0.85) +
  geom_line(data = national_lifetable %>% mutate(DEC_SEX = Gender),
            aes(age, qx, group = DEC_SEX),
            colour = "grey30", linewidth = 0.9) +
  facet_wrap(~DEC_SEX) +
  scale_y_log10(labels = scales::label_number()) +
  scale_colour_manual(values = eth_colours,
                      breaks = eth_order) +
  labs(title = "Baseline single-year mortality schedules by ethnic group and sex, Birmingham",
       subtitle = "Ethnic rates pooled from 2022-2024 death registrations; black line: national life table 2022-2024",
       x = "Age", y = expression(q[x]~(log~scale)),
       colour = "Ethnic group") +
  theme_bcc(base_size = 11)







life_expectancy = mortality_input %>% 
  arrange(year, eth_code, DEC_SEX, age) %>% 
  group_by(year, eth_code, DEC_SEX) %>% 
  mutate(
    ax = case_when(age == 0        ~ 0.07,
                   age == max(age) ~ NA_real_,
                   TRUE            ~ 0.5),
    qx = if_else(age == max(age), 1, 1 - exp(-mx_for_deaths)),
    lx = 100000 * cumprod(lag(1 - qx, default = 1)),
    dx = lx * qx,
    Lx = if_else(age == max(age),
                 lx / mx_for_deaths,
                 lead(lx) + ax * dx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx / lx
  ) %>% 
  ungroup()



life_expectancy_table = life_expectancy %>% 
  filter(
    year %in% c(2022, 2032, 2047),
    age == 0
  ) %>% 
  select(eth_code, DEC_SEX, year, ex) %>% 
  pivot_wider(
    names_from  = c(DEC_SEX, year),
    values_from = ex
  ) %>% 
  select(
    
    eth_code,
    Female_2022, Female_2032, Female_2047,
    Male_2022, Male_2032, Male_2047
  )

life_expectancy_table %>% 
  gt(rowname_col = "eth_code") %>% 
  tab_spanner(
    label   = "Female",
    columns = c(Female_2022, Female_2032, Female_2047)
  ) %>% 
  tab_spanner(
    label   = "Male",
    columns = c(Male_2022, Male_2032, Male_2047)
  ) %>% 
  cols_label(
    Female_2022 = "2022",
    Female_2032 = "2032",
    Female_2047 = "2047",
    Male_2022   = "2022",
    Male_2032   = "2032",
    Male_2047   = "2047"
  ) %>% 
  tab_stubhead(label = "Ethnic group") %>% 
  fmt_number(
    columns  = everything(),
    decimals = 1
  )





print(life_expectancy %>% 
  filter(year %in% c(2022, 2032, 2047), age == 0) %>% 
  select(eth_code, DEC_SEX, year, ex) %>% 
  pivot_wider(names_from = year, values_from = ex,
              names_prefix = "e0_") %>% 
  arrange(DEC_SEX, eth_code),n=24)
  


