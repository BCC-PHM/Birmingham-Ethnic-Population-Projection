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
  tail_df <- data.frame(
    eth_code = this_eth,
    DEC_SEX  = this_sex,
    age      = 90:100,
    qx       = 1 - exp(-out[as.character(90:100)])
  )
  
  tail_list[[i]] =  tail_df
}

# stack all 24 tails into one data frame
kannisto_tail = bind_rows(tail_list)


# join relational 0-89 with Kannisto 90-100
eth_single_qx_0_100 = bind_rows(eth_single_qx_0_89, kannisto_tail) %>%
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
  mutate(mx = -log(1-qx)) %>% 
  cross_join(improvement_rate) %>%
  mutate(
    mx_future = mx * cum_factor,     # this age's base mx, cumulatively improved
    qx_future = 1 - exp(-mx_future)  # back to qx
  ) %>% 
  select(DEC_SEX, eth_code, age, year, qx_future) %>% 
  group_by(eth_code, DEC_SEX,year) %>%
  arrange(age, .by_group = TRUE) %>%
  mutate(
    ax = case_when(age == 0 ~ 0.07,
                   age == max(age) ~ NA_real_,   # open interval, handled below
                   TRUE ~ 0.5),
    lx = 100000 * cumprod(lag(1 - qx_future, default = 1)),
    dx = lx * qx_future,
    mx = -log(1 - qx_future),
    Lx = if_else(age == max(age),
                 lx / mx,                          # open-age person-years
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

write_csv(eth_single_qx_0_100_future, "data/processed/01_02_Birmingham_mortality_rate_0_100_projected.csv")



