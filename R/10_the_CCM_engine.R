setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 10_the_CCM_engine.R
#the cohort component engine the project the population
# ============================================================
library(tidyverse)
library(readr)
library(readxl)

inputfiles = list.files(path = "data/processed", pattern = "^[0-9]", full.names = TRUE)

# ============================================================
#doing the ccm engine by running 1 cycle for testing
# ============================================================
#-------------------------------------------------------------
#prepare fertility
fertility = read_csv(inputfiles[2])

fertility = fertility %>% 
  select(year = Year, eth_code, age = single_age, fx) %>% 
  arrange(year, eth_code, age) %>% 
  group_by(year, eth_code) %>% 
  mutate(fx_plus1 = lead(fx, default = 0),
         fc = 0.5 * (fx + fx_plus1)) %>% 
  ungroup()


#-------------------------------------------------------------
#starting population which is 2021
bham_eth_pop  = read_rds(inputfiles[8])

start_population = bham_eth_pop %>% 
  mutate(year = 2021) %>% 
  rename(population = Observation) %>% 
  select(year, eth_code, sex=SEX, age=Age, population) %>% 
  arrange(eth_code, sex,age)
#-------------------------------------------------------------
#calculate births at the start 
births_by_mother_age = start_population %>%
  filter(sex == "Female",
         age>=15,
         age<=49) %>% 
  left_join(fertility %>% select(year, eth_code, age, fc),
            by = c("year", "eth_code", "age")) %>% 
  mutate(births = population * fc)


births_by_mother_ethnicity = births_by_mother_age %>%
  group_by(year,eth_code_mother = eth_code) %>%
  summarise(births = sum(births, na.rm = TRUE),
    .groups = "drop")

#-------------------------------------------------------------
#load the baby eth matrix 
birth_transition_matrix = read_csv(inputfiles[3])

birth_transition_matrix =birth_transition_matrix %>% 
  select(eth_code_mother,eth_code_baby,percentage)
  
#apply the transition matrix  
births_by_child_ethnicity = births_by_mother_ethnicity %>% 
  left_join(birth_transition_matrix, by ="eth_code_mother") %>% 
  mutate(births_child = births * percentage) %>% 
  group_by(year, eth_code = eth_code_baby)%>%
  summarise( births = sum(births_child, na.rm = TRUE),
             .groups = "drop") 

#check if the number of births the same
sum(births_by_mother_ethnicity$births)

sum(births_by_child_ethnicity$births)
  
#divide to male and female using the ons assumption of 
#male 0.512 and female 0.488
births_by_sex_ethnicity = births_by_child_ethnicity %>%
  tidyr::crossing(
    sex = c("Male", "Female")
  ) %>%
  mutate(
    sex_share = case_when(
      sex == "Male"   ~ 0.512,
      sex == "Female" ~ 0.488
    ),
    births = births * sex_share
  )
#------------------------------------------------------------- 
#apply the newborn survival ratio

#mortality 
mortality = read_csv(inputfiles[1])

mortality = mortality %>% 
  rename(sex = DEC_SEX)

newborn_survival = mortality%>%
  filter(age == 0) %>%
  transmute(
    year,
    eth_code,
    sex ,
    birth_survival
  )

newborn_population = births_by_sex_ethnicity %>%
  left_join(newborn_survival,by = c("year", "eth_code", "sex")) %>% 
  mutate(surviving_newborns = births * birth_survival,
         projection_year = year + 1,
         age = 0) %>%
  transmute(
    year = projection_year,
    eth_code,
    sex,
    age,
    population = surviving_newborns
  )

#-------------------------------------------------------------
#survive and age forward the existing population

#join start population to survival_probability
surviving_population = start_population %>% 
  left_join(mortality, by = c("year", "eth_code","sex", "age"),relationship = "many-to-one") %>% 
  mutate(population = population*survival_probability,
         year = year + 1,
         age = if_else(age >= 99, 100, age + 1))%>%
  group_by(year, eth_code, sex, age) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  ) %>% 
  select(year,eth_code, sex,age,population)


#combine the existing survivors with the newborn population

population_before_migration = bind_rows(
  surviving_population,
  newborn_population
) %>%
  arrange(year, eth_code, sex, age)


tibble(
  component = c(
    "2021 starting population",
    "2022 surviving existing population",
    "2022 surviving newborns",
    "2022 population before migration"
  ),
  population = c(
    sum(start_population$population),
    sum(surviving_population$population),
    sum(newborn_population$population),
    sum(population_before_migration$population)
  )
)

#-------------------------------------------------------------
international_emmigration_flow = read_csv(inputfiles[6])

international_emigration = international_emmigration_flow%>%
  transmute(
    year = Year,
    eth_code,
    sex,
    age = Age,
    international_emigrants = emmigration_count
  ) %>% 
  mutate(year = year+1,
         age = pmin(age + 1, 100))%>%
  group_by(year, eth_code, sex, age) %>%
  summarise(
    international_emigrants = sum(international_emigrants),
    .groups = "drop"
  )

#-------------------------------------------------------------
# subtract international emigration

within_country_survivors_bham = population_before_migration %>%
  left_join( international_emigration,
             by = c("year", "eth_code", "sex", "age"),
             relationship = "one-to-one")%>%
  mutate(
    international_emigrants =
      replace_na(international_emigrants, 0),
    WS_B = population - international_emigrants)

#-------------------------------------------------------------
# apply internal out-migration from Birmingham
internal_migration = read_csv(inputfiles[4])
internal_migration = internal_migration %>% 
  rename(age = Age)

population_after_internal_out = within_country_survivors_bham %>%
  left_join(internal_migration %>% select(-in_rate_as, -IN_B_as),
            by =  c("eth_code", "sex", "age")) %>% 
  mutate(internal_out_migrants = WS_B * out_rate_as,
         population_after_internal_out = WS_B - internal_out_migrants)





