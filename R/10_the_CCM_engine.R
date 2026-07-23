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
# Prepare all model inputs once
# ============================================================
#prepare fertility
fertility = read_csv(inputfiles[2]) %>%
  select(year = Year, eth_code, age = single_age, fx) %>%
  arrange(year, eth_code, age) %>%
  group_by(year, eth_code) %>%
  mutate(
    fx_plus1 = lead(fx, default = 0),
    fc = 0.5 * (fx + fx_plus1)
  ) %>%
  ungroup()


#load the baby eth matrix 
birth_transition_matrix = read_csv(inputfiles[3]) %>%
  select(eth_code_mother, eth_code_baby, percentage)


mortality = read_csv(inputfiles[1]) %>%
  rename(sex = DEC_SEX)

internal_migration = read_csv(inputfiles[4]) %>%
  rename(age = Age)

projected_REW = read_rds(inputfiles[7]) %>%
  rename(age = Age)

international_immigration = read_csv(inputfiles[5]) %>%
  transmute(
    year = Year,
    eth_code,
    sex,
    age = as.integer(Age),
    international_immigrants = immigration_count
  )

international_emigration = read_csv(inputfiles[6]) %>%
  transmute(
    year = Year,
    eth_code,
    sex,
    age = as.integer(Age),
    international_emigrants = emmigration_count
  )


#Initialise the 2021 starting population once
bham_eth_pop = read_rds(inputfiles[8])

start_population = bham_eth_pop %>%
  transmute(
    year = 2021,
    eth_code,
    sex = SEX,
    age = Age,
    population = Observation
  ) %>%
  arrange(eth_code, sex, age)


#Initialise output lists
projection_list = list()
birth_projection_list = list()
death_projection_list = list()
migration_projection_list = list()
component_projection_list = list()


projection_list[["2021"]] = start_population

for (current_year in 2021:2046) {

  projection_year = current_year + 1

# ============================================================
#Births
# ============================================================
  births_by_mother_age = start_population %>%
    filter(sex == "Female",
           age>=15,
           age<=49) %>% 
    left_join(fertility %>% filter(year == current_year) %>% select(year, eth_code, age, fc),
              by = c("year", "eth_code", "age")) %>% 
    mutate(births = population * fc)
  
  
  births_by_mother_ethnicity = births_by_mother_age %>%
    group_by(year,eth_code_mother = eth_code) %>%
    summarise(births = sum(births, na.rm = TRUE),
              .groups = "drop")
  
  #apply the transition matrix  
  births_by_child_ethnicity = births_by_mother_ethnicity %>% 
    left_join(birth_transition_matrix, by ="eth_code_mother") %>% 
    mutate(births_child = births * percentage) %>% 
    group_by(year, eth_code = eth_code_baby)%>%
    summarise( births = sum(births_child, na.rm = TRUE),
               .groups = "drop") 
  
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
# ==========================================================
# Newborn mortality
# ==========================================================
  
  newborn_survival = mortality%>%
    filter(
      year == current_year,
      age == 0) %>%
    transmute(
      year,
      eth_code,
      sex ,
      birth_survival
    )
  
  newborn_components = births_by_sex_ethnicity %>%
    left_join(newborn_survival,by = c("year", "eth_code", "sex")) %>% 
    mutate(newborn_deaths = births * (1 - birth_survival),
           surviving_newborns = births * birth_survival,
           year = projection_year,
           age = 0) %>%
    select(
      year,
      eth_code,
      sex,
      age,
      births,
      newborn_deaths,
      surviving_newborns
    )
  
  newborn_population = newborn_components %>%
    transmute(
      year,
      eth_code,
      sex,
      age,
      population = surviving_newborns
    )
  
# ==========================================================
# Mortality and ageing of existing population
# ==========================================================
  
  existing_population_components = start_population %>%
    left_join(mortality %>% filter(year == current_year), by = c("year", "eth_code","sex", "age"),
              relationship = "many-to-one") %>% 
    mutate(deaths = population * (1 - survival_probability),
           surviving_population = population*survival_probability,
           year = projection_year,
           age = if_else(age >= 99, 100, age + 1)) %>% 
    group_by(year, eth_code, sex, age) %>%
    summarise(
      deaths = sum(deaths, na.rm = TRUE),
      surviving_population =
        sum(surviving_population, na.rm = TRUE),
      .groups = "drop"
    )
    
  surviving_population = existing_population_components %>%
    transmute(
      year,
      eth_code,
      sex,
      age,
      population = surviving_population
    )
  
# ==========================================================
# Population before migration
# ========================================================== 
  population_before_migration = bind_rows(
    surviving_population,
    newborn_population) %>%
    group_by(year, eth_code, sex, age) %>%
    summarise(
      population = sum(population, na.rm = TRUE),
      .groups = "drop"
    )
  
# ==========================================================
# International emigration(flows)
# ========================================================== 
  
 international_emigration_current = international_emigration%>%
    filter(year == current_year) %>% 
    mutate(
      year = projection_year
    ) %>% 
    group_by(year, eth_code, sex, age) %>%
    summarise(
      international_emigrants =
        sum(international_emigrants, na.rm = TRUE),
      .groups = "drop"
    )
  
  within_country_survivors_bham = population_before_migration %>%  
    left_join( international_emigration_current,
               by = c("year", "eth_code", "sex", "age"),
               relationship = "one-to-one")%>%
    mutate(
      international_emigrants =
        replace_na(international_emigrants, 0),
      WS_B = population - international_emigrants)
    
  
# ============================================================
#Internal out-migration (constant rates)
# ============================================================
# apply internal out-migration from Birmingham

 population_after_internal_out = within_country_survivors_bham %>%
  left_join(internal_migration %>% select(eth_code, sex, age, out_rate_as),
            by =  c("eth_code", "sex", "age")) %>% 
  mutate(out_rate_as = replace_na(out_rate_as, 0),
         internal_out_migrants = WS_B * out_rate_as,
         population_after_internal_out = WS_B - internal_out_migrants)  
  
# ==========================================================
# Internal in-migration
# ==========================================================
  internal_in_current =projected_REW %>%
    filter(year == current_year)%>%   
    left_join(internal_migration %>%select(eth_code,sex,age,in_rate_as),
              by = c("eth_code", "sex", "age")) %>% 
    mutate(
      in_rate_as = replace_na(in_rate_as, 0),
      internal_in_migrants = ruk_ethnic_population * in_rate_as,
      year = projection_year
    ) %>%
    group_by(year, eth_code, sex, age) %>%
    summarise(
      internal_in_migrants = sum(internal_in_migrants, na.rm = TRUE),
      .groups = "drop"
    )
  

# ============================================================
#International immigration
# Direct projected flows into Birmingham
# ============================================================

 international_immigration_current = international_immigration %>%
  filter(year == current_year) %>% 
  mutate(
    year = projection_year
  ) %>%
  group_by(year, eth_code, sex, age) %>%
  summarise(
    international_immigrants = sum(international_immigrants, na.rm = TRUE),
    .groups = "drop"
  )


#============================================================
#population_after_migration 
#============================================================


 population_after_migration = population_after_internal_out %>%
  left_join(
    internal_in_current,
    by = c("year", "eth_code", "sex", "age")
  ) %>%
  left_join(
    international_immigration_current,
    by = c("year", "eth_code", "sex", "age")
  )  %>%
  mutate(
    internal_in_migrants = replace_na(internal_in_migrants, 0),
    international_immigrants = replace_na(international_immigrants, 0),
    
    population_after_migration =
      population_after_internal_out +
      internal_in_migrants +
      international_immigrants
  )
#============================================================
#endpop which is also the start pop of next cycle 
#============================================================ 

  end_population = population_after_migration %>%
    transmute(
      year,
      eth_code,
      sex,
      age,
      population = population_after_migration
    ) %>%
    arrange(eth_code, sex, age)

# ==========================================================
# Store individual components
# ==========================================================
  component_output =
    population_after_migration %>%
    select(
      year,
      eth_code,
      sex,
      age,
      population_before_migration = population,
      international_emigrants,
      WS_B,
      internal_out_migrants,
      population_after_internal_out,
      internal_in_migrants,
      international_immigrants,
      end_population = population_after_migration
    ) %>%
    left_join(
      existing_population_components %>%
        select(year, eth_code, sex, age, deaths),
      by = c("year", "eth_code", "sex", "age"),
      relationship = "one-to-one"
    ) %>%
    left_join(
      newborn_components %>%
        select(
          year,
          eth_code,
          sex,
          age,
          births,
          newborn_deaths,
          surviving_newborns
        ),
      by = c("year", "eth_code", "sex", "age"),
      relationship = "one-to-one"
    ) %>%
    mutate(
      across(
        c(
          births,
          newborn_deaths,
          surviving_newborns,
          deaths,
          international_emigrants,
          internal_out_migrants,
          internal_in_migrants,
          international_immigrants
        ),
        ~ replace_na(.x, 0)
      ),
      
      total_deaths = deaths + newborn_deaths
    ) %>%
    arrange(eth_code, sex, age)


  
  #to match list 
  year_name = as.character(projection_year)

  #store the output
  #full proj
  projection_list[[year_name]] = end_population
  
  #birth
  birth_projection_list[[year_name]] =
    newborn_components
  
  #death
  death_projection_list[[year_name]] =
    component_output %>%
    select(
      year,
      eth_code,
      sex,
      age,
      deaths,
      newborn_deaths,
      total_deaths
    )
  
  #all migration
  migration_projection_list[[year_name]] =
    component_output %>%
    select(
      year,
      eth_code,
      sex,
      age,
      international_emigrants,
      internal_out_migrants,
      internal_in_migrants,
      international_immigrants
    )
  
  
  component_projection_list[[year_name]] =
    component_output
  
  
  # End population becomes the start of the next cycle
  start_population = end_population
}



