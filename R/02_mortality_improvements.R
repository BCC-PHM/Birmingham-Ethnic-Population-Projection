setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 02_mortality_improvements.R
# Apply ONS-style mortality improvements to produce
# projected survival ratios for each CCM projection step
# ============================================================
age_levels = c("0-4","5-9","10-14","15-19","20-24","25-29",
               "30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80-84","85-89","90+")
library(tidyverse)

# Load base life table (output from 01_mortality_process.R)
life_table_base = read_csv("data/processed/Birmingham_survival_ratios.csv") %>%
  mutate(age_group_5yr = factor(age_group_5yr, levels = c(
    "0-4","5-9","10-14","15-19","20-24","25-29",
    "30-34","35-39","40-44","45-49","50-54","55-59",
    "60-64","65-69","70-74","75-79","80-84","85-89","90+"
  )))

#add age midpoint, because ONS use single year but we using 5 year interval

age_midpoints = c(
  "0-4" = 2, "5-9" = 7, "10-14" = 12, "15-19" = 17, "20-24" = 22,
  "25-29" = 27, "30-34" = 32, "35-39" = 37, "40-44" = 42, "45-49" = 47,
  "50-54" = 52, "55-59" = 57, "60-64" = 62, "65-69" = 67, "70-74" = 72,
  "75-79" = 77, "80-84" = 82, "85-89" = 87, "90+" = 95  # representative midpoint
)


long_run_rate = function(age_mid) {
  case_when(
    age_mid <= 90  ~ 0.011,
    age_mid <= 110 ~ 0.011 * (110 - age_mid) / (110 - 90),  # linear taper
    TRUE           ~ 0.0
  )
}

improvement_schedule = tibble(
  age_group_5yr = names(age_midpoints),
  age_mid       = age_midpoints,
  r_longrun     = long_run_rate(age_midpoints)
)


###############################################################
projection_year = seq(2021, 2061, by=5)


projection_improvements = crossing(
  year = projection_year,
  improvement_schedule
) %>% 
  mutate(
    years_elapsed = year - 2021,
    year_of_convergence = 25,
    r = ifelse(years_elapsed>=25,
               r_longrun,
               0+(r_longrun)*years_elapsed/year_of_convergence),
    
    r_avg = case_when(
      years_elapsed == 0 ~ 0,
      years_elapsed <= year_of_convergence ~ r / 2,
      TRUE ~ ((r_longrun / 2) * year_of_convergence +
                r_longrun * (years_elapsed - year_of_convergence)) / years_elapsed
    ),
    age_group_5yr = factor(age_group_5yr,
                           levels = age_levels)
    ) %>% 
  select(year, age_group_5yr, years_elapsed, r, r_avg) %>% 
  arrange(year, age_group_5yr)







life_table_projection = life_table_base %>% 
  select(eth_code, DEC_SEX,age_group_5yr,mx,n) %>% 
  right_join(projection_improvements, by = c("age_group_5yr"),relationship = "many-to-many") %>% 
  mutate(mx_projected = mx * (1 - r_avg)^years_elapsed)



life_table_projection  = life_table_projection  %>% 
  mutate(n =5,
         #ax is the average proportion of the age interval lived by those who die within that interval
         ax = case_when(age_group_5yr == "0-4" ~ 0.07,
                        age_group_5yr == "90+" ~ 1/mx_projected,
                        TRUE ~ 2.5),
         #probability of dying in interval
         qx = case_when(
           age_group_5yr == "90+" ~ 1,   # everyone alive at 90+ will eventually die within the open-ended 90+ interval.
           TRUE                   ~ (n * mx_projected) / (1 + (n - ax) * mx_projected)
         ),
         
         # just for safety because probability cant exceed 1 
         qx = pmin(qx, 1)
  ) %>% 
  arrange(eth_code, DEC_SEX, year, age_group_5yr) %>% 
  group_by(eth_code, DEC_SEX,year) %>% 
  mutate(
    # lx: survivors at start of interval, radix = 100,000
    lx  = 100000 * cumprod(lag(1 - qx, default = 1)),
    # dx: deaths in interval
    dx  = lx * qx,
    # Lx: person-years lived in interval
    Lx  = case_when(
      age_group_5yr == "90+" ~ lx / mx_projected,   # ONS open interval
      TRUE                   ~ n * (lx - dx) + ax * dx
    )
  ) %>% 
  mutate(
    # Tx: person-years lived above age x
    Tx  = rev(cumsum(rev(Lx))),
    # ex: life expectancy at age x
    ex  = Tx / lx
  ) %>% 
  ungroup()




survival_ratios_projected  = life_table_projection %>%
  group_by(eth_code, DEC_SEX,year) %>%
  mutate(
    Lx_next = lead(Lx),
    Tx_90 = Tx[age_group_5yr == "90+"],
    Tx_85 = Tx[age_group_5yr == "85-89"],
    
    Sx = case_when(
      age_group_5yr == "85-89" ~ Tx_90 / Tx_85,
      age_group_5yr == "90+"   ~ Tx_90 / Tx_85,
      TRUE ~ Lx_next / Lx
    ),
    
    Sx = pmin(Sx, 1)
  ) %>%
  ungroup()


# 1. check ex is rising over time for a given group — it should increase as mortality improves
survival_ratios_projected %>%
  filter(eth_code == "BAN", DEC_SEX == "Female", age_group_5yr == "0-4") %>%
  select(year, mx_projected, ex, Sx)

# 2. check Sx is also rising over time — survival should improve
survival_ratios_projected %>%
  filter(eth_code == "BAN", DEC_SEX == "Female", age_group_5yr == "60-64") %>%
  select(year, Sx)

# 3. check no Sx > 1
survival_ratios_projected %>%
  filter(Sx > 1)

# 4. check you have the right number of rows 
# 12 eth × 2 sex × 19 age groups × 9 years steps = 4,104
nrow(survival_ratios_projected)


write.csv(survival_ratios_projected, "data/processed/Birmingham_survival_ratios_projected.csv")




