setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")

#======================================================
# 11_post_CCM_result.R
#visualise the results
#======================================================
library(tidyverse)
library(readr)
library(readxl)
library(bcctheme)
library(patchwork)

#------------------------------------------------------
#handy function adjusting ggsave
#------------------------------------------------------
pixel_2_in = function(width, height){
  width_in = width/96
  height_in = height/96
  
  result = c(width_in, height_in)
  return(result)
}

#======================================================
#load results and build annual component summaries
#======================================================
CCM_result_list = readRDS("data/processed/CCM_result_list.rds")

full_component_projection = CCM_result_list[[1]]$full_component_projection

for (i in seq_along(CCM_result_list)){
  
  full_component_projection = CCM_result_list[[i]]$full_component_projection
  
  #---annual components, all groups----------------------
  CCM_result_list[[i]]$annual_components = full_component_projection %>% 
    group_by(year) %>% 
    summarise(
      births            = sum(births, na.rm = TRUE),
      deaths            = sum(total_deaths, na.rm = TRUE),
      natural_change    = births - deaths,
      internal_in       = sum(internal_in_migrants, na.rm = TRUE),
      internal_out      = sum(internal_out_migrants, na.rm = TRUE),
      net_internal      = internal_in - internal_out,
      international_in  = sum(international_immigrants, na.rm = TRUE),
      international_out = sum(international_emigrants, na.rm = TRUE),
      net_international = international_in - international_out,
      projected_change  = natural_change + net_internal + net_international,
      end_population    = sum(end_population, na.rm = TRUE),
      start_pop         = end_population - projected_change,
      .groups           = "drop"
    )
  
  #---annual components by ethnic group------------------
  CCM_result_list[[i]]$annual_components_byeth = full_component_projection %>% 
    group_by(year, eth_code) %>% 
    summarise(
      births            = sum(births, na.rm = TRUE),
      deaths            = sum(total_deaths, na.rm = TRUE),
      natural_change    = births - deaths,
      internal_in       = sum(internal_in_migrants, na.rm = TRUE),
      internal_out      = sum(internal_out_migrants, na.rm = TRUE),
      net_internal      = internal_in - internal_out,
      international_in  = sum(international_immigrants, na.rm = TRUE),
      international_out = sum(international_emigrants, na.rm = TRUE),
      net_international = international_in - international_out,
      projected_change  = natural_change + net_internal + net_international,
      end_population    = sum(end_population, na.rm = TRUE),
      start_pop         = end_population - projected_change,
      .groups           = "drop"
    )
  
}

#======================================================
#plot 10.1 - scenarios
#======================================================
plot_results_10.1 = bind_rows(
  "B-EF"      = CCM_result_list[[1]]$annual_components,
  "B-ER2022"  = CCM_result_list[[2]]$annual_components,
  "LT-ER2030" = CCM_result_list[[3]]$annual_components,
  .id = "scenario"
) %>% 
  mutate(scenario = factor(scenario, levels = c("B-EF", "B-ER2022", "LT-ER2030")),
         year     = as.character(year)) %>% 
  ggplot(aes(x = year, y = end_population, group = scenario, color = scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(1150000, 1300000)) +
  theme_bcc(base_size = 11) +
  scale_colour_bcc(palette = "multi") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Birmingham Population Projections under Alternative Emigration Scenarios") +
  labs(y = "Population", color = "Scenarios") +
  theme(legend.title = element_text(color = bcc_cols("black")))

#======================================================
#plot 10.2 - count pyramid, white/non-white
#======================================================
plot_results_10.2 = CCM_result_list[[3]]$full_component_projection %>% 
  group_by(year, age, sex, eth_code) %>% 
  summarise(
    births            = sum(births, na.rm = TRUE),
    deaths            = sum(total_deaths, na.rm = TRUE),
    natural_change    = births - deaths,
    internal_in       = sum(internal_in_migrants, na.rm = TRUE),
    internal_out      = sum(internal_out_migrants, na.rm = TRUE),
    net_internal      = internal_in - internal_out,
    international_in  = sum(international_immigrants, na.rm = TRUE),
    international_out = sum(international_emigrants, na.rm = TRUE),
    net_international = international_in - international_out,
    projected_change  = natural_change + net_internal + net_international,
    end_population    = sum(end_population, na.rm = TRUE),
    start_pop         = end_population - projected_change,
    .groups           = "drop"
  ) %>% 
  mutate(nonwhite = case_when(eth_code == "WBI" ~ "White British",
                              eth_code == "WHO" ~ "White Other",
                              TRUE              ~ "Non-White")) %>% 
  group_by(year, age, sex, nonwhite) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  mutate(age = ifelse(age >= 90, "90+", as.character(age)),
         age = factor(age, levels = c(as.character(0:89), "90+")),
         sex = factor(sex, levels = c("Male", "Female"))) %>% 
  select(year, age, sex, nonwhite, end_population) %>% 
  mutate(end_population = ifelse(sex == "Male", end_population*-1, end_population)) %>% 
  filter(year %in% c(2022, 2032, 2047)) %>% 
  ggplot(aes(y = age, x = end_population, fill = nonwhite)) +
  geom_col(width = 0.7) +
  annotate("text", x = -Inf, y = Inf, label = "Male",   hjust = -0.3, vjust = 1.2, size = 3.5) +
  annotate("text", x =  Inf, y = Inf, label = "Female", hjust =  1.3, vjust = 1.2, size = 3.5) +
  facet_wrap(~year) +
  scale_x_continuous(labels = function(x) scales::comma(abs(x))) +
  scale_y_discrete(breaks = c(as.character(seq(0, 80, 10)), "90+")) +
  scale_fill_bcc(palette = "multi") +
  ggtitle("Projected population by age and sex and ethnicity grouping for Birmingham") +
  labs(subtitle = "Projections for 2022, 2032 and 2047", x = "population") +
  theme_bcc(base_size = 12, gridline_x = F) +
  theme(legend.position  = "bottom",
        plot.margin      = margin(5, 20, 5, 20),
        panel.spacing.x  = unit(1.2, "lines"),
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold", hjust = 0.5))

#------------------------------------------------------
#all-Birmingham reference profile
#------------------------------------------------------
overall_profile = CCM_result_list[[3]]$full_component_projection %>% 
  group_by(year, sex, age) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  mutate(nonwhite = "All") %>% 
  group_by(year, age, sex, nonwhite) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  group_by(year, nonwhite) %>% 
  mutate(pop_share = end_population / sum(end_population, na.rm = TRUE) * 100) %>% 
  ungroup() %>% 
  mutate(
    age       = ifelse(age >= 90, "90+", as.character(age)),
    age       = factor(age, levels = c(as.character(0:89), "90+")),
    sex       = factor(sex, levels = c("Male", "Female")),
    pop_share = ifelse(sex == "Male", pop_share * -1, pop_share)
  ) %>% 
  filter(year %in% c(2022, 2032, 2047))

#------------------------------------------------------
#plot 10.2 - pct pyramid, white/non-white
#------------------------------------------------------
plot_results_10.2_pct = CCM_result_list[[3]]$full_component_projection %>% 
  group_by(year, age, sex, eth_code) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  mutate(nonwhite = case_when(eth_code %in% c("WBI", "WHO") ~ "White",
                              TRUE                          ~ "Non-White")) %>% 
  group_by(year, age, sex, nonwhite) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  group_by(year, nonwhite) %>% 
  mutate(pop_share = end_population / sum(end_population, na.rm = TRUE) * 100) %>% 
  ungroup() %>% 
  mutate(
    age       = ifelse(age >= 90, "90+", as.character(age)),
    age       = factor(age, levels = c(as.character(0:89), "90+")),
    sex       = factor(sex, levels = c("Male", "Female")),
    pop_share = ifelse(sex == "Male", pop_share * -1, pop_share)
  ) %>% 
  filter(year %in% c(2022, 2032, 2047)) %>% 
  ggplot(aes(y = age, x = pop_share)) +
  geom_col(data = overall_profile, aes(fill = nonwhite), width = 0.7) +
  geom_step(aes(colour = nonwhite, group = interaction(nonwhite, sex)),
            orientation = "y", linewidth = 0.5) +
  annotate("text", x = -Inf, y = Inf, label = "Male",   hjust = -0.3, vjust = 1.2, size = 3.5) +
  annotate("text", x =  Inf, y = Inf, label = "Female", hjust =  1.3, vjust = 1.2, size = 3.5) +
  facet_wrap(~year) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_discrete(breaks = c(as.character(seq(0, 80, 10)), "90+")) +
  scale_colour_bcc(palette = "multi") +
  scale_fill_manual(values = c("All" = "grey80"), labels = "All", name = NULL) +
  labs(x = "Share of ethnic group population (%)") +
  theme_bcc(base_size = 15, gridline_x = F) +
  theme(
    legend.position  = "bottom",
    plot.margin      = margin(5, 20, 5, 20),
    panel.spacing.x  = unit(1.2, "lines"),
    axis.text.x      = element_text(size = 11),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", hjust = 0.5)
  )

plot_results_10.2_pct

plot_results_10.2/plot_results_10.2_pct

#======================================================
#line plot - white british / white other / non-white
#======================================================
options(scipen = 999)

CCM_result_list[[3]]$full_component_projection %>% 
  group_by(year, eth_code) %>% 
  summarise(
    births            = sum(births, na.rm = TRUE),
    deaths            = sum(total_deaths, na.rm = TRUE),
    natural_change    = births - deaths,
    internal_in       = sum(internal_in_migrants, na.rm = TRUE),
    internal_out      = sum(internal_out_migrants, na.rm = TRUE),
    net_internal      = internal_in - internal_out,
    international_in  = sum(international_immigrants, na.rm = TRUE),
    international_out = sum(international_emigrants, na.rm = TRUE),
    net_international = international_in - international_out,
    projected_change  = natural_change + net_internal + net_international,
    end_population    = sum(end_population, na.rm = TRUE),
    start_pop         = end_population - projected_change,
    .groups           = "drop"
  ) %>% 
  mutate(nonwhite = case_when(eth_code == "WBI" ~ "White British",
                              eth_code == "WHO" ~ "White Other",
                              TRUE              ~ "Non-White")) %>% 
  group_by(year, nonwhite) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  mutate(year = as.character(year)) %>% 
  ggplot(aes(x = year, y = end_population, colour = nonwhite, group = nonwhite)) +
  geom_line(size = 1) +
  theme_bcc(base_size = 11) +
  scale_colour_bcc(palette = "multi") +
  scale_y_continuous(limits = c(10000, 900000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Birmingham Population Projections under Alternative Emigration Scenarios") +
  labs(y = "Population", color = "Scenarios") +
  theme(legend.title = element_text(color = bcc_cols("black")))

#======================================================
#10.2.2 broad ethnic group
#======================================================

#---plot 10.2.2 count pyramid--------------------------
CCM_result_list[[3]]$full_component_projection %>% 
  group_by(year, eth_code) %>% 
  summarise(
    births            = sum(births, na.rm = TRUE),
    deaths            = sum(total_deaths, na.rm = TRUE),
    natural_change    = births - deaths,
    internal_in       = sum(internal_in_migrants, na.rm = TRUE),
    internal_out      = sum(internal_out_migrants, na.rm = TRUE),
    net_internal      = internal_in - internal_out,
    international_in  = sum(international_immigrants, na.rm = TRUE),
    international_out = sum(international_emigrants, na.rm = TRUE),
    net_international = international_in - international_out,
    projected_change  = natural_change + net_internal + net_international,
    end_population    = sum(end_population, na.rm = TRUE),
    start_pop         = end_population - projected_change,
    .groups           = "drop"
  ) %>% 
  mutate(broad_eth = case_when(eth_code %in% c("WBI", "WHO")                      ~ "White",
                               eth_code == "MIX"                                  ~ "Mixed",
                               eth_code %in% c("IND", "PAK", "BAN", "CHI", "OAS") ~ "Asian",
                               eth_code %in% c("BLA", "BLC", "OBL")               ~ "Black",
                               eth_code == "OTH"                                  ~ "Other"),
         broad_eth = factor(broad_eth, levels = c("White", "Mixed", "Asian", "Black", "Other"))) %>% 
  group_by(year, broad_eth) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  mutate(year = as.character(year)) %>% 
  ggplot(aes(x = year, y = end_population, colour = broad_eth, group = broad_eth)) +
  geom_line(size = 1) +
  theme_bcc(base_size = 11) +
  scale_colour_manual(values = c("White" = "#00A9E0",
                                 "Mixed" = "#75BC22",
                                 "Asian" = "#DC582A",
                                 "Black" = "#84329B",
                                 "Other" = "#D00070")) +
  scale_y_continuous(limits = c(10000, 600000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Projected broad ethnic composition of Birmingham") +
  labs(y = "Population", color = "Ethnic group") +
  theme(legend.title = element_text(color = bcc_cols("black")))

#---broad ethnic shares, selected years----------------
CCM_result_list[[3]]$full_component_projection %>% 
  mutate(broad_eth = case_when(eth_code %in% c("WBI", "WHO")                      ~ "White",
                               eth_code == "MIX"                                  ~ "Mixed",
                               eth_code %in% c("IND", "PAK", "BAN", "CHI", "OAS") ~ "Asian",
                               eth_code %in% c("BLA", "BLC", "OBL")               ~ "Black",
                               eth_code == "OTH"                                  ~ "Other"),
         broad_eth = factor(broad_eth, levels = c("White", "Mixed", "Asian", "Black", "Other"))) %>% 
  group_by(year, broad_eth) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  group_by(year) %>% 
  mutate(all_population = sum(end_population, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(pop_share = end_population / all_population * 100) %>% 
  filter(year %in% c(2022, 2032, 2047))

#======================================================
#plot 10.5 - broad ethnic pyramids
#======================================================

#---plot 10.5 count pyramid----------------------------
plot_results_10.5 = CCM_result_list[[3]]$full_component_projection %>% 
  group_by(year, age, sex, eth_code) %>% 
  summarise(
    births            = sum(births, na.rm = TRUE),
    deaths            = sum(total_deaths, na.rm = TRUE),
    natural_change    = births - deaths,
    internal_in       = sum(internal_in_migrants, na.rm = TRUE),
    internal_out      = sum(internal_out_migrants, na.rm = TRUE),
    net_internal      = internal_in - internal_out,
    international_in  = sum(international_immigrants, na.rm = TRUE),
    international_out = sum(international_emigrants, na.rm = TRUE),
    net_international = international_in - international_out,
    projected_change  = natural_change + net_internal + net_international,
    end_population    = sum(end_population, na.rm = TRUE),
    start_pop         = end_population - projected_change,
    .groups           = "drop"
  ) %>% 
  mutate(broad_eth = case_when(eth_code %in% c("WBI", "WHO")                      ~ "White",
                               eth_code == "MIX"                                  ~ "Mixed",
                               eth_code %in% c("IND", "PAK", "BAN", "CHI", "OAS") ~ "Asian",
                               eth_code %in% c("BLA", "BLC", "OBL")               ~ "Black",
                               eth_code == "OTH"                                  ~ "Other"),
         broad_eth = factor(broad_eth, levels = c("White", "Mixed", "Asian", "Black", "Other"))) %>% 
  group_by(year, age, sex, broad_eth) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  mutate(age = ifelse(age >= 90, "90+", as.character(age)),
         age = factor(age, levels = c(as.character(0:89), "90+")),
         sex = factor(sex, levels = c("Male", "Female"))) %>% 
  select(year, age, sex, broad_eth, end_population) %>% 
  mutate(end_population = ifelse(sex == "Male", end_population*-1, end_population)) %>% 
  filter(year %in% c(2022, 2032, 2047)) %>% 
  ggplot(aes(y = age, x = end_population, fill = broad_eth)) +
  geom_col(width = 0.7) +
  annotate("text", x = -Inf, y = Inf, label = "Male",   hjust = -0.3, vjust = 1.2, size = 3.5) +
  annotate("text", x =  Inf, y = Inf, label = "Female", hjust =  1.3, vjust = 1.2, size = 3.5) +
  facet_wrap(~year) +
  scale_x_continuous(labels = function(x) scales::comma(abs(x))) +
  scale_y_discrete(breaks = c(as.character(seq(0, 80, 10)), "90+")) +
  scale_fill_manual(values = c("White" = "#00A9E0",
                               "Mixed" = "#75BC22",
                               "Asian" = "#DC582A",
                               "Black" = "#84329B",
                               "Other" = "#D00070")) +
  ggtitle("Projected population by age and sex and broad ethnic group for Birmingham") +
  labs(subtitle = "Projections for 2022, 2032 and 2047", x = "population") +
  theme_bcc(base_size = 12, gridline_x = F) +
  theme(legend.position  = "bottom",
        plot.margin      = margin(5, 20, 5, 20),
        panel.spacing.x  = unit(1.2, "lines"),
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold", hjust = 0.5))

#---plot 10.5 pct pyramid------------------------------
plot_results_10.5_pct = CCM_result_list[[3]]$full_component_projection %>% 
  mutate(broad_eth = case_when(eth_code %in% c("WBI", "WHO")                      ~ "White",
                               eth_code == "MIX"                                  ~ "Mixed",
                               eth_code %in% c("IND", "PAK", "BAN", "CHI", "OAS") ~ "Asian",
                               eth_code %in% c("BLA", "BLC", "OBL")               ~ "Black",
                               eth_code == "OTH"                                  ~ "Other"),
         broad_eth = factor(broad_eth, levels = c("White", "Mixed", "Asian", "Black", "Other"))) %>% 
  group_by(year, age, sex, broad_eth) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  group_by(year, age, sex, broad_eth) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  group_by(year, broad_eth) %>% 
  mutate(pop_share = end_population / sum(end_population, na.rm = TRUE) * 100) %>% 
  ungroup() %>% 
  mutate(
    age       = ifelse(age >= 90, "90+", as.character(age)),
    age       = factor(age, levels = c(as.character(0:89), "90+")),
    sex       = factor(sex, levels = c("Male", "Female")),
    pop_share = ifelse(sex == "Male", pop_share * -1, pop_share)
  ) %>% 
  filter(year %in% c(2022, 2032, 2047)) %>% 
  ggplot(aes(y = age, x = pop_share)) +
  geom_col(data = overall_profile, aes(fill = nonwhite), width = 0.7) +
  geom_step(aes(colour = broad_eth, group = interaction(broad_eth, sex)),
            orientation = "y", linewidth = 0.5) +
  annotate("text", x = -Inf, y = Inf, label = "Male",   hjust = -0.3, vjust = 1.2, size = 3.5) +
  annotate("text", x =  Inf, y = Inf, label = "Female", hjust =  1.3, vjust = 1.2, size = 3.5) +
  facet_wrap(~year) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_discrete(breaks = c(as.character(seq(0, 80, 10)), "90+")) +
  scale_colour_manual(values = c("White" = "#00A9E0",
                                 "Mixed" = "#75BC22",
                                 "Asian" = "#DC582A",
                                 "Black" = "#84329B",
                                 "Other" = "#D00070")) +
  scale_fill_manual(values = c("All" = "grey80"), labels = "All", name = NULL) +
  labs(x = "Share of ethnic group population (%)") +
  theme_bcc(base_size = 15, gridline_x = F) +
  theme(
    legend.position  = "bottom",
    plot.margin      = margin(5, 20, 5, 20),
    panel.spacing.x  = unit(1.2, "lines"),
    axis.text.x      = element_text(size = 11),
    strip.background = element_blank(),
    strip.text       = element_text(face = "bold", hjust = 0.5)
  )

#======================================================
#detailed ethnic group projections
#======================================================
broad_group_map = broad_group_map %>% 
  group_by(broad_colours) %>% 
  mutate(shade_number      = row_number(),
         harmonised_colour = bcc_pal(palette = first(broad_colours))(9)[round(seq(2, 9, length.out = n()))][shade_number]) %>% 
  ungroup()

harmonised_colour_values = broad_group_map %>% 
  distinct(eth_code, harmonised_colour) %>% 
  mutate(eth_code = as.character(eth_code)) %>% 
  tibble::deframe()

eth_code_order = c(
  "WBI", "WHO",
  "MIX",
  "IND", "PAK", "BAN", "CHI", "OAS",
  "BLA", "BLC", "OBL",
  "OTH"
)

#---harmonised group line plot-------------------------
plot_result_harmonised_line = function(eth){
  
  annual_components_byeth_plot = CCM_result_list[[3]]$annual_components_byeth %>% 
    mutate(eth_code = factor(eth_code, levels = eth_code_order)) %>% 
    filter(eth_code %in% eth) %>% 
    ggplot(aes(x = year, y = end_population, colour = eth_code, group = eth_code)) +
    geom_line(size = 0.8) +
    scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
    scale_colour_manual(values = harmonised_colour_values, breaks = eth_code_order) +
    labs(
      title  = "Projected population by harmonised ethnic group",
      x      = NULL,
      y      = "Population",
      colour = "Broad ethnic group"
    ) +
    theme_bcc(base_size = 11) +
    theme(title = element_text(size = 9))
  
  return(annual_components_byeth_plot)
  
}

plot_result_harmonised_line(eth = c("IND", "PAK", "BAN", "CHI", "OAS"))

#------------------------------------------------------
#harmonised group pyramids (count + pct)
#------------------------------------------------------
plot_result_harmonised_pyramid = function(broad = "White",
                                          eth = c("WBI", "WHO")){
  
  #---count pyramid------------------------------------
  harmonised_count_pyramid = CCM_result_list[[3]]$full_component_projection %>% 
    group_by(year, age, sex, eth_code) %>% 
    summarise(
      births            = sum(births, na.rm = TRUE),
      deaths            = sum(total_deaths, na.rm = TRUE),
      natural_change    = births - deaths,
      internal_in       = sum(internal_in_migrants, na.rm = TRUE),
      internal_out      = sum(internal_out_migrants, na.rm = TRUE),
      net_internal      = internal_in - internal_out,
      international_in  = sum(international_immigrants, na.rm = TRUE),
      international_out = sum(international_emigrants, na.rm = TRUE),
      net_international = international_in - international_out,
      projected_change  = natural_change + net_internal + net_international,
      end_population    = sum(end_population, na.rm = TRUE),
      start_pop         = end_population - projected_change,
      .groups           = "drop"
    ) %>% 
    group_by(year, age, sex, eth_code) %>% 
    summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
    mutate(age = ifelse(age >= 90, "90+", as.character(age)),
           age = factor(age, levels = c(as.character(0:89), "90+")),
           sex = factor(sex, levels = c("Male", "Female"))) %>% 
    select(year, age, sex, eth_code, end_population) %>% 
    mutate(end_population = ifelse(sex == "Male", end_population*-1, end_population)) %>% 
    filter(year %in% c(2022, 2032, 2047)) %>% 
    filter(eth_code %in% eth) %>% 
    ggplot(aes(y = age, x = end_population, fill = eth_code)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = "white") +
    annotate("text", x = -Inf, y = Inf, label = "Male",   hjust = -0.3, vjust = 1.2, size = 3.5) +
    annotate("text", x =  Inf, y = Inf, label = "Female", hjust =  1.3, vjust = 1.2, size = 3.5) +
    facet_wrap(~year) +
    scale_x_continuous(labels = function(x) scales::comma(abs(x))) +
    scale_y_discrete(breaks = c(as.character(seq(0, 80, 10)), "90+")) +
    scale_fill_manual(values = harmonised_colour_values, breaks = eth_code_order) +
    ggtitle("Projected population by age and sex and harmonised ethnic group for Birmingham") +
    labs(subtitle = "Projections for 2022, 2032 and 2047", x = "population") +
    theme_bcc(base_size = 12, gridline_x = F) +
    theme(legend.position  = "bottom",
          plot.margin      = margin(5, 20, 5, 20),
          panel.spacing.x  = unit(1.2, "lines"),
          strip.background = element_blank(),
          strip.text       = element_text(face = "bold", hjust = 0.5))
  
  #---broad group reference profile--------------------
  overall_profile_broadeth = CCM_result_list[[3]]$full_component_projection %>% 
    mutate(broad_eth = case_when(eth_code %in% c("WBI", "WHO")                      ~ "White",
                                 eth_code == "MIX"                                  ~ "Mixed",
                                 eth_code %in% c("IND", "PAK", "BAN", "CHI", "OAS") ~ "Asian",
                                 eth_code %in% c("BLA", "BLC", "OBL")               ~ "Black",
                                 eth_code == "OTH"                                  ~ "Other"),
           broad_eth = factor(broad_eth, levels = c("White", "Mixed", "Asian", "Black", "Other"))) %>% 
    group_by(year, sex, age, broad_eth) %>% 
    summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
    group_by(year, age, sex, broad_eth) %>% 
    summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
    group_by(year, broad_eth) %>% 
    mutate(pop_share = end_population / sum(end_population, na.rm = TRUE) * 100) %>% 
    ungroup() %>% 
    mutate(
      age       = ifelse(age >= 90, "90+", as.character(age)),
      age       = factor(age, levels = c(as.character(0:89), "90+")),
      sex       = factor(sex, levels = c("Male", "Female")),
      pop_share = ifelse(sex == "Male", pop_share * -1, pop_share)
    ) %>% 
    filter(year %in% c(2022, 2032, 2047)) %>% 
    filter(broad_eth %in% broad)
  
  #---pct pyramid--------------------------------------
  harmonised_pct_pyramid = CCM_result_list[[3]]$full_component_projection %>% 
    group_by(year, age, sex, eth_code) %>% 
    summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
    group_by(year, age, sex, eth_code) %>% 
    summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
    group_by(year, eth_code) %>% 
    mutate(pop_share = end_population / sum(end_population, na.rm = TRUE) * 100) %>% 
    ungroup() %>% 
    mutate(
      age       = ifelse(age >= 90, "90+", as.character(age)),
      age       = factor(age, levels = c(as.character(0:89), "90+")),
      sex       = factor(sex, levels = c("Male", "Female")),
      pop_share = ifelse(sex == "Male", pop_share * -1, pop_share)
    ) %>% 
    filter(year %in% c(2022, 2032, 2047)) %>% 
    filter(eth_code %in% eth) %>% 
    ggplot(aes(y = age, x = pop_share)) +
    geom_col(data = overall_profile_broadeth %>% filter(broad_eth == broad), aes(fill = "All"), width = 0.7) +
    geom_step(aes(colour = eth_code, group = interaction(eth_code, sex)),
              orientation = "y", linewidth = 0.8) +
    annotate("text", x = -Inf, y = Inf, label = "Male",   hjust = -0.3, vjust = 1.2, size = 3.5) +
    annotate("text", x =  Inf, y = Inf, label = "Female", hjust =  1.3, vjust = 1.2, size = 3.5) +
    facet_wrap(~year) +
    scale_x_continuous(labels = function(x) abs(x)) +
    scale_y_discrete(breaks = c(as.character(seq(0, 80, 10)), "90+")) +
    scale_colour_manual(values = harmonised_colour_values, breaks = eth_code_order) +
    scale_fill_manual(values = c("All" = "grey80"), labels = broad, name = NULL) +
    labs(x = "Share of ethnic group population (%)") +
    theme_bcc(base_size = 15, gridline_x = F) +
    theme(
      legend.position  = "bottom",
      plot.margin      = margin(5, 20, 5, 20),
      panel.spacing.x  = unit(1.2, "lines"),
      axis.text.x      = element_text(size = 11),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", hjust = 0.5)
    )
  
  combined_harmonised_pyramid = (harmonised_count_pyramid/harmonised_pct_pyramid)
  
  return(combined_harmonised_pyramid)
  
}

plot_result_harmonised_pyramid(broad = "Asian",
                               eth = c("IND", "PAK", "BAN", "CHI", "OAS"))

#======================================================
#pyramid profile plots referencing 2022 outline
#same as the interactive document
#======================================================
pyramid_profile = CCM_result_list[[3]]$full_population_projection %>% 
  mutate(age = ifelse(age >= 90, "90+", as.character(age)),
         age = factor(age, levels = c(as.character(0:89), "90+")),
         sex = factor(sex, levels = c("Male", "Female"))) %>% 
  group_by(year, eth_code, sex, age) %>% 
  summarise(population = sum(population), .groups = "drop") %>% 
  mutate(population = ifelse(sex == "Male", population*-1, population))

pyramid_profile_harmonised = function(select_ethnic = "WBI",
                                      slider_year = c(2022, 2032, 2047)){
  
  #---2022 reference outline, drawn in every panel-----
  ref_profile = pyramid_profile %>% 
    filter(eth_code == select_ethnic, year == 2022) %>% 
    select(-year)
  
  #---plot title specific to harmonised ethnic group---
  plot_title = paste0("Projected change in ", select_ethnic, " population by single year of age and sex\n",
                      "Birmingham, ", "2022, 2032 and 2047")
  
  pyramid_profile_plot = pyramid_profile %>% 
    filter(eth_code == select_ethnic,
           year     %in% slider_year) %>% 
    ggplot(aes(y = age, x = population, fill = sex)) +
    geom_col() +
    geom_step(data = ref_profile, aes(y = age, x = population, group = sex),
              colour = "black", linewidth = 0.6, direction = "mid", orientation = "y") +
    facet_wrap(~ year) +
    scale_x_continuous(labels = function(x) scales::comma(abs(x))) +
    scale_y_discrete(breaks = c(as.character(seq(0, 80, 10)), "90+")) +
    scale_fill_bcc(palette = "multi") +
    labs(title = plot_title) +
    theme_bcc(base_size = 12, gridline_y = F) +
    theme(
      legend.position  = "bottom",
      plot.margin      = margin(5, 20, 5, 20),
      panel.spacing.x  = unit(1.2, "lines"),
      axis.text.x      = element_text(size = 11),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", hjust = 0.5)
    )
  
  return(pyramid_profile_plot)
  
}

pyramid_profile_harmonised()


#======================================================
#to company pyramid profile
#showing how broader age groups changes with lines
#======================================================

age_cols =  c("0-4"   = "#C7E9B4", "5-15"  = "#7FCDBB", "16-24" = "#41B6C4",
              "25-49" = "#1D91C0", "50-64" = "#225EA8", "65+"   = "#0C2C84")


changes_in_agegroup = CCM_result_list[[3]]$full_population_projection %>% 
  mutate(age_groups = case_when(age %in% c(0:4) ~ "0-4",
                                age %in% c(5:15)~ "5-15",
                                age %in% c(16:24) ~ "16-24",
                                age %in% c(25:49) ~ "25-49",
                                age %in% c(50:64) ~ "50-64",
                                age %in% c(65:100) ~ "65+",
                                TRUE ~ NA_character_),
         age_groups = factor(age_groups, levels = rev(c("0-4", "5-15", "16-24", "25-49", "50-64", "65+")))) %>% 
  group_by(year, eth_code,age_groups) %>% 
  summarise(population = sum(population), .groups = "drop") %>% 
  filter(year %in% c(2022:2047))
  

age_changes_count = changes_in_agegroup %>% 
  filter(eth_code == "PAK") %>% 
  ggplot(aes(x = year, y = population, colour = age_groups, group = age_groups)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = age_cols, name = "Age group") +
  labs(title = "Projected Pakistani population by age group",
       subtitle = "Birmingham, 2022 to 2047",
       x     = NULL,
       y     = "Population") +
  theme_bcc(base_size = 15)+
  theme(legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 1))
  

age_changes_share = changes_in_agegroup %>% 
  filter(eth_code == "PAK") %>% 
  ggplot(aes(x = year, y = population, fill = age_groups, group = age_groups)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
  scale_fill_manual(values = age_cols)+
  labs(
    x      = NULL,
    y      = "Population (%)",
    colour = "Broad ethnic group"
  ) +
  theme_bcc(base_size = 15)+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1))
  



age_changes_count/age_changes_share



#======================================================
#waterfall plot
#2022 to 2032 , 2032 to 2047
#======================================================



waterfallplot_data = CCM_result_list[[3]]$annual_components_byeth %>% 
  filter(year %in% 2023:2032) %>%
  group_by(eth_code) %>%
  summarise(Births       = sum(births),
            Deaths       = sum(deaths) * -1,
            Internal_in  = sum(internal_in),
            Internal_out = sum(internal_out) * -1,
            Immigration  = sum(international_in),
            Emigration   = sum(international_out) * -1,
            .groups = "drop") %>%
  pivot_longer(cols = -eth_code,
               values_to = "changes",
               names_to  = "details") %>%
  mutate(details = factor(details,
                          levels = c("Births", "Deaths",
                                     "Internal_in", "Internal_out",
                                     "Immigration", "Emigration"))) %>%
  arrange(eth_code, details) %>%
  group_by(eth_code) %>%                      # <- grouping FIRST
  mutate(
    end   = cumsum(changes),
    start = lag(end,default = 0),
    ypos  = rev(seq_len(n())), 
    next_ypos = lead(ypos,default = 0),# 6..1 within each group
    pad   = max(abs(changes)) * 0.03,
    label_x     = end + ifelse(changes > 0, pad, -pad),
    label_y     = ypos,
    label_hjust = ifelse(changes > 0, 0, 1),
    change_label = paste0(ifelse(changes > 0, "+", ""),
                          scales::comma(round(changes)))
  ) %>%
  ungroup() 






waterfallplot_3247data = CCM_result_list[[3]]$annual_components_byeth %>% 
  filter(year %in% 2033:2047) %>%
  group_by(eth_code) %>%
  summarise(Births       = sum(births),
            Deaths       = sum(deaths) * -1,
            Internal_in  = sum(internal_in),
            Internal_out = sum(internal_out) * -1,
            Immigration  = sum(international_in),
            Emigration   = sum(international_out) * -1,
            .groups = "drop") %>%
  pivot_longer(cols = -eth_code,
               values_to = "changes",
               names_to  = "details") %>%
  mutate(details = factor(details,
                          levels = c("Births", "Deaths",
                                     "Internal_in", "Internal_out",
                                     "Immigration", "Emigration"))) %>%
  arrange(eth_code, details) %>%
  group_by(eth_code) %>%                      # <- grouping FIRST
  mutate(
    end   = cumsum(changes),
    start = lag(end,default = 0),
    ypos  = rev(seq_len(n())), 
    next_ypos = lead(ypos,default = 0),# 6..1 within each group
    pad   = max(abs(changes)) * 0.03,
    label_x     = end + ifelse(changes > 0, pad, -pad),
    label_y     = ypos,
    label_hjust = ifelse(changes > 0, 0, 1),
    change_label = paste0(ifelse(changes > 0, "+", ""),
                          scales::comma(round(changes)))
  ) %>%
  ungroup() 







waterfall_func = function(eth = "WBI"){

#----------------------------------------------
#for 2022-2032
#filter it 
waterfallplot_data_ready =waterfallplot_data %>% 
  filter(eth_code == eth)


start_pop = CCM_result_list[[3]]$annual_components_byeth %>%
  filter(year == 2022, eth_code == eth) %>%
  pull(end_population)

end_pop = CCM_result_list[[3]]$annual_components_byeth %>%
  filter(year == 2032, eth_code == eth) %>%
  pull(end_population)

pct_change = round((end_pop / start_pop - 1) * 100)

net_change = end_pop - start_pop
M = max(abs(waterfallplot_data_ready$changes))
M = ceiling(M / 10^(nchar(round(M)) - 2)) * 10^(nchar(round(M)) - 2) 

#title name for 2022-2032 changes 
title_2232 = paste0("Components of Projected ", eth,  " Population Change")

#---------------------------------------------
waterfall_2232 = ggplot(waterfallplot_data_ready)+
  geom_segment(aes(x = -Inf, xend = start,
                   y = ypos , yend = ypos),
               colour = "grey80",
               linewidth = 0.5)+
  geom_segment(aes(x=start, xend=end,
                   y=ypos, yend =ypos,
                   colour = changes>0),
               arrow = arrow(length = unit(8, "pt")),
               linewidth = 1)+
  geom_segment(aes(x = end,xend = end,
                   y = ypos, yend = next_ypos),
               colour = "grey50",
               linewidth = 1,
               linetype = "dashed")+
  geom_segment(data=waterfallplot_data_ready %>% filter(details=="Emigration"),
               aes(x=end,xend=end,
                   y=1,yend=0),
               colour = "black",
               linewidth = 1)+
  geom_segment(x = 0,xend = 0,
               y = 6, yend = 6.8,
               colour = "black",
               linewidth = 1)+
  # 2022 population circle
  annotate("point",x = 0, y = 6.8,
           shape = 21, fill = "white",
           size = 4, stroke = 1) +
  # 2032 population circle
  annotate("point",x = net_change, y = 0,
           shape = 21, fill = "white",
           size = 4, stroke = 1)+
  # 2022 population label
  annotate("text", x = 0, y = 7.15,
           label = paste0("2022 population: ", 
                          scales::comma(round(start_pop))),
           size = 4.5)+
  # 2032 population label
  annotate("text", x = net_change, y = -0.5,
           label = paste0("2032 projected population: \n", 
                          scales::comma(round(end_pop)),
                          " (",pct_change, "%)"),
           size = 4.5)+
  geom_text(aes(x = label_x,y = label_y,
                label = change_label,
                hjust = label_hjust,
                colour = changes > 0),
            fontface = "bold",
            size = 4,
            show.legend = FALSE)+
  scale_y_continuous(breaks = c(6.1,5.1,4.1,3.1,2.1,1.1), labels = c("Births", "Deaths",
                                                                     "Internal in-migration",
                                                                     "Internal out-migration",
                                                                     "Immigration",
                                                                     "Emigration"),
                     limits = c(-0.6, 7.5)) +
  scale_colour_manual(values = c("TRUE" = "#2166A5",
                                 "FALSE" = "#FF5A60"),
                      guide = "none")+
  scale_x_continuous(limits = c(-M, M) * 2, labels = scales::comma)+
  labs(x="",
       y="",
       title = title_2232,
       subtitle = "Birmingham, 2022-2032")+
  theme_bcc(gridline_x = F,
            gridline_y = F,
            base_size = 12)+
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = -0.75),
        axis.text.y = element_text(size=8))


#----------------------------------------------
#for 2022-2032
#filter it 


waterfallplot_data_ready3247 = waterfallplot_3247data %>% 
  filter(eth_code == eth)


start_pop3247 = CCM_result_list[[3]]$annual_components_byeth %>%
  filter(year == 2032, eth_code == eth) %>%
  pull(end_population)

end_pop3247 = CCM_result_list[[3]]$annual_components_byeth %>%
  filter(year == 2047, eth_code == eth) %>%
  pull(end_population)

pct_change3247 = round((end_pop3247 / start_pop3247 - 1) * 100)

net_change3247 = end_pop3247 - start_pop3247
M3247 = max(abs(waterfallplot_data_ready3247$changes))
M3247 = ceiling(M3247 / 10^(nchar(round(M3247)) - 2)) * 10^(nchar(round(M3247)) - 2) 

#title name for 2022-2032 changes 
title_3247 = paste0("Components of Projected ", eth,  " Population Change")


#---3247 plot----------------------------------------------
waterfall_3247 = ggplot(waterfallplot_data_ready3247)+
  geom_segment(aes(x = -Inf, xend = start,
                   y = ypos , yend = ypos),
               colour = "grey80",
               linewidth = 0.5)+
  geom_segment(aes(x=start, xend=end,
                   y=ypos, yend =ypos,
                   colour = changes>0),
               arrow = arrow(length = unit(8, "pt")),
               linewidth = 1)+
  geom_segment(aes(x = end,xend = end,
                   y = ypos, yend = next_ypos),
               colour = "grey50",
               linewidth = 1,
               linetype = "dashed")+
  geom_segment(data=waterfallplot_data_ready3247 %>% filter(details=="Emigration"),
               aes(x=end,xend=end,
                   y=1,yend=0),
               colour = "black",
               linewidth = 1)+
  geom_segment(x = 0,xend = 0,
               y = 6, yend = 6.8,
               colour = "black",
               linewidth = 1)+
  # 2032 population circle
  annotate("point",x = 0, y = 6.8,
           shape = 21, fill = "white",
           size = 4, stroke = 1) +
  # 2047 population circle
  annotate("point",x = net_change3247, y = 0,
           shape = 21, fill = "white",
           size = 4, stroke = 1)+
  # 2032 population label
  annotate("text", x = 0, y = 7.15,
           label = paste0("2032 population: ", 
                          scales::comma(round(start_pop3247))),
           size = 4.5)+
  # 2047 population label
  annotate("text", x = net_change3247, y = -0.5,
           label = paste0("2047 projected population: \n", 
                          scales::comma(round(end_pop3247)),
                          " (",pct_change3247, "%)"),
           size = 4.5)+
  geom_text(aes(x = label_x,y = label_y,
                label = change_label,
                hjust = label_hjust,
                colour = changes > 0),
            fontface = "bold",
            size = 4,
            show.legend = FALSE)+
  scale_y_continuous(breaks = c(6.1,5.1,4.1,3.1,2.1,1.1), labels = c("Births", "Deaths",
                                                                     "Internal in-migration",
                                                                     "Internal out-migration",
                                                                     "Immigration",
                                                                     "Emigration"),
                     limits = c(-0.6, 7.5)) +
  scale_colour_manual(values = c("TRUE" = "#2166A5",
                                 "FALSE" = "#FF5A60"),
                      guide = "none")+
  scale_x_continuous(limits = c(-M3247, M3247) * 2, labels = scales::comma)+
  labs(x="",
       y="",
       title = title_3247,
       subtitle = "Birmingham, 2032-2047")+
  theme_bcc(gridline_x = F,
            gridline_y = F,
            base_size = 15)+
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = -0.75),
        axis.text.y = element_text(size=8))

#---return both--------------------------------------------
return(waterfall_2232+waterfall_3247)



}



waterfall_func()










#======================================================
#load the ons population projection
#======================================================
snpp2022 = read_csv("data/2022 SNPP Population persons.csv")

bham_snpp = snpp2022 %>% 
  filter(AREA_NAME == "Birmingham", AGE_GROUP == "All ages") %>% 
  pivot_longer(cols = where(is.numeric), names_to = "year", values_to = "count") %>% 
  mutate(series = "ONS SNPP") %>% 
  select(year, series, count) %>% 
  rbind(annual_components %>% 
          select(year, count = end_population) %>% 
          mutate(year   = as.character(year),
                 series = "In-house") %>% 
          select(year, series, count))

#======================================================
#projected population of birmingham: ethnic cohort
#model compared with ons snpp
#======================================================
Comparison_plot_ons_inhouse = snpp2022 %>% 
  filter(AREA_NAME == "Birmingham", AGE_GROUP == "All ages") %>% 
  pivot_longer(cols = where(is.numeric), names_to = "year", values_to = "count") %>% 
  mutate(series = "ONS SNPP") %>% 
  select(year, series, count) %>% 
  rbind(annual_components %>% 
          select(year, count = end_population) %>% 
          mutate(year   = as.character(year),
                 series = "In-house") %>% 
          select(year, series, count)) %>% 
  ggplot(aes(x = year, y = count, colour = series, group = series)) +
  geom_line(size = 1) +
  annotate("segment", x = "2047", xend = "2047", y = 1214469, yend = 1261990,
           colour = "red", linewidth = 1,
           arrow = grid::arrow(ends = "both", type = "closed", length = grid::unit(0.15, "cm"))) +
  annotate("text", x = "2046", y = 1240000, label = "4%", colour = "red") +
  scale_y_continuous(limits = c(1100000, 1300000)) +
  theme_bcc(base_size = 11) +
  scale_colour_bcc(palette = "multi") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Projected Population of Birmingham: \nEthnic Cohort Model Compared with ONS SNPP") +
  labs(y = "Population")

ggsave("fig/11.1_comparison_plot_ons_inhouse.png",
       Comparison_plot_ons_inhouse,
       width = pixel_2_in(744, 448)[1], height = pixel_2_in(744, 448)[2],
       units = "in",
       dpi = 600)

#======================================================
#broad group lookup and harmonised colours
#======================================================
options(scipen = 999)

broad_group_map = tibble::tribble(
  ~eth_code, ~ethnic_group,          ~broad_group, ~broad_colours,
  "WBI",     "White British",        "White",      "blue",
  "WHO",     "White Other",          "White",      "blue",
  "MIX",     "Mixed",                "Mixed",      "green",
  "IND",     "Indian",               "Asian",      "orange",
  "PAK",     "Pakistani",            "Asian",      "orange",
  "BAN",     "Bangladeshi",          "Asian",      "orange",
  "CHI",     "Chinese",              "Asian",      "orange",
  "OAS",     "Other Asian",          "Asian",      "orange",
  "BLA",     "Black African",        "Black",      "purple",
  "BLC",     "Black Caribbean",      "Black",      "purple",
  "OBL",     "Other Black",          "Black",      "purple",
  "OTH",     "Other ethnic group",   "Other",      "pink"
)

bcc_pal(palette = "blue")(10)[2]

broad_group_map = broad_group_map %>% 
  group_by(broad_colours) %>% 
  mutate(shade_number      = row_number(),
         harmonised_colour = bcc_pal(palette = first(broad_colours))(n() + 2)[shade_number]) %>% 
  ungroup() %>% 
  mutate(broad_colours_hex = bcc_cols(broad_colours)) %>% 
  mutate(broad_group = factor(broad_group, levels = c("White", "Mixed", "Asian", "Black", "Other")),
         eth_code    = factor(eth_code, levels = c(
           "WBI", "WHO",
           "MIX",
           "IND", "PAK", "BAN", "CHI", "OAS",
           "BLA", "BLC", "OBL",
           "OTH"
         ))) %>% 
  arrange(broad_group, eth_code)

# broad_palette_names = c(
#   "White" = "blue",
#   "Mixed" = "green",
#   "Asian" = "orange",
#   "Black" = "purple",
#   "Other" = "pink"
# )

#---broad group line plot------------------------------
broad_colour_values = broad_group_map %>% 
  distinct(eth_code, harmonised_colour) %>% 
  mutate(eth_code = as.character(eth_code)) %>% 
  tibble::deframe()

annual_components_bybroadeth_plot = CCM_result_list[[3]]$annual_components_byeth %>% 
  left_join(broad_group_map, by = "eth_code") %>% 
  group_by(year, broad_group) %>% 
  summarise(end_population = sum(end_population, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = year, y = end_population, colour = broad_group, group = broad_group)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
  scale_y_continuous(breaks = seq(0, 600000, by = 100000), limits = c(0, 550000)) +
  scale_colour_manual(values = c("White" = "#00A9E0",
                                 "Mixed" = "#75BC22",
                                 "Asian" = "#DC582A",
                                 "Black" = "#84329B",
                                 "Other" = "#D00070")) +
  labs(
    title  = "Projected population by broad ethnic group",
    x      = NULL,
    y      = "Population",
    colour = "Broad ethnic group"
  ) +
  theme_bcc(base_size = 11) +
  theme(title = element_text(size = 9))

#---harmonised colours and group order-----------------
harmonised_colour_values = broad_group_map %>% 
  distinct(eth_code, harmonised_colour) %>% 
  mutate(eth_code = as.character(eth_code)) %>% 
  tibble::deframe()

eth_code_order = c(
  "WBI", "WHO",
  "MIX",
  "IND", "PAK", "BAN", "CHI", "OAS",
  "BLA", "BLC", "OBL",
  "OTH"
)

#---harmonised group line plot-------------------------
plot_result_harmonised_line = function(eth){
  
  annual_components_byeth_plot = CCM_result_list[[3]]$annual_components_byeth %>% 
    mutate(eth_code = factor(eth_code, levels = eth_code_order)) %>% 
    filter(eth_code %in% eth) %>% 
    ggplot(aes(x = year, y = end_population, colour = eth_code, group = eth_code)) +
    geom_line(size = 0.8) +
    scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
    scale_colour_manual(values = harmonised_colour_values, breaks = eth_code_order) +
    labs(
      title  = "Projected population by harmonised ethnic group",
      x      = NULL,
      y      = "Population",
      colour = "Broad ethnic group"
    ) +
    theme_bcc(base_size = 11) +
    theme(title = element_text(size = 9))
  
  return(annual_components_byeth_plot)
  
}

plot_result_harmonised_line(eth = c("IND", "PAK", "BAN", "CHI", "OAS"))

#---combined broad + harmonised line plots-------------
annual_components_byeth_plot = CCM_result_list[[3]]$annual_components_byeth %>% 
  mutate(eth_code = factor(eth_code, levels = eth_code_order)) %>% 
  ggplot(aes(x = year, y = end_population, colour = eth_code, group = eth_code)) +
  geom_line(size = 0.8) +
  scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
  scale_y_continuous(breaks = seq(0, 600000, by = 100000), limits = c(0, 550000)) +
  scale_colour_manual(values = harmonised_colour_values, breaks = eth_code_order) +
  labs(
    title  = "Projected population by harmonised ethnic group",
    x      = NULL,
    y      = "Population",
    colour = "Broad ethnic group"
  ) +
  theme_bcc(base_size = 11) +
  theme(title = element_text(size = 9))

combined_projection_plot_eth =
  annual_components_bybroadeth_plot +
  annual_components_byeth_plot

ggsave("fig/11.2_combined_projection_plot_eth.png",
       combined_projection_plot_eth,
       width = pixel_2_in(996, 862)[1], height = pixel_2_in(996, 862)[2],
       units = "in",
       dpi = 600)

#======================================================
#women of childbearing age by ethnicity
#======================================================
full_population_projection = CCM_result_list[[2]]

women_15_49_by_ethnicity = full_population_projection %>% 
  filter(sex == "Female", age >= 15, age <= 49) %>% 
  group_by(year, eth_code) %>% 
  summarise(women_15_49 = sum(population, na.rm = TRUE), .groups = "drop")

women_15_49_by_ethnicity %>% 
  ggplot(aes(x = year, y = women_15_49, colour = eth_code)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_bcc(palette = "multi") +
  labs(
    title    = "Projected women aged 15–49 by ethnicity",
    subtitle = "Birmingham, 2021–2061",
    x        = "Year",
    y        = "Women aged 15–49",
    colour   = "Ethnic group"
  ) +
  theme_bcc(legend_position = "bottom", gridline_x = FALSE)

women_15_49_by_ethnicity %>% 
  ggplot(aes(x = year, y = women_15_49)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ eth_code, scales = "free_y") +
  labs(
    title    = "Projected women aged 15–49 by ethnicity",
    subtitle = "Birmingham, 2021–2061",
    x        = "Year",
    y        = "Women aged 15–49"
  ) +
  theme_bcc(gridline_x = FALSE)

#======================================================
#waterfall plot of components of change
#======================================================
waterfallplot_data = annual_components_byeth %>% 
  filter(year %in% 2023:2032) %>% 
  group_by(eth_code) %>% 
  summarise(Births       = sum(births),
            Deaths       = sum(deaths) * -1,
            Internal_in  = sum(internal_in),
            Internal_out = sum(internal_out) * -1,
            Immigration  = sum(international_in),
            Emigration   = sum(international_out) * -1,
            .groups      = "drop") %>% 
  pivot_longer(cols = -eth_code, values_to = "changes", names_to = "details") %>% 
  mutate(details = factor(details,
                          levels = c("Births", "Deaths",
                                     "Internal_in", "Internal_out",
                                     "Immigration", "Emigration"))) %>% 
  arrange(eth_code, details) %>% 
  group_by(eth_code) %>%                      # <- grouping FIRST
  mutate(
    end          = cumsum(changes),
    start        = lag(end, default = 0),
    ypos         = rev(seq_len(n())),
    next_ypos    = lead(ypos, default = 0),   # 6..1 within each group
    pad          = max(abs(changes)) * 0.03,
    label_x      = end + ifelse(changes > 0, pad, -pad),
    label_y      = ypos,
    label_hjust  = ifelse(changes > 0, 0, 1),
    change_label = paste0(ifelse(changes > 0, "+", ""), scales::comma(round(changes)))
  ) %>% 
  ungroup() %>% 
  filter(eth_code == "BAN")

start_pop = annual_components_byeth %>% 
  filter(year == 2022, eth_code == "BAN") %>% 
  pull(end_population)

end_pop = annual_components_byeth %>% 
  filter(year == 2032, eth_code == "BAN") %>% 
  pull(end_population)

pct_change = round((end_pop / start_pop - 1) * 100)

net_change = end_pop - start_pop
M = max(abs(componentchange_data$changes))
M = ceiling(M / 10^(nchar(round(M)) - 2)) * 10^(nchar(round(M)) - 2)  # up to 2 s.f.

waterfall_plot = ggplot(componentchange_data) +
  geom_segment(aes(x = -Inf, xend = start,
                   y = ypos, yend = ypos),
               colour = "grey80",
               linewidth = 0.5) +
  geom_segment(aes(x = start, xend = end,
                   y = ypos, yend = ypos,
                   colour = changes > 0),
               arrow = arrow(length = unit(8, "pt")),
               linewidth = 1) +
  geom_segment(aes(x = end, xend = end,
                   y = ypos, yend = next_ypos),
               colour = "grey50",
               linewidth = 1,
               linetype = "dashed") +
  geom_segment(data = componentchange_data %>% filter(details == "Emigration"),
               aes(x = end, xend = end,
                   y = 1, yend = 0),
               colour = "black",
               linewidth = 1) +
  geom_segment(x = 0, xend = 0,
               y = 6, yend = 6.8,
               colour = "black",
               linewidth = 1) +
  #---2022 population circle---------------------------
annotate("point", x = 0, y = 6.8,
         shape = 21, fill = "white",
         size = 4, stroke = 1) +
  #---2032 population circle---------------------------
annotate("point", x = net_change, y = 0,
         shape = 21, fill = "white",
         size = 4, stroke = 1) +
  #---2022 population label----------------------------
annotate("text", x = 0, y = 7.15,
         label = paste0("2022 population: ",
                        scales::comma(round(start_pop))),
         size = 4.5) +
  #---2032 population label----------------------------
annotate("text", x = net_change, y = -0.5,
         label = paste0("2032 projected population: \n",
                        scales::comma(round(end_pop)),
                        " (", pct_change, "%)"),
         size = 4.5) +
  geom_text(aes(x = label_x, y = label_y,
                label = change_label,
                hjust = label_hjust,
                colour = changes > 0),
            fontface = "bold",
            size = 4,
            show.legend = FALSE) +
  scale_y_continuous(breaks = c(6.1, 5.1, 4.1, 3.1, 2.1, 1.1),
                     labels = c("Births", "Deaths",
                                "Internal in-migration",
                                "Internal out-migration",
                                "Immigration",
                                "Emigration"),
                     limits = c(-0.6, 7.5)) +
  scale_colour_manual(values = c("TRUE"  = "#2166A5",
                                 "FALSE" = "#FF5A60"),
                      guide = "none") +
  scale_x_continuous(limits = c(-M, M) * 2, labels = scales::comma) +
  labs(x = "",
       y = "",
       title = "How the population is projected to change, \nby cause and ethnicity") +
  theme_bcc(gridline_x = F,
            gridline_y = F,
            base_size = 12) +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y  = element_blank(),
        axis.line.x  = element_blank(),
        axis.ticks.y = element_blank())