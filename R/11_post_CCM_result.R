setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/BCC ethnic population projection")
# ============================================================
# 11_post_CCM_result.R
#visualise the results
# ============================================================
library(tidyverse)
library(readr)
library(readxl)
library(bcctheme)
library(patchwork)


#===========================================================
# just handy function adjusting ggsave
pixel_2_in = function(width,
                      height){
  width_in = width/96
  height_in = height/96
  
  result = c(width_in,height_in)
  return(result)
}



#============================================================

CCM_result_list = readRDS("data/processed/CCM_result_list.rds")

full_component_projection = CCM_result_list[[1]]

annual_components = full_component_projection %>%
  group_by(year) %>%
  summarise(
    births = sum(births, na.rm = TRUE),
    deaths = sum(total_deaths, na.rm = TRUE),
    
    natural_change =
      births - deaths,
    
    internal_in =
      sum(internal_in_migrants, na.rm = TRUE),
    
    internal_out =
      sum(internal_out_migrants, na.rm = TRUE),
    
    net_internal =
      internal_in - internal_out,
    
    international_in =
      sum(international_immigrants, na.rm = TRUE),
    
    international_out =
      sum(international_emigrants, na.rm = TRUE),
    
    net_international =
      international_in - international_out,
    
    projected_change =
      natural_change +
      net_internal +
      net_international,
    
    end_population =
      sum(end_population, na.rm = TRUE),
    start_pop = end_population-projected_change,
    
    .groups = "drop"
  )

annual_components_byeth = full_component_projection %>%
  group_by(year,eth_code) %>%
  summarise(
    births = sum(births, na.rm = TRUE),
    deaths = sum(total_deaths, na.rm = TRUE),
    
    natural_change =
      births - deaths,
    
    internal_in =
      sum(internal_in_migrants, na.rm = TRUE),
    
    internal_out =
      sum(internal_out_migrants, na.rm = TRUE),
    
    net_internal =
      internal_in - internal_out,
    
    international_in =
      sum(international_immigrants, na.rm = TRUE),
    
    international_out =
      sum(international_emigrants, na.rm = TRUE),
    
    net_international =
      international_in - international_out,
    
    projected_change =
      natural_change +
      net_internal +
      net_international,
    
    end_population =
      sum(end_population, na.rm = TRUE),
    start_pop = end_population-projected_change,
    
    .groups = "drop"
  )





#=========================================================================
#load the ons population projection 
snpp2022 = read_csv("data/2022 SNPP Population persons.csv")


bham_snpp = snpp2022 %>% 
  filter(AREA_NAME == "Birmingham", AGE_GROUP == "All ages") %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "year",
               values_to = "count") %>% 
  mutate(series = "ONS SNPP") %>% 
  select(year, series, count) %>% 
  rbind(annual_components %>% 
          select(year, count = end_population) %>% 
          mutate(year = as.character(year),
                 series = "In-house")%>% 
          select(year, series,count))

#===================================================================================
#Projected Population of Birmingham: Ethnic Cohort Model Compared with ONS SNPP
Comparison_plot_ons_inhouse = snpp2022 %>% 
  filter(AREA_NAME == "Birmingham", AGE_GROUP == "All ages") %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "year",
               values_to = "count") %>% 
  mutate(series = "ONS SNPP") %>% 
  select(year, series, count) %>% 
  rbind(annual_components %>% 
          select(year, count = end_population) %>% 
          mutate(year = as.character(year),
                 series = "In-house")%>% 
          select(year, series,count)) %>% 
  ggplot(aes(x=year,y=count,colour= series, group=series))+
  geom_line(size=1)+
  annotate("segment", x = "2047",xend = "2047", y=1214469, yend = 1261990, 
           colour = "red",linewidth = 1,
           arrow = grid::arrow(ends = "both", type="closed",length = grid::unit(0.15, "cm")))+
  annotate("text", x="2046", y=1240000, label = "4%",colour = "red")+
  scale_y_continuous(limits = c(1100000,1300000))+
  theme_bcc(base_size = 11)+
  scale_colour_bcc(palette = "multi")+
  theme( axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Projected Population of Birmingham: \nEthnic Cohort Model Compared with ONS SNPP")+
  labs(y="Population")

ggsave("fig/11.1_comparison_plot_ons_inhouse.png", 
       Comparison_plot_ons_inhouse ,
       width = pixel_2_in(744,448)[1], height = pixel_2_in(744,448)[2],
       units = "in",
       dpi=600)

#===================================================================================
options(scipen = 999)

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

bcc_pal(palette = "blue")(10)[2]

broad_group_map = broad_group_map %>% 
  group_by(broad_colours) %>% 
  mutate(shade_number = row_number(),
   harmonised_colour = bcc_pal(palette = first(broad_colours))(n() + 2)[shade_number]) %>% 
  ungroup() %>% 
  mutate(broad_colours_hex = bcc_cols(broad_colours)) %>% 
  mutate(broad_group = factor(broad_group,levels = c("White","Mixed","Asian","Black","Other")),
         eth_code = factor(eth_code, levels = c(
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
#-------------------------------------------------------
broad_colour_values = broad_group_map %>%
  distinct(eth_code, harmonised_colour) %>%
  mutate(eth_code = as.character(eth_code)) %>%
  tibble::deframe()


annual_components_bybroadeth_plot = annual_components_byeth %>% 
  left_join(broad_group_map, by = "eth_code") %>% 
  group_by(year, broad_group, broad_colours_hex) %>%
  summarise(
    end_population = sum(end_population, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x=year,y=end_population,colour= broad_group, group=broad_group))+
  geom_line(size=0.8)+
  scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
  scale_y_continuous(breaks = seq(0,600000,by = 100000),limits = c(0,550000))+
  scale_colour_manual(
    values = broad_colour_values) +
  labs(
    title = "Projected population by broad ethnic group",
    x = NULL,
    y = "Population",
    colour = "Broad ethnic group"
  ) +
  theme_bcc(base_size=11)+
  theme(title = element_text(size = 9))


#-------------------------------------------------------

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

annual_components_byeth_plot = annual_components_byeth %>%
  mutate(
    eth_code = factor(
      eth_code,
      levels = eth_code_order
    )
  ) %>% 
  ggplot(aes(x=year,y=end_population, colour = eth_code,
             group = eth_code))+
  geom_line(size=0.8)+
  scale_x_continuous(breaks = seq(2022, 2047, by = 5)) +
  scale_y_continuous(breaks = seq(0,600000,by = 100000), limits = c(0,550000))+
  scale_colour_manual(
    values = harmonised_colour_values,
    breaks = eth_code_order) + 
  labs(
    title = "Projected population by harmonised ethnic group",
    x = NULL,
    y = "Population",
    colour = "Broad ethnic group"
  ) +
  theme_bcc(base_size=11)+
  theme(title = element_text(size = 9))


combined_projection_plot_eth =
  annual_components_bybroadeth_plot +
  annual_components_byeth_plot

ggsave("fig/11.2_combined_projection_plot_eth.png", 
       combined_projection_plot_eth ,
       width = pixel_2_in(996,862)[1], height = pixel_2_in(996,862)[2],
       units = "in",
       dpi=600)













