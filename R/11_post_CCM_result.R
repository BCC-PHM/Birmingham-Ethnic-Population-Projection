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
    
    natural_change =births - deaths,
    
    internal_in =sum(internal_in_migrants, na.rm = TRUE),
    
    internal_out =sum(internal_out_migrants, na.rm = TRUE),
    
    net_internal =internal_in - internal_out,
    
    international_in =sum(international_immigrants, na.rm = TRUE),
    
    international_out =sum(international_emigrants, na.rm = TRUE),
    
    net_international =international_in - international_out,
    
    projected_change =natural_change + net_internal +net_international,
    
    end_population = sum(end_population, na.rm = TRUE),
    start_pop = end_population-projected_change,
    
    .groups = "drop"
  )

annual_components_byeth = full_component_projection %>%
  group_by(year,eth_code) %>%
  summarise(
    births = sum(births, na.rm = TRUE),
    deaths = sum(total_deaths, na.rm = TRUE),
    
    natural_change =births - deaths,
    
    internal_in =sum(internal_in_migrants, na.rm = TRUE),
    
    internal_out =sum(internal_out_migrants, na.rm = TRUE),
    
    net_internal =internal_in - internal_out,
    
    international_in =sum(international_immigrants, na.rm = TRUE),
    
    international_out =sum(international_emigrants, na.rm = TRUE),
    
    net_international =international_in - international_out,
    
    projected_change =natural_change + net_internal +net_international,
    
    end_population = sum(end_population, na.rm = TRUE),
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



full_population_projection = CCM_result_list[[2]]

women_15_49_by_ethnicity =
  full_population_projection %>%
  filter(
    sex == "Female",
    age >= 15,
    age <= 49
  ) %>%
  group_by(year, eth_code) %>%
  summarise(
    women_15_49 = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

women_15_49_by_ethnicity %>%
  ggplot(
    aes(
      x = year,
      y = women_15_49,
      colour = eth_code
    )
  ) +
  geom_line(linewidth = 1) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_colour_bcc(
    palette = "multi"
  ) +
  labs(
    title = "Projected women aged 15–49 by ethnicity",
    subtitle = "Birmingham, 2021–2061",
    x = "Year",
    y = "Women aged 15–49",
    colour = "Ethnic group"
  ) +
  theme_bcc(
    legend_position = "bottom",
    gridline_x = FALSE
  )



women_15_49_by_ethnicity %>%
  ggplot(
    aes(
      x = year,
      y = women_15_49
    )
  ) +
  geom_line(linewidth = 1) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  facet_wrap(
    ~ eth_code,
    scales = "free_y"
  ) +
  labs(
    title = "Projected women aged 15–49 by ethnicity",
    subtitle = "Birmingham, 2021–2061",
    x = "Year",
    y = "Women aged 15–49"
  ) +
  theme_bcc(
    gridline_x = FALSE
  )


#==========================================================================
waterfallplot_data  = annual_components_byeth %>% 
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



waterfall_plot = ggplot(componentchange_data)+
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
  geom_segment(data=componentchange_data %>% filter(details=="Emigration"),
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
       title = "How the population is projected to change, \nby cause and ethnicity")+
  theme_bcc(gridline_x = F,
            gridline_y = F,
            base_size = 12)+
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank())


