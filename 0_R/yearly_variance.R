
list2env(readRDS("1_input/config_params.RDS"),          
         envir = .GlobalEnv)

# if(!file.exists(fn_database_input)){
#   gsheet_pull(database_gsheet_key, "data", fn_database_input)
# }

pacman::p_load(tidyverse, PooledInfRate, patchwork, rquery)

database = read.csv(fn_database_input)

be = database %>% 
  filter(zone == "BE")


devtools::source_url("https://raw.githubusercontent.com/rtobiaskoch/wnv-s_data_tools/main/0_R/calc_vi_stats.R")

grp_vars = c("year", "week", "zone")

#replace abund from the culex trap data
data_zone_wk_update = read.csv("1_input/data_zone_wk_new_update.csv") %>%
  filter(year < 2024 & zone == "BE" & spp == "All") %>%
  group_by(year, week, zone, abund) %>%
  select(year, week, zone, abund)

#note with addition of culex data sd is wrong. I think this is okay because it is not being vizualized
#recalc 
be_vi = calc_vi(be, grp_vars) %>%
  mutate(year = as.integer(year),
         week = as.integer(week)) %>%
  #add the abund from the culex_sheet that is in data_zone_wk_update
  left_join(data_zone_wk_update, by = grp_vars) %>%
  mutate(abund = coalesce(abund.y, abund.x)) %>%
  select(-abund.x, -abund.y) %>%
  #recalc VI
    mutate(vi = round(abund * pir,4),
           vi_lci = round(abund * pir_lci,4),
           vi_uci = round(abund * pir_uci,4)
    ) %>%
  mutate(week = factor(week),
         year2 = case_when(year == 2024 ~ "2024",
                           year == 2023 ~ "2023",
                           T ~ "2017-2022 Baseline"),
         
         year2 = factor(year2),
         year = factor(year)) %>%
  group_by(year2, week, zone) %>%
  summarise(across(where(is.numeric), ~ round(mean(.x),4)), .groups = "drop")


write.csv(be_vi, "3_output/berthoud_trap_analysis_17-24.csv", row.names = F)

 fun_line_plot = function(df, val, yr, axis) {
  ggplot(df, aes(x = week, y = {{val}}, group = {{yr}}, color = {{yr}})) +
    geom_line(size = 1.05) +
    geom_point(size = 2) +
    # geom_point() +
    ylab(axis) +
     scale_color_manual(values = c("2023" = "#820263", "2024" = "#e9724c",
                                   "2017-2022 Baseline" = "grey50" )) +
   # scale_color_grey(start = 0.9, end = 0.1) +
    theme_classic()
}

 p_abund_line = fun_line_plot(be_vi, abund, year2, "ABUNDANCE") +
   theme(axis.title.x = element_blank())
 p_abund_line
 


p_pir_line = fun_line_plot(be_vi, pir, year2, "POOLED INFECTION RATE") +
  theme(axis.title.x = element_blank())
p_pir_line

y_min = floor(min(be_vi$vi))
y_max = ceiling(max(be_vi$vi))

p_vi_line = fun_line_plot(be_vi, vi, year2, "VECTOR INDEX")  +
  geom_hline(yintercept = 0.75, color = "red", linetype = 2) +
  scale_y_continuous(breaks = seq(y_min, y_max, by = 1), limits = c(y_min, y_max))
p_vi_line


p_line = (p_abund_line/p_pir_line/p_vi_line) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_blank())
p_line  

ggsave("3_output/line_variance_17-24.pdf", width = 14, height = 10)
