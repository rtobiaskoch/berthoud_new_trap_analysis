#get number of active traps. For the purposes of historical calculations not going to consider malfunctioning traps
use_active_trap = T

if(use_active_trap == T) {
  routine_traps = read.csv(fn_trap)
  
  trap_data0 = routine_traps %>% 
    filter(active == 1 & method == "L") %>%
    group_by(zone) %>%
    summarize(trap_L = n()) 
  
  trap_data_fc = routine_traps %>% 
    filter(active == 1 & method == "L") %>% #only keep active light traps
    filter(zone %in% fc_zones) %>% #keep zones in group
    mutate(zone = "FC") %>% #change zones to FC
    group_by(zone) %>%
    summarize(trap_L = n()) 
  
} else { #if you want to calculate by the number of unique traps sampled each week
  
  trap_data0 = data_input %>% 
    filter(method == "L") %>%
    distinct(year, week, zone, trap_id) %>%
    group_by(year, week, zone) %>%
    summarize(trap_L= n())
  
  trap_data_fc = data_input %>% 
    filter(method == "L") %>% #only keep active light traps
    filter(zone %in% fc_zones) %>% #keep zones in group
    distinct(year, week, zone, trap_id) %>%
    mutate(zone = "FC") %>% #change zones to FC
    group_by(year, week, zone) %>%
    summarize(trap_L = n())
}

trap_data = rbind(trap_data0, trap_data_fc)
rm(trap_data0, trap_data_fc)