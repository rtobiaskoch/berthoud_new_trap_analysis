#Config
rm(list = ls())
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(googlesheets4, googledrive, rio, readxl, openxlsx, googledrive, #importing and exporting
                 tidyverse, janitor, lubridate, rquery, #manipulation
                 PooledInfRate, #analysis
                 ggpubr, wesanderson, paletteer, leaflet, patchwork# plotting
                )
  
                })


if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
  p_load(devtools)
  devtools::install_github("https://github.com/CDCgov/PooledInfRate",
                           build_vignettes = TRUE)
}

if(!file.exists("data_input/config_params.RDS")) {
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FUNCTIONS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
source(url("https://raw.githubusercontent.com/rtobiaskoch/TK_useful_functions/refs/heads/main/gsheet_pull.R"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#DATA PARAMETERS:
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
args = commandArgs(trailingOnly = T)

year_filter = as.integer(args[1])
if(!is.integer(year_filter)) {print("your year filter must be numeric")}
week_filter = as.integer(args[2])
if(!is.integer(year_filter)) {print("your week filter must be numeric")}

week_filter_yr= 23:week_filter
week_filter_hx = 23:37
year_filter_hx = seq(year_filter-11, year_filter-1, by = 1)



copy_threshold = 500
rn_threshold = 34000
vi_threshold = 0.75
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#OUTPUT FILE NAME GENERATION
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(length(year_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_year = paste0(min(year_filter), "-", max(year_filter))
} else { #otherwise fn_year stays the same
  fn_year = year_filter
}

if(length(week_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_week = paste0(min(week_filter), "-", max(week_filter))
} else { #otherwise fn_year stays the same
  fn_week = week_filter
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FILE NAMES INPUT
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
database_gsheet_key = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
trap_gsheet_key = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"
trap_malfunction_key = "1dsTyvZoCN6NUJlTcDLINMfxuGZdJuP2ADpn8noQwL6Q"
#trap_active_key = "1SA_PE74KLH6_jG3yR49e8py1uXgb_C02Q3Iz9MWivrY"
standards_key = "1bSMYQ4bZ9uBfrOQ6ylsegNmmGYdf9YFVbxB4qBhnFQo"
routine_trap_tl_key = "1kIOqx6CldJ3ivXu9_ws60qqxrlLSmjHRz_tkv03vhqs"




fn_gdrive_database = "wnv-s_database"

#weekly input
fn_datasheet_input = "1_input/datasheet" #replaced vdci and cdc input because also have boulder so all in one place
fn_culex_sheet = "1_input/culex_sheet_combined.csv"
fn_platemap = "1_input/platemap"
fn_pcr = "1_input/pcr"

fn_trap_malfunction = "1_input/trap_malfunction.csv"
fn_trap_active = "1_input/trap_active.csv"
fn_trap = "1_input/foco_trap.csv"
fn_database_input = "1_input/wnv-s_database.csv" 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# FILE NAMES MID
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn_database_update = "2_mid/wnv-s_database_update.csv"

fn_datasheet_clean = paste0("2_mid/","y",fn_year, "_", "w",fn_week, "_datasheet.csv")

fn_datasheet_clean_test = paste0("2_mid/","y",fn_year, "_", "w",fn_week, "_datasheet_test.csv")

fn_weekly_input_format_mid = "2_mid/weekly_input_format_mid.RData"

fn_cq_out = paste0("2_mid/","y",fn_year, "_", "w",fn_week, "_platemap.csv")

fn_abund_out = paste0("2_mid/","y",fn_year, "_", "w",fn_week, "_abundance")

fn_pools_mid = paste0("2_mid/","y",fn_year, "_", "w",fn_week, "_pools.csv")

fn_inactive_trap = "2_mid/inactive_traps.csv"

fn_func_trap = "2_mid/functional_traps.csv"

fn_max_trap_yr = "2_mid/max_trap_zone_yr.csv"

fn_trap_p_wk = "2_mid/trap_p_wk.csv"


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#FILE NAMES OUTPUT
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fn_gdrive_archive = paste0("wnv-s_database_pre_y",year_filter, "_w", week_filter,".gsheet")

fn_output = paste0("3_output/","y",fn_year, "_", "w",fn_week, "_data_update.csv")

fn_weekly_input_format = "3_output/weekly_input_format.csv"

fn_stds_ctrl_slev_bird = "3_output/std_ctrl_slev_bird.csv"

fn_non_database_sample = "3_output/non_database_samples(std-ctrl-bird-etc).csv"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#SELECTION COLUMN SETTINGS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
trap_col = c("zone", "lat", "long")

input_data_col = c("csu_id", "trap_id", "year", "week", "trap_date", 
                   "county", "method", "spp", "total", "test_code", "zone")

data_col = c("csu_id", "trap_id", "year", "week", "trap_date", 
             "county", "method", "genus", "spp", "sex", "no_gravid",
             "no_deplete", "total", "test_code", "seq", "cq", 
             "zone", "lat", "long")

database_col = c( "csu_id", "trap_id", "year", "week", "trap_date", "county", "method", "spp",
  "total", "test_code", "cq", "copies_WNV", "seq", "zone", "cq", "lat", "long")

pcr_check_col <- c("csu_id", "test_code", "ct_threshold", "plate", "copies_SLEV", "copies_WNV", "cq_SLEV", "cq")

class_col <- c("csu_id" = "character", 
            "trap_id" = "character", 
            "year" = "numeric", 
            "week" = "numeric", 
            "trap_date" = "character", 
            "county" = "character", 
            "method" = "character", 
            "genus" = "character", 
            "spp" = "character", 
            "sex" = "logical", 
            "no_gravid" = "numeric", 
            "no_deplete" = "numeric", 
            "total" = "numeric", 
            "test_code" = "numeric", 
            "seq" = "numeric", 
            "cq" = "numeric",
            "zone" = "character",
            "lat" = "numeric",
            "long" = "numeric")

rename_col <- c("csu_id" = "CSU Pool Number (CMC Enters)" , 
               "trap_id" = "Collection Site       (Trap ID)", 
               "year" = "Year", 
               "week" = "Week", 
               "trap_date" = "Trap Date", 
               "county" = "County", 
               "method" = "L/G", 
               "genus" = "Genus", 
               "spp" = "Species", 
               "sex" = "Sex", 
               "no_gravid" = "No. Gravid", 
               "no_deplete" = "No. Deplete", 
               "total" = "Total", 
               "test_code" = "Test Code (CSU Enters)" , 
               "zone" = "Zone")


weekly_input_report_format <- c(
  "Year",
  "CSU Pool Number (CMC Enters)",
  "IDA Pool (CSU Enters, Leave Blank)",
  "Week",
  "Trap Date",
  "County",
  "Account",
  "Collection Site       (Trap ID)" ,
  "Zone",
  "Method",
  "Genus",
  "SPP",
  "Sex",
  "No. Gravid",
  "No. Deplete",
  "Total",
  "Test Code (CSU Enters)",
  "Test Result (CSU Enters)",
  "Comments"
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GROUP VARS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
grp_vars = c("year", "week", "zone", "spp")
hx_grp_vars = c("week", "zone")
zone_lvls = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC")
non_routine_zones = c("BC")
fc_zones = c("NW", "NE", "SE","SW")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#COLOR SETTINGS
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
color_palette = wes_palette('Darjeeling1')

# year_col_highlight = "2023"
# 
# year_cols_df = data.frame(year = 2000:2050) %>%
#   mutate(year = as.factor(year)) %>%
#   mutate(col = if_else(as.character(year) == year_col_highlight,
#                        year_col_highlight,
#                        "other"))
# 
# year_cols = c("2023" = "#c5283d",
#               "other" = "grey50")

#https://coolors.co/palette/fb8b24-d90368-820263

mozzy_pal = c("hx_Tarsalis" = "grey50",
              "hx_Pipiens" = "grey30",
              "current_Tarsalis" = "#e9724c",
              "current_Pipiens" = "#820263")

mozzy_pal2 = c("Pipiens" = "#820263",
               "Tarsalis" = "#e9724c",
               "All" = "#faa916")


mozzy_pal3 = c("#ffc857", "#e9724c", "#c5283d")

curr_hx_pal = c("current" = "#e9724c",
                "hx"      = "grey50")


status_pal = c("original" = "grey50",
               "combined" = "#e9724c")

status_pal2 = c("original" = "grey50",
               "new" = "#e9724c")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


all_params <- ls(envir = .GlobalEnv)

# Create a list containing all the values of these objects
all_params_list <- mget(all_params, envir = .GlobalEnv)

saveRDS(all_params_list, "1_input/config_params.RDS")

} else { #end of if config_params.RDS file exists
  
  list2env(readRDS("1_input/config_params.RDS"), 
           envir = .GlobalEnv)
  cat(paste0("config file already exists in 1_input/config_params.RDS", "\n",
             "year filter = ", year_filter, "\n",
             "week filter = ", week_filter)
       )
  
}
