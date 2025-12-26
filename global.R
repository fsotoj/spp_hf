library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(sf)
library(dplyr)
library(tidyr)
library(DT)
library(leaflet)
library(classInt)
library(shinyWidgets)
library(shinyjs)
library(mapview)
library(webshot)
library(jsonlite)
library(tibble)
library(purrr)
library(echarts4r)
library(highcharter)
library(htmltools)
library(httr2)
library(promises)
library(future)
library(later)

source("map_module.R")
source("hc_line_module.R")
source("get_jstree_data.R")
source("camera_module_v2.R")
#source("camera_module.R")
source("about_spp_module.R")
source("spp_list_module.R")



# Define the data fetcher
get_data <- function(file_name) {
  base_url <- "https://raw.githubusercontent.com/fsotoj/spp-data/main/data/"
  local_dir <- "data_cache"
  if (!dir.exists(local_dir)) dir.create(local_dir)
  
  local_path <- file.path(local_dir, file_name)
  
  # Only download if the file doesn't exist locally in the container
  if (!file.exists(local_path)) {
    # URLencode handles spaces and parentheses in filenames
    target_url <- utils::URLencode(paste0(base_url, file_name))
    message(paste("Downloading:", file_name))
    download.file(target_url, destfile = local_path, mode = "wb")
  }
  
  return(local_path)
}


data_info <- read.xlsx(get_data("dict_new.xlsx")) %>% 
  select(Category = category, Variable = variable, Description = description_for_ui)

dict <- read.xlsx(get_data("dict_new.xlsx")) %>% 
  filter(scope == "subnational")

sled_names <- dict %>% 
  filter(dataset == "Legislative Elections", viewable_map == 1) %>% pull(variable)


NED <- read.xlsx(get_data("NED (v.0.1).xlsx")) %>%
  mutate(ideo_party_nat_exe = as.double(ideo_party_nat_exe),
         start_date_head_nat_exe = as.Date(start_date_head_nat_exe - 2, origin = "1900-01-01"),
         end_date_head_nat_exe   = as.Date(end_date_head_nat_exe - 2, origin = "1900-01-01"))

SEED <- read.xlsx(get_data("SEED SHINY (v.0.1).xlsx"))
SED  <- read.xlsx(get_data("SED (v.0.1).xlsx"))
SLED <- read.xlsx(get_data("SLED (v.0.1).xlsx"))
SDI  <- read.xlsx(get_data("SDI (v.1).xlsx")) 

cols_to_fill <- c("chamber_sub_leg",as.vector(outer(setdiff(sled_names,c("chamber_sub_leg","concurrent_election_with_nat_sub_leg")), c("_1","_2"), paste0)))

SLED_wide <- SLED %>%
  select(country_state_code, year, chamber_election_sub_leg, all_of(sled_names)) %>%
  distinct() %>%
  pivot_wider(
    id_cols     = c(country_state_code, year,chamber_sub_leg),
    names_from  = chamber_election_sub_leg,
    values_from = c(all_of(sled_names),-chamber_sub_leg),
    names_glue  = "{.value}_{chamber_election_sub_leg}",
    #values_fn   = ~ dplyr::last(., na_rm = TRUE),   # collapse duplicates
    values_fill = NA                                 # fill absent cells with NA
  ) %>%
  arrange(country_state_code, year)%>%
  # carry-forward within each chamber separately
  group_by(country_state_code) %>%
  complete(year = seq(min(year, na.rm = TRUE), max(year, na.rm = TRUE), 1)) %>%
  arrange(country_state_code, year) %>%
  mutate(across(
    starts_with("concurrent_election_with_nat_sub_leg_"),
    ~ replace(., is.na(.), 0)
  )) %>%
  fill(all_of(cols_to_fill), .direction = "down") %>%
  ungroup()


data <- left_join(NED,SED,c("country_name","country_code","year")) %>% 
  left_join(.,SEED,c("country_state_code","year"))  %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>% 
  left_join(.,SLED_wide,c("country_state_code","year"))  %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>% 
  left_join(.,SDI,c("country_state_code","year"))  %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))



geom <- st_read(get_data("geom_simple_maps.geojson"))

party_colors     <- read.xlsx(get_data("party_colors.xlsx"))
party_colors_leg <- read.xlsx(get_data("party_colors_leg.xlsx"))


country_bboxes <- list(
  ARGENTINA = list(lng1 = -73.5, lat1 = -59, lng2 = -56, lat2 = -21.8),
  BRAZIL    = list(lng1 = -73.9, lat1 = -33.7, lng2 = -44.5, lat2 = 5.3),
  MEXICO    = list(lng1 = -118.5, lat1 = 12.5, lng2 = -84.7, lat2 = 34.7)
  # ,
  # `Select a country`  = list(lng1 = -118.5, lat1 = -55.1, lng2 = -34.8, lat2 = 32.7)
)