## HACG Impacts 2025 -- Demographic Data ###

rm(list = ls()) ## clear environments


# SET UP  -------------------------------------------------------
options(scipen = 999)  # no scientific notation

# Load Libraries
{
  library(tidyverse)
  library(openxlsx)
  library(readxl)
  library(tidycensus)
}

# UPDATE: WORKING DIRECTORY  ----------------------------------------------------------------
## EG WORK COMP
# setwd("C:/Users/egoldstein/OneDrive - Econsult Solutions Inc/HACG EIS - Primary/Analysis/Data")

## eg mac
setwd("~/Library/CloudStorage/OneDrive-EconsultSolutionsInc/HACG EIS - Primary/Analysis/Data")


# load census data --------------------------------------------------------
# Load all census data
acs_vars23 <- load_variables(2023, "acs5", cache = TRUE)

# SET GEOMETRY
# set state
state <- "NC"


## Median Gross Rent
years <- 2019:2023
variable <- "B25064_001"  # Median Gross Rent

# Function to pull data for one year
get_rent_data <- function(year) {
  get_acs(geography = "place",
          variables = variable,
          state = state,
          year = year,
          survey = "acs5",
          geometry = FALSE) %>%
    filter(GEOID == "3726880") %>%
    mutate(year = year) %>%
    select(year, estimate, moe)
}


rent_list <- list()

# Loop over years
for (yr in years) {
  tmp <- get_rent_data(yr)
  rent_list[[as.character(yr)]] <- tmp
}

# Combine results
rent_timeseries <- bind_rows(rent_list)

## Housing Units by Rental Rates
table <- "B25068"  # Gross Rent (Rent Ranges)

# Function to pull data for one year
get_rent_range_data <- function(year) {
  get_acs(geography = "place",
          table = table,
          state = state,
          year = year,
          survey = "acs5",
          geometry = FALSE) %>%
    filter(GEOID == "3726880") %>%
    mutate(year = year)
}

# Pull all years
rent_range_timeseries <- map_df(years, get_rent_range_data)

# Filter just B25068 variables
rent_vars <- acs_vars23 %>%
  filter(str_detect(name, "B25068_")) %>%
  select(name, label)

# Now join this to your data
rent_range_timeseries_clean <- rent_range_timeseries %>%
  left_join(rent_vars, by = c("variable" = "name")) %>%
  select(year, variable, label, estimate) %>%
  mutate(rent_category = str_remove(label, "Estimate!!")) %>% 
  select(-label)
#%>%
  # filter(rent_category != "Total:")  # Exclude Total row 


## MEDIAN GROSS RENT BY BEDROOM ##
state <- "NC"
years <- 2019:2023
table <- "B25031"  # Median Gross Rent by Bedrooms

# Function to pull one year
get_bedroom_rent_data <- function(year) {
  get_acs(geography = "place",
          table = table,
          state = state,
          year = year,
          survey = "acs5",
          geometry = FALSE) %>%
    filter(GEOID == "3726880") %>%
    mutate(year = year)
}

# Pull all years
bedroom_rent_timeseries <- map_df(years, get_bedroom_rent_data)

# Get labels for B25031
bedroom_vars <- acs_vars23 %>%
  filter(str_detect(name, "B25031_")) %>%
  select(name, label)

# Join labels to your data
bedroom_rent_clean <- bedroom_rent_timeseries %>%
  left_join(bedroom_vars, by = c("variable" = "name")) %>%
  select(year, variable, label, estimate, moe) %>%
  mutate(bedroom_category = str_remove(label, "Estimate!!Median gross rent --!!")) %>% 
  mutate(bedroom_category = str_remove(bedroom_category, "Total:!!")) %>% 
  select(-label)
  

# MEDIAN VALUE BY OWNER OCCUP ---------------------------------------------

# Parameters
state <- "NC"
years <- 2019:2023
table <- "B25075"  # Value (Owner-Occupied Units)

# Function to pull one year
get_home_value_dist_data <- function(year) {
  get_acs(geography = "place",
          table = table,
          state = state,
          year = year,
          survey = "acs5",
          geometry = FALSE) %>%
    filter(GEOID == "3726880") %>%
    mutate(year = year)
}

# Pull all years
home_value_dist_timeseries <- map_df(years, get_home_value_dist_data)

# Get labels for B25075
home_value_vars <- acs_vars23 %>%
  filter(str_detect(name, "B25075_")) %>%
  select(name, label)

# Join labels to your data
home_value_dist_clean <- home_value_dist_timeseries %>%
  left_join(home_value_vars, by = c("variable" = "name")) %>%
  select(year, variable, label, estimate, moe) %>%
  mutate(value_category = str_remove(label, "Estimate!!")) %>% 
  mutate(value_category = str_remove(value_category, "Total:!!")) %>% 
  select(-c(label,moe))


# HOUSING COST BURDEN -----------------------------------------------------

# Parameters
state <- "NC"
years <- 2019:2023
table_owners <- "B25091"  # Owner-occupied housing cost as % of income

# Function to pull one year
get_owner_cost_burden <- function(year) {
  get_acs(geography = "place",
          table = table_owners,
          state = state,
          year = year,
          survey = "acs5",
          geometry = FALSE) %>%
    filter(GEOID == "3726880") %>%
    mutate(year = year)
}

# Pull all years
owner_cost_timeseries <- map_df(years, get_owner_cost_burden)

# Get labels for B25091
owner_cost_vars <- acs_vars23 %>%
  filter(str_detect(name, "B25091_")) %>%
  select(name, label)

# Join labels
owner_cost_clean <- owner_cost_timeseries %>%
  left_join(owner_cost_vars, by = c("variable" = "name")) %>%
  select(year, variable, label, estimate) %>%
  mutate(cost_category_full = str_remove(label, "Estimate!!Total:!!")) %>%
  separate(cost_category_full, into = c("group", "cost_category"), sep = "!!", fill = "right") %>%
  mutate(group = str_trim(group),
         cost_category = str_trim(cost_category)) %>%
  select(year, group, cost_category, estimate)

owner_cost_grouped <- owner_cost_clean %>%
  filter(!is.na(cost_category),  # Drop NA rows (these come from "Housing units with mortgage" header row)
         !str_detect(cost_category, "Not computed")) %>%  # Drop "Not computed" row
  mutate(burden_group = case_when(
    str_detect(cost_category, "Less than 10") ~ "<30%",
    str_detect(cost_category, "10.0 to 14.9") ~ "<30%",
    str_detect(cost_category, "15.0 to 19.9") ~ "<30%",
    str_detect(cost_category, "20.0 to 24.9") ~ "<30%",
    str_detect(cost_category, "25.0 to 29.9") ~ "<30%",
    str_detect(cost_category, "30.0 to 34.9") ~ "30% or more",
    str_detect(cost_category, "35.0 to 39.9") ~ "30% or more",
    str_detect(cost_category, "40.0 to 49.9") ~ "30% or more",
    str_detect(cost_category, "50.0 percent or more") ~ "30% or more",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(burden_group)) %>%
  group_by(year, group, burden_group) %>%
  summarise(units = sum(estimate), .groups = "drop") %>%
  arrange(year, group, burden_group)

## RENTERS COST BURDEN ##

table_renters <- "B25070"

# Function to pull one year
get_renter_cost_burden <- function(year) {
  get_acs(geography = "place",
          table = table_renters,
          state = state,
          year = year,
          survey = "acs5",
          geometry = FALSE) %>%
    filter(GEOID == "3726880") %>%
    mutate(year = year)
}

# Pull all years
renter_cost_timeseries <- map_df(years, get_renter_cost_burden)

# Get labels for B25070
renter_cost_vars <- acs_vars23 %>%
  filter(str_detect(name, "B25070_")) %>%
  select(name, label)

# Process renter data
renter_cost_clean <- renter_cost_timeseries %>%
  left_join(renter_cost_vars, by = c("variable" = "name")) %>%
  select(year, variable, label, estimate) %>%
  mutate(cost_category_full = str_remove(label, "Estimate!!Total:!!")) %>%
  separate(cost_category_full, into = c("group", "cost_category"), sep = "!!", fill = "right") %>%
  mutate(group = str_trim(group),
         cost_category = str_trim(cost_category)) %>%
  select(year, group, cost_category, estimate)

# Group into <30% and ≥30%
renter_cost_grouped <- renter_cost_clean %>%
  mutate(burden_group = case_when(
    str_detect(group, "Less than 10") ~ "<30%",
    str_detect(group, "10.0 to 14.9") ~ "<30%",
    str_detect(group, "15.0 to 19.9") ~ "<30%",
    str_detect(group, "20.0 to 24.9") ~ "<30%",
    str_detect(group, "25.0 to 29.9") ~ "<30%",
    str_detect(group, "30.0 to 34.9") ~ "30% or more",
    str_detect(group, "35.0 to 39.9") ~ "30% or more",
    str_detect(group, "40.0 to 49.9") ~ "30% or more",
    str_detect(group, "50.0 percent or more") ~ "30% or more",
    str_detect(group, "Not computed") ~ "Not computed",
    TRUE ~ NA_character_
  )) %>%
  group_by(year, cost_category, burden_group) %>%
  summarise(units = sum(estimate), .groups = "drop") %>%
  arrange(year,cost_category, burden_group)

