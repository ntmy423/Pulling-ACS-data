library(tidycensus)
library(dplyr)
library(tidyverse)
library(tigris)
library(janitor)
library(mapview)
library(sf)
library(openxlsx)

#install.packages("openxlsx")

setwd("O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R")

#load census data ----------------------------------------------------
# load census data api 
your_key <- "1a9a4573ba63a9ec3f543b0aec1920ed1d9483eb"
census_api_key(your_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# Load all census data
census.var.23 <- load_variables(2023, "acs5", cache = TRUE)
census.var.14 <- load_variables(2014, "acs5", cache = TRUE)
# census.var.15 <- load_variables(2015, "acs5", cache = TRUE)
# SET GEOMETRY
# set state
state <- 24
county <- "Prince George"
geography <- "tract" 

# EXTRACT CENSUS by COUNTY#
census_shp_2022 <-
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2023,
    survey="acs5",
    variables = "B01001_001E",
    geometry = TRUE,
    output="wide"
  ) %>% select(-B01001_001E, -B01001_001M)

census_shp_2014 <-
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2014,
    survey="acs5",
    variables = "B01001_001E",
    geometry = TRUE,
    output="wide"
  ) %>% select(-B01001_001E, -B01001_001M)
# if this is successfully pulled, move on; otherwise one needs to adjust settings above

# Get census data -------------------------------------------------------
## Population & disability -------------------------------------------------------
# Create datasets for export

var_population <- c("B01001_001", "B01001_002", "B01001_026", 
                    "B01001_003", "B01001_004", "B01001_005", 
                    "B01001_006", "B01001_027", "B01001_028", 
                    "B01001_029", "B01001_030", "B01001_020", 
                    "B01001_021", "B01001_022", "B01001_023", 
                    "B01001_024", "B01001_025", "B01001_044", 
                    "B01001_045", "B01001_046", "B01001_047", 
                    "B01001_048", "B01001_049", "B14006_002", 
                    "B19001_002", "B19001_003", "B19001_004", 
                    "B19001_005", "B19001_006", "B19001_007", 
                    "B02001_002", "B02001_003", "B02001_004", 
                    "B02001_005", "B02001_006", "B02001_007", 
                    "B02001_008", "B03001_003", "B03001_002", 
                    "B03002_003", "B06011_001", "B01001_007", 
                    "B01001_031", "B01001_008", "B01001_032", 
                    "B01001_009", "B01001_010", "B01001_011", 
                    "B01001_012", "B01001_033", "B01001_013", 
                    "B01001_034", "B01001_014", "B01001_035", 
                    "B01001_015", "B01001_036", "B01001_016", 
                    "B01001_037", "B01001_017", "B01001_038", 
                    "B01001_018", "B01001_039", "B01001_019", 
                    "B01001_040", "B01001_041", "B01001_042", 
                    "B01001_043")

var_disability <-c("B18101_001" , "B18101_004" , "B18101_007" , 
                   "B18101_010" , "B18101_013" , "B18101_016" , 
                   "B18101_019" , "B18101_023" , "B18101_026" ,
                   "B18101_029" , "B18101_032" , "B18101_035" , 
                   "B18101_038")
population.22 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2023,
    survey="acs5",
    variables =c(var_population, var_disability),
    geometry = TRUE,
    output="wide"
  ) %>%
  select(-ends_with("M")) %>%
  mutate(`Age <18 yr old`=B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_027E+B01001_028E+B01001_029E+B01001_030E,
         `Age 5 to 17 yr old`=B01001_004E+B01001_005E+B01001_006E+B01001_028E+B01001_029E+B01001_030E,
         `Age >65 yr old`=B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E,
         `HHInc less than $35k`= B19001_002E+B19001_003E+B19001_004E+B19001_005E+B19001_006E+B19001_007E,
         `Disability`= B18101_004E+B18101_007E+B18101_010E+B18101_013E+B18101_016E+B18101_019E+B18101_023E+B18101_026E+
           B18101_029E+B18101_032E+B18101_035E+B18101_038E,
         `Age under 5` =B01001_003E+B01001_027E,
         `Age 5 to 9`  =B01001_004E+B01001_028E,
         `Age 10 to 14`=B01001_005E+B01001_029E,
         `Age 15 to 17`=B01001_006E+B01001_030E,
         `Age 18 to 19`=B01001_007E+B01001_031E,
         `Age 20`      =B01001_008E+B01001_032E,
         `Age 21`      =B01001_009E+B01001_033E,
         `Age 22 to 24`=B01001_010E+B01001_034E,
         `Age 25 to 29`=B01001_011E+B01001_035E,
         `Age 30 to 34`=B01001_012E+B01001_036E,
         `Age 35 to 39`=B01001_013E+B01001_037E,
         `Age 40 to 44`=B01001_014E+B01001_038E,
         `Age 45 to 49`=B01001_015E+B01001_039E,
         `Age 50 to 54`=B01001_016E+B01001_040E,
         `Age 55 to 59`=B01001_017E+B01001_041E,
         `Age 60 to 61`=B01001_018E+B01001_042E,
         `Age 62 to 64`=B01001_019E+B01001_043E,
         `Age 65 to 66`=B01001_020E+B01001_044E,
         `Age 67 to 69`=B01001_021E+B01001_045E,
         `Age 70 to 74`=B01001_022E+B01001_046E,
         `Age 75 to 79`=B01001_023E+B01001_047E,
         `Age 80 to 84`=B01001_024E+B01001_048E,
         `Above 85`    =B01001_025E+B01001_049E
  ) %>% 
  rename (
    `Total Population`="B01001_001E",
    `Male`="B01001_002E",
    `Female`="B01001_026E",
    `Population	Below Poverty Level`="B14006_002E",
    `White`="B02001_002E",
    `Black`="B02001_003E",
    `American Indian and Alaska Native`="B02001_004E",
    `Asian`="B02001_005E",
    `Native Hawaiian and Other Pacific Islander`="B02001_006E",
    `Some other race`="B02001_007E",
    `Two or more races`="B02001_008E",
    `Hispanic`="B03001_003E",
    `Non-Hispanic`="B03001_002E",
    `White, Non-Hispanic`="B03002_003E",
    `Disability Universe`="B18101_001E" ,
    `Median Income in the Past 12 Months`="B06011_001E"
  ) %>% 
  mutate(
    `Ethnic Minority`=Hispanic,
    `Racial Minority`=`Total Population`-`White`,
    `Ethnic Racial Minority`=`Total Population`-`White, Non-Hispanic`
  )  %>% 
  select(
    `GEOID`, `NAME`,
    `Total Population`,
    `Male`,
    `Female`,
    `White`,
    `Black`,
    `Hispanic`,
    `Asian`,
    `American Indian and Alaska Native`,
    `Native Hawaiian and Other Pacific Islander`,
    `Some other race`,
    `Two or more races`,
    `White, Non-Hispanic`,
    `Non-Hispanic`,
    `Racial Minority`,
    `Ethnic Minority`,
    `Ethnic Racial Minority`,
    `Disability Universe`,
    `Age <18 yr old`,
    `Age 5 to 17 yr old`,
    `Age >65 yr old`,
    `Population	Below Poverty Level`,
    `Median Income in the Past 12 Months`,
    `Age under 5`,
    `Age 5 to 9`,
    `Age 10 to 14`,
    `Age 15 to 17`,
    `Age 18 to 19`,
    `Age 20`,
    `Age 21`,
    `Age 22 to 24`,
    `Age 25 to 29`,
    `Age 30 to 34`,
    `Age 35 to 39`,
    `Age 40 to 44`,
    `Age 45 to 49`,
    `Age 50 to 54`,
    `Age 55 to 59`,
    `Age 60 to 61`,
    `Age 62 to 64`,
    `Age 65 to 66`,
    `Age 67 to 69`,
    `Age 70 to 74`,
    `Age 75 to 79`,
    `Age 80 to 84`,
    `Above 85`
  )

population.22$area <- st_area(population.22$geometry)
population.22$area <- as.numeric(population.22$area) * 3.861e-7
population.22$density <- population.22$`Total Population`/ population.22$area
population.22 <- population.22 %>%
  select(-area, -geometry)

## Historical pop

population.14 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2014,
    survey="acs5",
    variables =c(var_population, var_disability),
   geometry = TRUE,
    output="wide"
  ) %>%
  select(-ends_with("M")) %>%
  mutate(`Age <18 yr old`=B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_027E+B01001_028E+B01001_029E+B01001_030E,
         `Age 5 to 17 yr old`=B01001_004E+B01001_005E+B01001_006E+B01001_028E+B01001_029E+B01001_030E,
         `Age >65 yr old`=B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E,
         `HHInc less than $35k`= B19001_002E+B19001_003E+B19001_004E+B19001_005E+B19001_006E+B19001_007E,
         `Disability`= B18101_004E+B18101_007E+B18101_010E+B18101_013E+B18101_016E+B18101_019E+B18101_023E+B18101_026E+
           B18101_029E+B18101_032E+B18101_035E+B18101_038E,       
         `Age under 5`=B01001_003E+B01001_027E,
         `Age 5 to 9`=B01001_004E + B01001_028E,
         `Age 10 to 14`=B01001_005E+B01001_029E,
         `Age 15 to 17`=B01001_006E+B01001_030E,
         `Age 18 to 19`=B01001_007E+B01001_031E,
         `Age 20`      =B01001_008E+B01001_032E,
         `Age 21`      =B01001_009E+B01001_033E,
         `Age 22 to 24`=B01001_010E+B01001_034E,
         `Age 25 to 29`=B01001_011E+B01001_035E,
         `Age 30 to 34`=B01001_012E+B01001_036E,
         `Age 35 to 39`=B01001_013E+B01001_037E,
         `Age 40 to 44`=B01001_014E+B01001_038E,
         `Age 45 to 49`=B01001_015E+B01001_039E,
         `Age 50 to 54`=B01001_016E+B01001_040E,
         `Age 55 to 59`=B01001_017E+B01001_041E,
         `Age 60 to 61`=B01001_018E+B01001_042E,
         `Age 62 to 64`=B01001_019E+B01001_043E,
         `Age 65 to 66`=B01001_020E+B01001_044E,
         `Age 67 to 69`=B01001_021E+B01001_045E,
         `Age 70 to 74`=B01001_022E+B01001_046E,
         `Age 75 to 79`=B01001_023E+B01001_047E,
         `Age 80 to 84`=B01001_024E+B01001_048E,
         `Above 85`    =B01001_025E+B01001_049E
  ) %>% 
  rename (
    `Total Population`="B01001_001E",
    `Male`="B01001_002E",
    `Female`="B01001_026E",
    `Population	Below Poverty Level`="B14006_002E",
    `White`="B02001_002E",
    `Black`="B02001_003E",
    `American Indian and Alaska Native`="B02001_004E",
    `Asian`="B02001_005E",
    `Native Hawaiian and Other Pacific Islander`="B02001_006E",
    `Some other race`="B02001_007E",
    `Two or more races`="B02001_008E",
    `Hispanic`="B03001_003E",
    `Non-Hispanic`="B03001_002E",
    `White, Non-Hispanic`="B03002_003E",
    `Disability Universe`="B18101_001E" ,
    `Median Income in the Past 12 Months`="B06011_001E"
  ) %>% 
  mutate(
    `Ethnic Minority`=Hispanic,
    `Racial Minority`=`Total Population`-`White`,
    `Ethnic Racial Minority`=`Total Population`-`White, Non-Hispanic`
  )  %>% 
  select(
    `GEOID`, `NAME`,
    `Total Population`,
    `Male`,
    `Female`,
    `White`,
    `Black`,
    `Hispanic`,
    `Asian`,
    `American Indian and Alaska Native`,
    `Native Hawaiian and Other Pacific Islander`,
    `Some other race`,
    `Two or more races`,
    `White, Non-Hispanic`,
    `Non-Hispanic`,
    `Racial Minority`,
    `Ethnic Minority`,
    `Ethnic Racial Minority`,
    `Disability Universe`,
    `Age <18 yr old`,
    `Age 5 to 17 yr old`,
    `Age >65 yr old`,
    `Population	Below Poverty Level`,
    `Median Income in the Past 12 Months`,
    `Age under 5`,
    `Age 5 to 9`,
    `Age 10 to 14`,
    `Age 15 to 17`,
    `Age 18 to 19`,
    `Age 20`,
    `Age 21`,
    `Age 22 to 24`,
    `Age 25 to 29`,
    `Age 30 to 34`,
    `Age 35 to 39`,
    `Age 40 to 44`,
    `Age 45 to 49`,
    `Age 50 to 54`,
    `Age 55 to 59`,
    `Age 60 to 61`,
    `Age 62 to 64`,
    `Age 65 to 66`,
    `Age 67 to 69`,
    `Age 70 to 74`,
    `Age 75 to 79`,
    `Age 80 to 84`,
    `Above 85`
  )

#pax per sq mile 

population.14$area <- st_area(population.14$geometry)
population.14$area <- as.numeric(population.14$area) * 3.861e-7
population.14$density <- population.14$`Total Population`/ population.14$area
population.14 <- population.14 %>%
  select(-area, -geometry)

#test <- left_join(population_hist, population, by = "GEOID")


## Income & poverty -------------------------------------------------------

var_income <- c("B19001_001", "B19001_002","B19001_003",
                "B19001_004","B19001_005","B19001_006",
                "B19001_007","B19001_008","B19001_009",
                "B19001_010", "B19001_011","B19001_012",
                "B19001_013","B19001_014","B19001_015",
                "B19001_016","B19001_017","B19013_001")
var_pov <- c("B17026_001", "B17026_002", "B17026_003", 
             "B17026_004", "B17026_005", "B17026_006", 
             "B17026_007", "B17026_008", "B17026_009")

income.22 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2023,
    survey="acs5",
    variables =c(var_income, var_pov),
#  geometry = TRUE,
    output="wide"
  ) %>%
  select(-ends_with("M")) %>%  
  mutate(
    `HHInc $15-25k`= B19001_004E+B19001_005E,
    `HHInc $25-35k`= B19001_006E+B19001_007E,
    `HHInc $35-50k`= B19001_008E+B19001_009E,
    `HHInc $50-75k`= B19001_010E+B19001_011E,
    `HHInc $75-100k`= B19001_012E+B19001_013E,
    `HHInc $100-150k`= B19001_014E+B19001_015E,
    `HHInc <$40k`=B19001_002E+B19001_003E+B19001_004E+B19001_005E+B19001_006E+B19001_007E+B19001_008E,
    `Under 200% of Poverty Level`= B17026_002E+B17026_003E+B17026_004E+B17026_005E+B17026_006E+B17026_007E+B17026_008E+B17026_009E,
    `Under_200`= B17026_002E+B17026_003E+B17026_004E+B17026_005E+B17026_006E+B17026_007E+B17026_008E+B17026_009E
  ) %>% 
  rename(
    `All Households` = "B19001_001E",
    `HHInc <$10k`="B19001_002E",
    `HHInc $10-15k`="B19001_003E",
    `HHInc $150-200k`="B19001_016E",
    `HHInc >$200k`="B19001_017E",
    `Median HHInc`="B19013_001E",
    `Total Families (Poverty Level Universe)` = "B17026_001E"
  ) %>% 
  select(-starts_with("B"),-Under_200)

# 2014 income poverty

income.14 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2014,
    survey="acs5",
    variables =c(var_income, var_pov),
#   geometry = TRUE,
    output="wide"
  ) %>%
  select(-ends_with("M")) %>%  
  mutate(
    `HHInc $15-25k`= B19001_004E+B19001_005E,
    `HHInc $25-35k`= B19001_006E+B19001_007E,
    `HHInc $35-50k`= B19001_008E+B19001_009E,
    `HHInc $50-75k`= B19001_010E+B19001_011E,
    `HHInc $75-100k`= B19001_012E+B19001_013E,
    `HHInc $100-150k`= B19001_014E+B19001_015E,
    `HHInc <$40k`=B19001_002E+B19001_003E+B19001_004E+B19001_005E+B19001_006E+B19001_007E+B19001_008E,
    `Under 200% of Poverty Level`= B17026_002E+B17026_003E+B17026_004E+B17026_005E+B17026_006E+B17026_007E+B17026_008E+B17026_009E,
    `Under_200`= B17026_002E+B17026_003E+B17026_004E+B17026_005E+B17026_006E+B17026_007E+B17026_008E+B17026_009E
  ) %>% 
  rename(
    `All Households` = "B19001_001E",
    `HHInc <$10k`="B19001_002E",
    `HHInc $10-15k`="B19001_003E",
    `HHInc $150-200k`="B19001_016E",
    `HHInc >$200k`="B19001_017E",
    `Median Household Income`="B19013_001E",
    `Total Families (Poverty Level Universe)` = "B17026_001E"
  ) %>% 
  select(-starts_with("B"), -Under_200)



## Education, Age, English -------------------------------------------------------
var_edu <- c(
  "B15003_001",  # Total educational attainment universe
  "B15003_002",  # No schooling completed
  "B15003_003",  # Nursery school
  "B15003_004",  # Kindergarten
  "B15003_005",  # 1st grade
  "B15003_006",  # 2nd grade
  "B15003_007",  # 3rd grade
  "B15003_008",  # 4th grade
  "B15003_009",  # 5th grade
  "B15003_010",  # 6th grade
  "B15003_011",  # 7th grade
  "B15003_012",  # 8th grade
  "B15003_013",  # 9th grade
  "B15003_014",  # 10th grade
  "B15003_015",  # 11th grade
  "B15003_016",  # 12th grade, no diploma
  "B15003_017",  # High school graduate (includes equivalency)
  "B15003_018",  # GED or alternative credential
  "B15003_019",  # Some college, less than 1 year
  "B15003_020",  # Some college, 1 or more years, no degree
  "B15003_021",  # Associate's degree
  "B15003_022",  # Bachelor's degree
  "B15003_023",  # Master's degree
  "B15003_024",  # Professional school degree
  "B15003_025"   # Doctorate degree
)


education_22 <- get_acs(
  geography = geography,     
  state = state,            
  county = county,           
  year = 2023,                
  survey = "acs5",            
  variables = var_edu,        
  output = "wide"             
) %>%
  mutate(
    `High School and Some College` = `B15003_017E` + `B15003_018E` + `B15003_019E` + `B15003_020E`,
    `Below High School` = `B15003_002E` + `B15003_003E` + `B15003_004E` +
      `B15003_005E` + `B15003_006E` + `B15003_007E` +
      `B15003_008E` + `B15003_009E` + `B15003_010E` +
      `B15003_011E` + `B15003_012E` + `B15003_013E` +
      `B15003_014E` + `B15003_015E` + `B15003_016E`,
    `Graduate or Professional Degree` = `B15003_023E` + `B15003_024E` + `B15003_025E`,
  ) %>%
  # Rename columns for better readability
  rename(
    `Edu Universe` = "B15003_001E",
    `Bachelor's Degree` = "B15003_022E",
    `Associate's Degree` = "B15003_021E",
  ) %>%
  # Select only relevant columns
  select(
    GEOID,
    `Edu Universe`,
    `Below High School`,
    `High School and Some College`,
    `Associate's Degree`,
    `Bachelor's Degree`,
    `Graduate or Professional Degree`
  )

## 2014 education

education_14 <- get_acs(
  geography = geography,     
  state = state,            
  county = county,           
  year = 2014,                
  survey = "acs5",            
  variables = var_edu,        
  output = "wide"             
) %>%
  mutate(
    `High School and Some College` = `B15003_017E` + `B15003_018E` + `B15003_019E` + `B15003_020E`,
    `Below High School` = `B15003_002E` + `B15003_003E` + `B15003_004E` +
      `B15003_005E` + `B15003_006E` + `B15003_007E` +
      `B15003_008E` + `B15003_009E` + `B15003_010E` +
      `B15003_011E` + `B15003_012E` + `B15003_013E` +
      `B15003_014E` + `B15003_015E` + `B15003_016E`,
    `Graduate or Professional Degree` = `B15003_023E` + `B15003_024E` + `B15003_025E`,
  ) %>%
  # Rename columns for better readability
  rename(
    `Edu Universe` = "B15003_001E",
    `Bachelor's Degree` = "B15003_022E",
    `Associate's Degree` = "B15003_021E",
  ) %>%
  # Select only relevant columns
  select(
    GEOID,
    `Edu Universe`,
    `Below High School`,
    `High School and Some College`,
    `Associate's Degree`,
    `Bachelor's Degree`,
    `Graduate or Professional Degree`
  )

## Age -------------------------------------------------------

var_age <- c(
  "B01001_001", # Total population
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",  # Male under 18
  "B01001_007", "B01001_008", "B01001_009",                # Male 18-34
  "B01001_010", "B01001_011", "B01001_012",                # Male 35-64
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",  # Male 65+
  "B01001_027", "B01001_028", "B01001_029", "B01001_030",  # Female under 18
  "B01001_031", "B01001_032", "B01001_033",                # Female 18-34
  "B01001_034", "B01001_035", "B01001_036",                # Female 35-64
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"   # Female 65+
)

# Retrieve ACS data for the defined variables
age_22 <- get_acs(
  geography = geography,   
  state = state,         
  county = county,        
  year = 2023,            
  survey = "acs5",       
  variables = var_age,     
  output = "wide"          
) %>%
  mutate(
    `Under 18` = B01001_003E + B01001_004E + B01001_005E + B01001_006E + B01001_027E + B01001_028E + B01001_029E + B01001_030E,
    `18-34` = B01001_007E + B01001_008E + B01001_009E + B01001_031E + B01001_032E + B01001_033E,
    `35-64` = B01001_010E + B01001_011E + B01001_012E + B01001_034E + B01001_035E + B01001_036E,
    `Over 65` = B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
      B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E
  ) %>%
  rename(
    `Age Universe` = "B01001_001E"  # Total population
  ) %>%
  select(
    GEOID,
    NAME,
    `Age Universe`,
    `Under 18`,
    `18-34`,
    `35-64`,
    `Over 65`
  )
## 2014 age 

age.14 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2014,
    survey="acs5",
    variables =c(var_age),
  #  geometry = TRUE,
    output="wide"
  ) %>%
  mutate(
    `Under 18` = B01001_003E + B01001_004E + B01001_005E + B01001_006E + B01001_027E + B01001_028E + B01001_029E + B01001_030E,
    `18-34` = B01001_007E + B01001_008E + B01001_009E + B01001_031E + B01001_032E + B01001_033E,
    `35-64` = B01001_010E + B01001_011E + B01001_012E + B01001_034E + B01001_035E + B01001_036E,
    `Over 65` = B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
      B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E
  ) %>%
  rename(
    `Age Universe` = "B01001_001E"  # Total population
  ) %>%
  select(
    GEOID,
    NAME,
    `Age Universe`,
    `Under 18`,
    `18-34`,
    `35-64`,
    `Over 65`
  )

## Housing  -------------------------------------------------------

var_housing <- c("B25002_002","B25002_003",
                 "B25003_001", "B25003_002", "B25003_003", "B11016_002", 
                 "B11016_009", "B08201_001", "B08201_002", "B08202_002",
                 "B09001_002", "B19019_001","B25024_001", "B25024_002", 
                 "B25024_003", "B25024_004", "B25024_005", "B25024_006", 
                 "B25024_007", "B25024_008","B11009_002", "B11012_008", 
                 "B11012_013", "B25024_009", "B11016_003","B11016_004", 
                 "B11016_005", "B11016_006", "B11016_007", "B11016_008",
                 "B11016_010", "B11016_011","B11016_012","B11016_013",
                 "B11016_014","B11016_015","B11016_016", "B11008_002", 
                 "B25012_004","B25012_012", "B25012_008", "B25012_016", "B11009_001",
                 "B09002_005","B09002_006","B09002_007","B09002_012","B09002_013","B09002_014",
                 "B09002_018","B09002_019","B09002_020","B11005_004","B11005_013",
                 'B11010_003','B11010_010','B11010_005','B11010_012',"B25010_001")

housing.22 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2023,
    survey="acs5",
    variables = var_housing,
    #    geometry = TRUE,
    output="wide"
  ) %>%
  mutate(`Single Family Home`=B25024_002E+B25024_003E,
         `Two to Four Units`=B25024_004E+B25024_005E,
         `Five or More Units`=B25024_006E+B25024_007E+B25024_008E+B25024_009E,
         `school_age_children(5-17)` =  B09002_005E+ B09002_006E + B09002_007E+
                                        B09002_012E+ B09002_013E + B09002_014E+
                                        B09002_018E+ B09002_019E + B09002_020E,
         #live alone, >65 yrs
         `live_alone_nf_HH` = B11010_003E+B11010_010E,
         `live_alone_>65_HH`= B11010_005E+B11010_012E
         # `HH with own children of HHer <18` = B25012_004E+B25012_012E, #owner + renter
         # `HH with no children of HHer <18` = B25012_008E+B25012_016E
  ) %>%
  rename(
    `Housing Type Universe` = "B25024_001E",
    `Total Housing Units` = "B25003_001E",
    `Avg Persons per HH` = "B25010_001E",
    `Occupied HU` = "B25002_002E",
    `Vacant HU` = "B25002_003E",
    `OwnerOcc` = "B25003_002E",
    `RenterOcc` = "B25003_003E",
    `HHs with a vehicle` = "B08201_001E",
    `HHs without a vehicle` = "B08201_002E",
    `HHs with no workers` = "B08202_002E",
    # `HHs with kids` = "B09001_002E",
    `Median HHIncome` = "B19019_001E",
    `Family Households` = "B11016_002E",
    `Non-Family Households` = "B11016_009E", #there is data by number of occupants in fam/non fam homes
    `HH_Fam_2P` = "B11016_003E",  # 2 person HH - Family
    `HH_Fam_3P` = "B11016_004E",  # 3 person HH - Family
    `HH_Fam_4P` = "B11016_005E",  # 4 person HH - Family
    `HH_Fam_5P` = "B11016_006E",  # 5 person HH - Family
    `HH_Fam_6P` = "B11016_007E",  # 6 person HH - Family
    `HH_Fam_7P` = "B11016_008E",  # 7+ person HH - Family
    `HH_NFam_1P` = "B11016_010E", # 1 person HH - Non Family
    `HH_NFam_2P` = "B11016_011E", # 2 person HH - Non Family
    `HH_NFam_3P` = "B11016_012E", # 3 person HH - Non Family
    `HH_NFam_4P` = "B11016_013E", # 4 person HH - Non Family
    `HH_NFam_5P` = "B11016_014E", # 5 person HH - Non Family
    `HH_NFam_6P` = "B11016_015E", # 6 person HH - Non Family
    `HH_NFam_7P` = "B11016_016E",  # 7+ person HH - Non Family
     # school age children hh
    `HH_Married_with_child<18`= "B11005_004E",
    `HH_Married_without_child<18`= "B11005_013E",
    `Cohabiting Couple Households` = "B11008_002E",
    `Married Couple Households` =  "B11009_002E", # only by state?
    `Female Householder - no spouse present` = "B11012_008E",
    `Male Householder - no spouse present` = "B11012_013E",
    )%>%  
  select(-ends_with("M"),-starts_with("B"))

#2014
var_housing.14 <- c("B25002_002","B25002_003","B25003_001", "B25003_002", "B25003_003", "B11016_002", 
                 "B11016_009", "B08201_001", "B08201_002", "B08202_002",
                 "B09001_002", "B19019_001","B25024_001", "B25024_002", 
                 "B25024_003", "B25024_004", "B25024_005", "B25024_006", 
                 "B25024_007", "B25024_008","B11009_002", 
                 "B11012_013", "B25024_009", "B11016_003","B11016_004", 
                 "B11016_005", "B11016_006", "B11016_007", "B11016_008",
                 "B11016_010", "B11016_011","B11016_012","B11016_013",
                 "B11016_014","B11016_015","B11016_016", 
                 "B25012_004","B25012_012", "B25012_008", "B25012_016", "B11009_001",
                 "B11012_010","B11012_007", 
                 "B09002_005","B09002_006","B09002_007","B09002_012","B09002_013","B09002_014",
                 "B09002_018","B09002_019","B09002_020","B11005_004","B11005_013",
                 'B11010_003','B11010_010','B11010_005','B11010_012',"B25010_001")

housing.14 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2014,
    survey="acs5",
    variables = var_housing.14,
    #    geometry = TRUE,
    output="wide"
  ) %>%
  mutate(`Single Family Home`=B25024_002E+B25024_003E,
         `Two to Four Units`=B25024_004E+B25024_005E,
         `Five or More Units`=B25024_006E+B25024_007E+B25024_008E+B25024_009E,
         `school_age_children(5-17)` =  B09002_005E+ B09002_006E + B09002_007E+
           B09002_012E+ B09002_013E + B09002_014E+
           B09002_018E+ B09002_019E + B09002_020E,
         #live alone, >65 yrs
         `live_alone_nf_HH` = B11010_003E+B11010_010E,
         `live_alone_>65_HH`= B11010_005E+B11010_012E
         # `HH with own children of HHer <18` = B25012_004E+B25012_012E, #owner + renter
         # `HH with no children of HHer <18` = B25012_008E+B25012_016E
  ) %>%
  rename(
    `Housing Type Universe` = "B25024_001E",
    `Total Housing Units` = "B25003_001E",
    `Avg Persons per HH` = "B25010_001E",
    `Occupied HU` = "B25002_002E",
    `Vacant HU` = "B25002_003E",
    `OwnerOcc` = "B25003_002E",
    `RenterOcc` = "B25003_003E",
    `Family Households` = "B11016_002E",
    `Non-Family Households` = "B11016_009E", #there is data by number of occupants in fam/non fam homes
    `HHs with a vehicle` = "B08201_001E",
    `HHs without a vehicle` = "B08201_002E",
    `HHs with no workers` = "B08202_002E",
    # `HHs with kids` = "B09001_002E",
    `Median HH income` = "B19019_001E",
    `HH_Fam_2P` = "B11016_003E",  # 2 person HH - Family
    `HH_Fam_3P` = "B11016_004E",  # 3 person HH - Family
    `HH_Fam_4P` = "B11016_005E",  # 4 person HH - Family
    `HH_Fam_5P` = "B11016_006E",  # 5 person HH - Family
    `HH_Fam_6P` = "B11016_007E",  # 6 person HH - Family
    `HH_Fam_7P` = "B11016_008E",  # 7+ person HH - Family
    `HH_NFam_1P` = "B11016_010E", # 1 person HH - Non Family
    `HH_NFam_2P` = "B11016_011E", # 2 person HH - Non Family
    `HH_NFam_3P` = "B11016_012E", # 3 person HH - Non Family
    `HH_NFam_4P` = "B11016_013E", # 4 person HH - Non Family
    `HH_NFam_5P` = "B11016_014E", # 5 person HH - Non Family
    `HH_NFam_6P` = "B11016_015E", # 6 person HH - Non Family
    `HH_NFam_7P` = "B11016_016E" , # 7+ person HH - Non Family
    `HH_Married_with_child<18`= "B11005_004E",
    `HH_Married_without_child<18`= "B11005_013E",
    `Cohabiting Couple Households (Unmarried)` = "B11009_001E",#diff variables across years
    `Married Couple Households` =  "B11009_002E", # only by state?
    `Female Householder - no spouse present` = "B11012_010E",
    `Male Householder - no spouse present` = "B11012_007E",
) %>%
  select(-ends_with("M"),-starts_with("B"))

colnames(housing.14)
colnames(housing.22)
## Transportation & travel -------------------------------------------------------

var_travel <- c("B08006_001","B08006_002","B08006_003","B08006_004","B08006_005","B08006_006","B08006_007","B08006_008","B08006_009",
                "B08006_010","B08006_011","B08006_012","B08006_013","B08006_014","B08006_015","B08006_016","B08006_017",
                "B08122_002","B08122_005","B08122_010","B08122_014","B08122_018","B08122_022","B08122_026",
                "B08105A_002","B08105A_003","B08105A_004","B08105A_005","B08105A_006","B08105A_007",
                "B08105H_002","B08105H_003","B08105H_004","B08105H_005","B08105H_006","B08105H_007","B08301_018") 
#rep <-- var_travel[!var_travel%in%census.var.county20$name==TRUE] %>% print()

travel.22 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2023,
    survey="acs5",
    variables = var_travel,
    #    geometry = TRUE,
    output="wide"
  ) %>% 
  rename(
    `Total_Work` = "B08006_001E",
    `Car_Total` = "B08006_002E",
    `Car_Alone` = "B08006_003E",
    `Carpool_Total` = "B08006_004E",
    `Carpool_2P` = "B08006_005E",
    `Carpool_3P` = "B08006_006E",
    `Carpool_4P` = "B08006_007E",
    `Transit_Total` = "B08006_008E",
    `Transit_Bus` = "B08006_009E",
    `Transit_Subway` = "B08006_010E",
    `Transit_Rail` = "B08006_011E",
    `Transit_LightRail` = "B08006_012E",
    `Transit_Ferry` = "B08006_013E",
    `Bike_Total` = "B08006_014E",
    `Bike_Work` = "B08301_018E",
    `Walk_Total` = "B08006_015E",
    `Other_Modes` = "B08006_016E",
    `Work_Home` = "B08006_017E"
  #   `People Below 100% of Poverty`="B08122_002E",
  #   `Car, truck, van - drove alone (People Below 100% of Poverty)`="B08122_005E",
  #   `Car, truck, van - carpooled (People Below 100% of Poverty)`="B08122_010E",
  #   `Public Transportation (excluding taxicab) (People Below 100% of Poverty)`="B08122_014E",
  #   `Walked (People Below 100% of Poverty)`="B08122_018E",
  #   `Taxicab, motorcycle, bicycle, or other means (People Below 100% of Poverty)`="B08122_022E",
  #   `Work from Home (People Below 100% of Poverty)`="B08122_026E") %>% 
  # mutate(
  #   `Car, truck, van - drove alone (Racial Minority)`=`Car, truck, van - drove alone (Total)`-B08105A_002E,
  #   `Car, truck, van - carpooled (Racial Minority)`=`Car, truck, van - carpooled (Total)`-B08105A_003E,
  #   `Public Transportation (excluding taxicab) (Racial Minority)`=`Public Transportation (excluding taxicab) (Total)`-B08105A_004E,
  #   `Walked (Racial Minority)`=`Walked (Total)`-B08105A_005E,
  #   `Taxicab, motorcycle, bicycle, or other means (Racial Minority)`=`Taxicab, motorcycle, bicycle, or other means (Total)`-B08105A_006E,
  #   `Work from Home (Racial Minority)` = `Work from Home (Total)`-B08105A_007E,
  #   `Car, truck, van - drove alone (Racial Ethnic Minority)`=`Car, truck, van - drove alone (Total)`-B08105H_002E,
  #   `Car, truck, van - carpooled (Racial Ethnic Minority)`=`Car, truck, van - carpooled (Total)`-B08105H_003E,
  #   `Public Transportation (excluding taxicab) (Racial Ethnic Minority)`=`Public Transportation (excluding taxicab) (Total)`-B08105H_004E,
  #   `Walked (Racial Ethnic Minority)`=`Walked (Total)`-B08105H_005E,
  #   `Taxicab, motorcycle, bicycle, or other means (Racial Ethnic Minority)`=`Taxicab, motorcycle, bicycle, or other means (Total)`-B08105H_006E,
  #   `Work from Home (Racial Ethnic Minority)` = `Work from Home (Total)`-B08105H_007E
  ) %>% 
  select(-ends_with("M"),-starts_with("B"))


travel.14 <- 
  get_acs(
    geography = geography, #alt: tract, block group
    state = state,
    county = county,
    year = 2014,
    survey="acs5",
    variables = var_travel,
    #    geometry = TRUE,
    output="wide"
  ) %>% 
  rename(
    `Total_Work` = "B08006_001E",
    `Car_Total` = "B08006_002E",
    `Car_Alone` = "B08006_003E",
    `Carpool_Total` = "B08006_004E",
    `Carpool_2P` = "B08006_005E",
    `Carpool_3P` = "B08006_006E",
    `Carpool_4P` = "B08006_007E",
    `Transit_Total` = "B08006_008E",
    `Transit_Bus` = "B08006_009E",
    `Transit_Subway` = "B08006_010E",
    `Transit_Rail` = "B08006_011E",
    `Transit_LightRail` = "B08006_012E",
    `Transit_Ferry` = "B08006_013E",
    `Bike_Total` = "B08006_014E",
    `Bike_Work` = "B08301_018E",
    `Walk_Total` = "B08006_015E",
    `Other_Modes` = "B08006_016E",
    `Work_Home` = "B08006_017E"
    #   `People Below 100% of Poverty`="B08122_002E",
    #   `Car, truck, van - drove alone (People Below 100% of Poverty)`="B08122_005E",
    #   `Car, truck, van - carpooled (People Below 100% of Poverty)`="B08122_010E",
    #   `Public Transportation (excluding taxicab) (People Below 100% of Poverty)`="B08122_014E",
    #   `Walked (People Below 100% of Poverty)`="B08122_018E",
    #   `Taxicab, motorcycle, bicycle, or other means (People Below 100% of Poverty)`="B08122_022E",
    #   `Work from Home (People Below 100% of Poverty)`="B08122_026E") %>% 
    # mutate(
    #   `Car, truck, van - drove alone (Racial Minority)`=`Car, truck, van - drove alone (Total)`-B08105A_002E,
    #   `Car, truck, van - carpooled (Racial Minority)`=`Car, truck, van - carpooled (Total)`-B08105A_003E,
    #   `Public Transportation (excluding taxicab) (Racial Minority)`=`Public Transportation (excluding taxicab) (Total)`-B08105A_004E,
    #   `Walked (Racial Minority)`=`Walked (Total)`-B08105A_005E,
    #   `Taxicab, motorcycle, bicycle, or other means (Racial Minority)`=`Taxicab, motorcycle, bicycle, or other means (Total)`-B08105A_006E,
    #   `Work from Home (Racial Minority)` = `Work from Home (Total)`-B08105A_007E,
    #   `Car, truck, van - drove alone (Racial Ethnic Minority)`=`Car, truck, van - drove alone (Total)`-B08105H_002E,
    #   `Car, truck, van - carpooled (Racial Ethnic Minority)`=`Car, truck, van - carpooled (Total)`-B08105H_003E,
    #   `Public Transportation (excluding taxicab) (Racial Ethnic Minority)`=`Public Transportation (excluding taxicab) (Total)`-B08105H_004E,
    #   `Walked (Racial Ethnic Minority)`=`Walked (Total)`-B08105H_005E,
    #   `Taxicab, motorcycle, bicycle, or other means (Racial Ethnic Minority)`=`Taxicab, motorcycle, bicycle, or other means (Total)`-B08105H_006E,
    #   `Work from Home (Racial Ethnic Minority)` = `Work from Home (Total)`-B08105H_007E
  ) %>% 
  select(-ends_with("M"),-starts_with("B"))


### Data joins

PGC.data.2014 <- population.14 %>%
  left_join((income.14 %>%
              select(-NAME))
            , by = "GEOID") 
PGC.data.2014 <- PGC.data.2014 %>%
  left_join(education_14, by = "GEOID") 
PGC.data.2014 <- PGC.data.2014 %>%
  left_join((housing.14 %>%
               select(-NAME))
            , by = "GEOID") 
PGC.data.2014 <- PGC.data.2014 %>%
  left_join((age.14 %>%
               select(-NAME))
            , by = "GEOID")
PGC.data.2014 <- PGC.data.2014 %>%
  left_join((travel.14 %>%
               select(-NAME))
            , by = "GEOID") 


PGC.data.2022 <- population.22 %>%
  left_join((income.22 %>%
               select(-NAME))
            , by = "GEOID") 
PGC.data.2022 <- PGC.data.2022 %>%
  left_join(education_22, by = "GEOID") 
PGC.data.2022 <- PGC.data.2022 %>%
  left_join((housing.22 %>%
               select(-NAME))
            , by = "GEOID") 
PGC.data.2022 <- PGC.data.2022 %>%
  left_join((age_22 %>%
               select(-NAME))
            , by = "GEOID") 
PGC.data.2022 <- PGC.data.2022 %>%
  left_join((travel.22 %>%
               select(-NAME))
            , by = "GEOID") 


colnames(PGC.data.2022)

# Clean verison - Select Census data for -------------------------------------------------------

# PGC_tracts <-
#   get_acs(
#     geography = geography, #alt: tract, block group
#     state = state,
#     county = county,
#     year = 2022,
#     survey="acs5",
#     variables =c(var_age),
#     geometry = TRUE,
#     output="wide",
#   ) %>%
#   select(-c("B01001_001M","B01001_001E"))

# PGC.data.2022 <- PGC.data.2022 %>%
#   rename_with(~ gsub("[:]", "_", .)) %>%  # Replace invalid characters (e.g., `:`)
#   rename_with(~ make.unique(.))%>% 
#   rename_with(~ substr(., 1, 10)) 

#names(PGC.data.2022)

# PGC.data.2014 <- PGC.data.2014 %>%
#   rename_with(~ gsub("[:]", "_", .)) %>%  # Replace invalid characters (e.g., `:`)
#   rename_with(~ substr(., 1, 10))  %>% 
#   rename_with(~ make.unique(.)) 

write.csv(st_drop_geometry(PGC.data.2014), "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/PGC_data_2014.csv", row.names = FALSE)
st_write(census_shp_2014, "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/Census_shp/PGC_shp_2014.shp", overwrite = TRUE, append=FALSE)

write.csv(st_drop_geometry(PGC.data.2022), "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/PGC_data_2022.csv", row.names = FALSE)
st_write(census_shp_2022, "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/Census_shp/PGC_shp_2022.shp", overwrite = TRUE, append=FALSE)
mapview(PGC.data.2022)

colnames(PGC.data.2022)




# Selected variable verison - Select Census data for -------------------------------------------------------


PGC.data.2022.select <- PGC.data.2022 %>%
  mutate(
    children_5 = `Age under 5`,
    Junior_6_19 =  `Age 5 to 9` + `Age 10 to 14` + `Age 15 to 17` + `Age 18 to 19`,
    Young_20_24 = `Age 20` + `Age 21` + `Age 22 to 24`,
    Young_Pro_25_34 = `Age 25 to 29` + `Age 30 to 34`,
    Middle_Age_I_35_54 = `Age 35 to 39` + `Age 40 to 44` + `Age 45 to 49` + `Age 50 to 54`,
    Middle_Age_II_55_64 = `Age 55 to 59` + `Age 60 to 61` + `Age 62 to 64`,
    Senior_65 = `Age 65 to 66` + `Age 67 to 69` + `Age 70 to 74` + `Age 75 to 79` + `Age 80 to 84` + `Above 85`,
    Other_race = `Some other race` + `Two or more races`
  ) %>% 
  select(GEOID,`Total Population`, Male, Female,White,
         Black, Asian, `American Indian and Alaska Native`,`Native Hawaiian and Other Pacific Islander`, Other_race, Hispanic,`Non-Hispanic`,
         `Disability Universe`,`Population\tBelow Poverty Level`,`Total Families (Poverty Level Universe)`, `Under 200% of Poverty Level`,
         children_5,Junior_6_19,Young_20_24,Young_Pro_25_34,Middle_Age_I_35_54,Middle_Age_II_55_64, Senior_65,
         `All Households`, `HHInc <$10k`, `HHInc $10-15k`,`HHInc $15-25k`,`HHInc $25-35k`,`HHInc $35-50k`,
         `HHInc $50-75k` ,`HHInc $75-100k`,`HHInc $100-150k`, `HHInc $150-200k`, `HHInc >$200k`,
         `Edu Universe`,`Below High School`, `High School and Some College`,`Associate's Degree`,`Bachelor's Degree`,`Graduate or Professional Degree`,
         `Total Housing Units`, `Avg Persons per HH`, `Vacant HU`, `Occupied HU`, `OwnerOcc`, `RenterOcc`,
         `Family Households`, `Non-Family Households`, `Married Couple Households`,`HH_Married_with_child<18`, `HH_Married_without_child<18`,
         `school_age_children(5-17)`, `live_alone_nf_HH`, `live_alone_>65_HH`,
         `Single Family Home`, `Two to Four Units`, `Five or More Units`,
         `HH_Fam_2P`, `HH_Fam_3P`, `HH_Fam_4P`, `HH_Fam_5P`, `HH_Fam_6P`, `HH_Fam_7P`, 
         `HH_NFam_1P`, `HH_NFam_2P`,  `HH_NFam_3P`, `HH_NFam_4P`, `HH_NFam_5P`, `HH_NFam_6P`,
         Total_Work, Car_Total, Car_Alone, Carpool_Total,
         Transit_Total,Transit_Bus, Transit_Subway, Transit_Rail, Transit_LightRail,Transit_Ferry,
         Walk_Total,Other_Modes,Work_Home,
         ) 

PGC.data.2014.select <- PGC.data.2014 %>%
  mutate(
    children_5 = `Age under 5`,
    Junior_6_19 =  `Age 5 to 9` + `Age 10 to 14` + `Age 15 to 17` + `Age 18 to 19`,
    Young_20_24 = `Age 20` + `Age 21` + `Age 22 to 24`,
    Young_Pro_25_34 = `Age 25 to 29` + `Age 30 to 34`,
    Middle_Age_I_35_54 = `Age 35 to 39` + `Age 40 to 44` + `Age 45 to 49` + `Age 50 to 54`,
    Middle_Age_II_55_64 = `Age 55 to 59` + `Age 60 to 61` + `Age 62 to 64`,
    Senior_65 = `Age 65 to 66` + `Age 67 to 69` + `Age 70 to 74` + `Age 75 to 79` + `Age 80 to 84` + `Above 85`,
    Other_race = `Some other race` + `Two or more races`
  ) %>% 
  select(GEOID,`Total Population`, Male, Female,White,
         Black, Asian, `American Indian and Alaska Native`,`Native Hawaiian and Other Pacific Islander`, Other_race, Hispanic,`Non-Hispanic`,
         `Disability Universe`,`Population\tBelow Poverty Level`,`Total Families (Poverty Level Universe)`, `Under 200% of Poverty Level`,
         children_5,Junior_6_19,Young_20_24,Young_Pro_25_34,Middle_Age_I_35_54,Middle_Age_II_55_64, Senior_65,
         `All Households`, `HHInc <$10k`, `HHInc $10-15k`,`HHInc $15-25k`,`HHInc $25-35k`,`HHInc $35-50k`,
         `HHInc $50-75k` ,`HHInc $75-100k`,`HHInc $100-150k`, `HHInc $150-200k`, `HHInc >$200k`,
         `Edu Universe`,`Below High School`, `High School and Some College`,`Associate's Degree`,`Bachelor's Degree`,`Graduate or Professional Degree`,
         `Total Housing Units`,`Avg Persons per HH`, `Vacant HU`, `Occupied HU`, `OwnerOcc`, `RenterOcc`,
         `Family Households`, `Non-Family Households`, `Married Couple Households`,`HH_Married_with_child<18`, `HH_Married_without_child<18`,
         `school_age_children(5-17)`, `live_alone_nf_HH`, `live_alone_>65_HH`,
         `Single Family Home`, `Two to Four Units`, `Five or More Units`,
         `HH_Fam_2P`, `HH_Fam_3P`, `HH_Fam_4P`, `HH_Fam_5P`, `HH_Fam_6P`, `HH_Fam_7P`, 
         `HH_NFam_1P`, `HH_NFam_2P`,  `HH_NFam_3P`, `HH_NFam_4P`, `HH_NFam_5P`, `HH_NFam_6P`,
         Total_Work, Car_Total, Car_Alone, Carpool_Total,
         Transit_Total,Transit_Bus, Transit_Subway, Transit_Rail, Transit_LightRail,Transit_Ferry,
         Walk_Total,Other_Modes,Work_Home,
  )

write.csv(st_drop_geometry(PGC.data.2014.select), "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/PGC_data_2014_selected.csv", row.names = FALSE)
write.csv(st_drop_geometry(PGC.data.2022.select), "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/PGC_data_2022_selected.csv", row.names = FALSE)



# PGC.joined <- PGC.data.2022.select %>%
#   full_join(PGC.data.2014.select, by = "GEOID", suffix = c(".2022", ".2014")) %>%
#   filter(!is.na(GEOID))  # Ensure GEOID exists in the joined data
# 
# PGC.trend <- PGC.data.2022.select %>%
#   select(-GEOID, -geometry) %>%    # Exclude GEOID and geometry
#   mutate(
#     across(
#       everything(),                # Apply the operation to all columns
#       ~ (. - PGC.data.2014.select[[cur_column()]]) / PGC.data.2014.select[[cur_column()]],
#       .names = "{.col}_change"      # Append "_trend" to the new column names
#     )
#   ) %>%
#   bind_cols(                       # Re-add GEOID and geometry to the result
#     PGC.data.2022.select %>% select(GEOID, geometry)
#   )

# Interpolate 2014 into 2022 tracts -------------------------------------------------------
unmatched_2014 <- PGC.data.2014.select%>%
  filter(!GEOID %in% PGC.data.2022.select$GEOID)

unmatched_2022 <- PGC.data.2022.select%>%
  filter(!GEOID %in% PGC.data.2014.select$GEOID)

unmatched_2014 <- st_transform(unmatched_2014, crs = st_crs(unmatched_2022))
unmatched_2022 <- st_transform(unmatched_2022, crs = st_crs(unmatched_2014))

mapview(unmatched_2022, col.regions="red")  + mapview(unmatched_2014,col.regions="blue")

columns_to_interpolate <- names(unmatched_2014)[2:(ncol(unmatched_2014) - 1)]

st_crs(unmatched_2014) <- st_crs(unmatched_2022)
unmatched_2014 <- st_make_valid(unmatched_2014)
unmatched_2022 <- st_make_valid(unmatched_2022)

interpolated_results <- lapply(columns_to_interpolate, function(var) {
  unmatched_2014_single <- unmatched_2014 %>%
    select(GEOID, geometry, !!sym(var))  # Select the specific column for interpolation
  
  interpolate_pw(
    from = unmatched_2014_single,  # Source dataset with only one column
    to = unmatched_2022 %>% select(GEOID, geometry),           # Target dataset
    extensive = TRUE,              # Treat the variable as extensive
    weights = unmatched_2014_single,  # Use 2014 geometries for weighting
    weight_placement = "surface",     # Use area-weighted interpolation
    crs = 4269                         # CRS for both layers
  )
})


# Combine only the interpolated columns from the results
interpolated_columns <- do.call(cbind, lapply(interpolated_results, function(x) x[, -which(colnames(x) %in% c("id", "geometry"))]))

# Remove duplicate geometry columns
interpolated_columns <- interpolated_columns[, !grepl("^geometry", colnames(interpolated_columns))]
colnames(interpolated_columns)

#rbind with unmatch 2022 ID
geometry_reference <- unmatched_2022 %>%
  select(GEOID, geometry)

rownames(interpolated_columns) <- geometry_reference$GEOID
interpolated_columns <- cbind(geometry_reference, interpolated_columns)  # Add back the target geometry
interpolated_columns <- interpolated_columns[, !colnames(interpolated_columns) %in% "geometry.1"]
colnames(interpolated_columns)

colnames(interpolated_columns) <- colnames(unmatched_2014)
# mapview(interpolated_columns, col.regions = "red") 


#join he interpolated to remaining 2014
PGC.data.2014.remaining <- PGC.data.2014.select %>%
  filter(GEOID %in% PGC.data.2022.select$GEOID)

PGC.data.2014.interpolated <- rbind(PGC.data.2014.remaining, interpolated_columns)

all(sort(PGC.data.2014.interpolated$GEOID) == sort(PGC.data.2022$GEOID))

write.csv(st_drop_geometry(PGC.data.2014.interpolated), "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/PGC_data_2014_interpolated.csv", row.names = FALSE)

st_write(PGC.data.2014.interpolated, "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/Census_trend_shp/PGC_data_2014_interpolated.shp", overwrite = TRUE, append=FALSE)
st_write(PGC.data.2022.select, "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/Census_trend_shp/PGC_data_2022_select.shp", overwrite = TRUE, append=FALSE)



# joined_data <- unmatched_2014 %>%
#   select(GEOID) %>% 
#   st_join(unmatched_2022 %>% select(GEOID),
#           join = st_intersects, left = TRUE) %>% 
#   rename(
#     GEOID_2014 = GEOID.x,  # Rename GEOID from unmatched_2014
#     GEOID_2022 = GEOID.y   # Rename GEOID from unmatched_2022
#   )
# 
# 
# filtered_unmatched_2022 <- unmatched_2022 %>%
#   filter(GEOID %in% c("24033800216"))
# 
# filtered_unmatched_2014 <- unmatched_2014 %>%
#   filter(GEOID %in% c("24033800208","24033800214"))
# 
# # Use mapview to visualize the filtered data
# mapview(filtered_unmatched_2022, col.regions = "red") +
#   mapview(filtered_unmatched_2014, col.regions = "blue")
# 
# colnames(unmatched_2014)

# calculate change as trend 2022-2014 -------------------------------------------------------
numeric_columns <- setdiff(names(PGC.data.2022.select ), c("GEOID", "geometry"))

PGC_trend <- PGC.data.2022.select  %>%
  select(GEOID, geometry, all_of(numeric_columns)) %>%
  mutate(across(all_of(numeric_columns), ~ (. - PGC.data.2014.interpolated[[cur_column()]]) / PGC.data.2014.interpolated[[cur_column()]], .names = "{.col}_trend")) %>%
  select(GEOID, geometry, ends_with("_trend"))  # Keep only trend columns along with GEOID and geometry

PGC_trend2 <- PGC.data.2022.select %>%
  select(GEOID, geometry, all_of(numeric_columns)) %>%
  mutate(
    # Add trend (% change)
    across(
      all_of(numeric_columns),
      ~ (. - PGC.data.2014.interpolated[[cur_column()]]) / PGC.data.2014.interpolated[[cur_column()]],
      .names = "{.col}_trend"
    ),
    # Add absolute change
    across(
      all_of(numeric_columns),
      ~ (. - PGC.data.2014.interpolated[[cur_column()]]),
      .names = "{.col}_change"
    )
  ) %>%
  select(GEOID, geometry, ends_with("_trend"), ends_with("_change"))

wb <- createWorkbook()

addWorksheet(wb, "PGC_trend")
writeData(wb, "PGC_trend", st_drop_geometry(PGC_trend))  # Drop geometry if present

addWorksheet(wb, "PGC_data_2022")
writeData(wb, "PGC_data_2022", st_drop_geometry(PGC.data.2022.select))  # Drop geometry if present

addWorksheet(wb, "PGC_data_2014")
writeData(wb, "PGC_data_2014", st_drop_geometry(PGC.data.2014.interpolated))  # Drop geometry if present

# Save the workbook
saveWorkbook(wb, "PGC_data_trend_summary.xlsx", overwrite = TRUE)

mapview(PGC_trend, zcol = "Total Population_trend")
colnames(PGC_trend)

st_write(PGC_trend, "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/Census_trend_shp/PGC_trend.shp", overwrite = TRUE, append=FALSE)

write.csv(st_drop_geometry(PGC_trend), "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/Census_R/Outputs/PGC_trend.csv", row.names = FALSE)


PGC.data.2023.trend <- PGC.data.2022.select %>% 
  left_join(st_drop_geometry(PGC_trend), by = "GEOID")

PGC.data.2023.trend.change <- PGC.data.2022.select %>% 
  left_join(st_drop_geometry(PGC_trend2), by = "GEOID")

# Set up gpkg file -----------------------------
# gdb_path <- "O:/CLIENT/1 Consulting/Prince George's County/Fiscal Impact of Growth/GIS/PGC_maps/Census.gdb"

# st_write(census_shp_2014, "Outputs/gpkg/census_tract_2014.gpkg", layer = "census_tract_2014")
# st_write(census_shp_2022, "Outputs/gpkg/census_tract_2023.gpkg", layer = "census_tract_2023")
# 
# st_write(PGC.data.2014.interpolated, "Outputs/gpkg/PGC_data_2014_interpolated.gpkg", layer = "PGC_data_2014_interpolated")
# st_write(PGC.data.2022.select, "Outputs/gpkg/PGC_data_2022_select.gpkg", layer = "PGC_data_2022_select")
# 
# st_write(PGC_trend, "Outputs/gpkg/PGC_trend.gpkg", layer = "PGC_trend")
# st_write(PGC.data.2023.trend, "Outputs/gpkg/PGC_data_2023_trend.gpkg", layer = "PGC_data_2023_trend")
# 
# st_write(PGC.data.2023.trend.change, "Outputs/gpkg/PGC_data_2023_trend_change2.gpkg", layer = "PGC_data_2023_trend_change2")


st_write(census_shp_2014, "Outputs/gpkg_with_white_pop/census_tract_2014.gpkg", layer = "census_tract_2014")
st_write(census_shp_2022, "Outputs/gpkg_with_white_pop/census_tract_2023.gpkg", layer = "census_tract_2023")

st_write(PGC.data.2014.interpolated, "Outputs/gpkg_with_white_pop/PGC_data_2014_interpolated.gpkg", layer = "PGC_data_2014_interpolated")
st_write(PGC.data.2022.select, "Outputs/gpkg_with_white_pop/PGC_data_2022_select.gpkg", layer = "PGC_data_2022_select")

st_write(PGC_trend, "Outputs/gpkg_with_white_pop/PGC_trend.gpkg", layer = "PGC_trend")
st_write(PGC.data.2023.trend, "Outputs/gpkg_with_white_pop/PGC_data_2023_trend.gpkg", layer = "PGC_data_2023_trend")

st_write(PGC.data.2023.trend.change, "Outputs/gpkg_with_white_pop/PGC_data_2023_trend_change2.gpkg", layer = "PGC_data_2023_trend_change2")
