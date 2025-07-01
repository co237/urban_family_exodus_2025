## Family exodus blog July 2025

# Load libraries
library(dplyr)
library(stringr)
library(tidyverse)

# Load the data
exodus_data <- read.csv("cc-est2024-alldata.csv")
# Load county typology
county_types <- read.csv("county_summary_final.csv")

# Add FIPS codes

exodus_data <- exodus_data %>%
  mutate(
    COUNTY = str_pad(COUNTY, width = 3, side = "left", pad = "0"),
    FIPS = paste0(STATE, COUNTY)
  )

# Join county types and add age bucket descriptions

county_types$county_fips <- as.character(county_types$county_fips)

exodus_data <- exodus_data %>%
  left_join(
    county_types %>% select(county_fips, locale_type, county_nm),
    by = c("FIPS" = "county_fips")
  ) %>%
  mutate(Age_group = case_when(
    AGEGRP == 0 ~ "Total",
    AGEGRP == 1 ~ "Under 5",
    AGEGRP == 2 ~ "Age 5 to 9",
    AGEGRP == 3 ~ "Age 10 to 14",
    AGEGRP == 4 ~ "Age 15 to 19",
    AGEGRP == 5 ~ "Age 20 to 24",
    AGEGRP == 6 ~ "Age 25 to 29",
    AGEGRP == 7 ~ "Age 30 to 34",
    AGEGRP == 8 ~ "Age 35 to 39",
    AGEGRP == 9 ~ "Age 40 to 44",
    AGEGRP == 10 ~ "Age 45 to 49",
    AGEGRP == 11 ~ "Age 50 to 54",
    AGEGRP == 12 ~ "Age 55 to 59",
    AGEGRP == 13 ~ "Age 60 to 64",
    AGEGRP == 14 ~ "Age 65 to 69",
    AGEGRP == 15 ~ "Age 70 to 74",
    AGEGRP == 16 ~ "Age 75 to 79",
    AGEGRP == 17 ~ "Age 80 to 84",
    AGEGRP == 18 ~ "Age 85 and up"
  ))

# Under 5 analysis

## All counties
under_5_panel <- exodus_data %>%
  filter(AGEGRP==1) %>%
  select(FIPS, TOT_POP, YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = TOT_POP) %>%
  rename(April_2020 =`1`,
         July_2020 = `2`,
         July_2021 =  `3`,
         July_2022 = `4`,
         July_2023 = `5`,
         July_2024 = `6`
  ) %>%
  mutate(pc_21 = (July_2021-July_2020)/July_2020*100,
         pc_22 = (July_2022-July_2021)/July_2021*100,
         pc_23 = (July_2023-July_2022)/July_2022*100,
         pc_24 = (July_2024-July_2023)/July_2023*100,
         pc_since_20 = (July_2024-April_2020)/April_2020*100,
         raw_change_21 = July_2021-July_2020,
         raw_change_22 = July_2022-July_2021,
         raw_change_23 = July_2023-July_2022,
         raw_change_24 = July_2024-July_2023,
         raw_change_since_april_20 = July_2024- April_2020
  )%>%
  left_join(
    county_types %>% select(county_fips, locale_type, county_nm),
    by = c("FIPS" = "county_fips")
  )

## Summarise by county type
under_5_county_types <- under_5_panel %>%
  group_by(locale_type) %>%
  summarise(April_2020 = sum(April_2020),
            July_2020 = sum(July_2020),
            July_2021 = sum(July_2021),
            July_2022 = sum(July_2022),
            July_2023 = sum(July_2023),
            July_2024 = sum(July_2024)
  ) %>%
  mutate(pc_21 = (July_2021-July_2020)/July_2020*100,
         pc_22 = (July_2022-July_2021)/July_2021*100,
         pc_23 = (July_2023-July_2022)/July_2022*100,
         pc_24 = (July_2024-July_2023)/July_2023*100,
         pc_since_20 = (July_2023-April_2020)/April_2020*100
  )

## National demographics

national_demographics <- exodus_data %>%
  filter(AGEGRP!= 0) %>%
  select(Age_group, YEAR, TOT_POP, FIPS) %>%
  pivot_wider(names_from = YEAR, values_from = TOT_POP) %>%
  rename(April_2020 =`1`,
         July_2020 = `2`,
         July_2021 =  `3`,
         July_2022 = `4`,
         July_2023 = `5`,
         July_2024 = `6`
  ) %>%
  group_by(Age_group) %>%
  summarise(April_2020 = sum(April_2020),
            July_2020 = sum(July_2020),
            July_2021 = sum(July_2021),
            July_2022 = sum(July_2022),
            July_2023 = sum(July_2023) ,
            July_2024 = sum(July_2024)
  ) %>%
  mutate(change_2021 = July_2021-July_2020,
         change_2022 = July_2022-July_2021,
         change_2023 = July_2023 - July_2022,
         change_2024 = July_2024 - July_2023,
         pc_21 = (July_2021-July_2020)/July_2020*100,
         pc_22 = (July_2022-July_2021)/July_2021*100,
         pc_23 = (July_2023-July_2022)/July_2022*100,
         pc_since_20 = (July_2024-April_2020)/April_2020*100,
         pc_24 = (July_2024-July_2023)/July_2023*100
  )

## Large urban counties only

under_5_panel_large_urban <- under_5_panel %>%
  filter(locale_type == "Large urban")

## New York City analysis

nyc_under_5 <- exodus_data %>%
  select(FIPS, county_nm, Age_group, YEAR, TOT_POP) %>%
  filter(county_nm == "Kings County, New York" |
           county_nm == "Queens County, New York" |
           county_nm == "New York County, New York" |
           county_nm == "Richmond County, New York" |
           county_nm == "Bronx County, New York",
         Age_group == "Under 5") %>%
  pivot_wider(names_from = YEAR, values_from = TOT_POP) %>%
  rename(April_2020 =`1`,
         July_2020 = `2`,
         July_2021 =  `3`,
         July_2022 = `4`,
         July_2023 = `5`,
         July_2024 = `6`
  ) %>%
  group_by(county_nm) %>%
  summarise(April_2020 = sum(April_2020),
            July_2020 = sum(July_2020),
            July_2021 = sum(July_2021),
            July_2022 = sum(July_2022),
            July_2023 = sum(July_2023),
            July_2024 = sum(July_2024)
  ) %>%
  mutate(pc_21 = (July_2021-July_2020)/July_2020*100,
         pc_22 = (July_2022-July_2021)/July_2021*100,
         pc_23 = (July_2023-July_2022)/July_2022*100,
         pc_24 = (July_2024-July_2023)/July_2023*100,
         pc_since_20 = (July_2024-April_2020)/April_2020*100
  ) 

nyc_under_5_total <- new_york_city %>%
  mutate(county_nm = "New York City") %>%
  filter(Age_group == "Under 5") %>%
  select(county_nm, pc_21, pc_22, pc_23, pc_24)

## NYC boroughs and city-wide bar chart data
nyc_graph <- nyc_under_5 %>%
  select(county_nm, pc_21, pc_22, pc_23, pc_24) %>%
  rbind(nyc_under_5_total) %>%
  mutate(county_nm = case_when(
    county_nm == "New York County, New York" ~ "Manhattan",
    county_nm == "Bronx County, New York" ~ "Bronx",
    county_nm == "Queens County, New York" ~ "Queens",
    county_nm == "Kings County, New York" ~ "Brooklyn",
    county_nm == "Richmond County, New York" ~ "Staten Island",
    county_nm == "New York City" ~ "New York City"
  ))

## Fastest-shrinking large urban counties table

Fastest_large_urban_shrinkers <- under_5_panel_large_urban %>%
  arrange(pc_since_20) %>%
  head(10) %>%
  select(county_nm, pc_since_20, pc_24, July_2024)

## Fastest-growing large urban counties table

Fastest_large_urban_growers <- under_5_panel_large_urban %>%
  arrange(desc(pc_since_20)) %>%
  head(10) %>%
  select(county_nm, pc_since_20, pc_24, July_2024)

## County types graph

national_under_5 <- national_demographics %>%
  filter(Age_group=="Under 5") %>%
  mutate(locale_type = "National") %>%
  select(locale_type, pc_24, pc_since_20)

under_5_county_types_graph <- under_5_county_types %>%
  select(locale_type, pc_24, pc_since_20) %>%
  rbind(national_under_5)

## Large urban vs national_graph

large_urban_vs_national <- under_5_panel_large_urban %>%
  summarise(April_2020 = sum(April_2020),
            July_2020 = sum(July_2020),
            July_2021 = sum(July_2021),
            July_2022 = sum(July_2022),
            July_2023 = sum(July_2023),
            July_2024 = sum(July_2024)
  ) %>%
  mutate(pc_21 = (July_2021-July_2020)/July_2020*100,
         pc_22 = (July_2022-July_2021)/July_2021*100,
         pc_23 = (July_2023-July_2022)/July_2022*100,
         pc_24 = (July_2024-July_2023)/July_2023*100,
         pc_since_20 = (July_2024-April_2020)/April_2020*100,
         locale_type = "Large urban"
  )%>%
  select(locale_type, pc_21, pc_22, pc_23,pc_24)

national_under_5 <- national_demographics %>%
  filter(Age_group=="Under 5") %>%
  mutate(locale_type = "National") %>%
  select(locale_type, pc_21, pc_22, pc_23, pc_24)

large_urban_vs_national <- large_urban_vs_national %>%
  rbind(national_under_5)