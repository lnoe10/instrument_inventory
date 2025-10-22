### script to scrape MICS data

library(httr)
library(jsonlite)
library(tidyverse)
library(openxlsx)

# Download raw file, convert to tibble, and restrict to relevant variables
# CSV file from https://mics.unicef.org/surveys
mics_raw <- read.csv('Input/mics_surveys_catalogue.csv') %>%
  as_tibble() %>%
  select(survey, country, year, status, notes = round)

# There are sub-national surveys that we need to separate from
# national surveys, which is what we count as producing data that can be used
# for national monitoring of gender data.
# The variable country describes the country name in case the variable
# survey only describes a sub-set of the country
# To get the national surveys we want, we filter as follows:
# Where country == survey
# Exceptions:
#   Nepal (Six Cycles) was a national survey
#   Kosovo is mislabelled for 1996 and 2000 in country_in_filter. 
#     It describes FR Yugoslavia, which was Serbia and Montenegro.
#     These two will be added manually.
#   Sudan (including current South Sudan) was an additional note but this
#     was pre=independence so it's just Sudan

# Create clean dataframe
mics <- mics_raw %>%
  # Drop Federal Republic of Yugoslavia, which was a union between
  # present-day Serbia and Montenegro. Duplicate these rows and assign to either country
  filter(!str_detect(survey, "Yugoslavia")) %>%
  bind_rows(mics_raw %>% 
              filter(str_detect(survey, "Yugoslavia")) %>% 
              uncount(2, .id = "id") %>%
              mutate(survey = case_when(
                id == 1 ~ "Serbia",
                TRUE ~ "Montenegro"),
                country = survey) %>%
              select(-id)) %>%
  # Correct survey variable names where country clash but it's actually
  # a national survey
  mutate(survey = case_when(
    str_detect(survey, "Six Cycles") ~ "Nepal",
    str_detect(survey, "including current South Sudan") ~ "Sudan",
    survey == "Marshall Islands, Republic of" ~ "Marshall Islands",
    survey == "Syrian Arab Republic" ~ "Syria",
    survey == "South Sudan, Republic of" ~ "South Sudan",
    survey == "Kosovo under UNSC res. 1244" ~ "Kosovo (UNSC 1244)",
    survey == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    survey == "Sao Tome and Principe" ~ "Sao Tome & Principe",
    survey == "North Macedonia, Republic of" ~ "North Macedonia",
    survey == "Korea, Democratic People's Republic of" ~ "Korea DPR",
    survey == "Congo, Democratic Republic of the" ~ "Congo DR",
    survey == "Côte d'Ivoire" ~ "Cote d'Ivoire",
    survey == "Moldova, Republic of" ~ "Moldova",
    survey == "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
    survey == "Myanmar, Republic of the Union of" ~ "Myanmar",
    survey == "Bolivia, Plurinational State of" ~ "Bolivia",
    survey == "Venezuela, Bolivarian Republic of" ~ "Venezuela",
    survey == "Iran, Islamic Republic of" ~ "Iran",
    survey == "Tanzania, United Republic of" ~ "Tanzania",
    survey == "Türkiye" ~ "Turkiye",
    TRUE ~ survey)) %>%
  # Filter where agreement between country_in_filter and country that
  # this was a national survey, which is what we want.
  filter(country == survey) %>%
  # Additional cleaning, creating necessary identifiers
  mutate(
    iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
    iso3c = case_when(
      str_detect(country, "Kosovo") ~ "XKX",
      str_detect(country, "iye$") ~ "TUR",
      TRUE ~ iso3c
    ),
    # Create clean country column
    country_clean = countrycode::countrycode(iso3c, "iso3c", "country.name"),
    country_clean = case_when(iso3c == "XKX" ~ "Kosovo",
                              iso3c == "TUR" ~  "Türkiye",
                              TRUE ~ country_clean),
    # Clean years, keeping last year. in case of survey like 2015-2016, keep 2016
    year = as.numeric(case_when(
      str_detect(year, "-") ~ str_extract(year, "(?<=-)[0-9]{4}"),
      TRUE ~ year
    )),
    source = "https://mics.unicef.org/surveys", instrument_name = "MICS",
    instrument_type = "Household health survey") %>%
  select(country = country_clean, iso3c, year, status, instrument_name, instrument_type, source, country_original = country, notes)

# export the cleaned dataset

openxlsx::write.xlsx(mics, "Output/instrument_data_all_years/mics.xlsx")