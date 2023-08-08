### script to scrape MICS data

library(httr)
library(jsonlite)
library(tidyverse)

# Download raw file, convert to tibble, and restrict to relevant variables
mics_raw <- fromJSON(content(GET("https://mics.unicef.org/api/survey"), "text"), flatten = TRUE) %>%
  as_tibble() |> 
  select(country_in_filter, country, year, status, notes = round) 

# There are sub-national surveys that we need to separate from
# national surveys, which is what we count as producing data that can be used
# for national monitoring of gender data.
# The variable country_in_filter describes the country name in case the variable
# country only describes a sub-set of the country
# To get the national surveys we want, we filter as follows:
# Where country_in_filter == country
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
  filter(!str_detect(country, "Yugoslavia")) %>%
  bind_rows(mics_raw %>% 
              filter(str_detect(country, "Yugoslavia")) %>% 
              uncount(2, .id = "id") %>%
              mutate(country = case_when(
                id == 1 ~ "Serbia",
                TRUE ~ "Montenegro"),
                country_in_filter = country) %>%
              select(-id)) %>%
  # Correct country variable names where country_in_filter clash but it's actually
  # a national survey
  mutate(country = case_when(
    str_detect(country, "Six Cycles") ~ "Nepal",
    str_detect(country, "including current South Sudan") ~ "Sudan",
    TRUE ~ country)) %>%
  # Filter where agreement between country_in_filter and country that
  # this was a national survey, which is what we want.
  filter(country_in_filter == country) %>%
  # Additional cleaning, creating necessary identifiers
  mutate(
    iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
    iso3c = case_when(
      str_detect(country, "Kosovo") ~ "XKX",
      TRUE ~ iso3c
    ),
    # Create clean country column
    country_clean = countrycode::countrycode(iso3c, "iso3c", "country.name"),
    country_clean = case_when(iso3c == "XKX" ~ "Kosovo", TRUE ~ country_clean),
    # Clean years, keeping last year. in case of survey like 2015-2016, keep 2016
    year = as.numeric(case_when(
      str_detect(year, "-") ~ str_extract(year, "(?<=-)[0-9]{4}"),
      TRUE ~ year
    )),
    source = "https://mics.unicef.org/surveys", instrument_name = "MICS",
    instrument_type = "Household health survey") %>%
  select(country = country_clean, iso3c, year, status, instrument_name, instrument_type, source, country_original = country, notes)

# filter to OGDI years only
mics_clean <- mics |> filter(year>=2013 & year<=2022)

# export filtered and full datasets
#xlsx::write.xlsx(mics_clean, "Output/mics_ogdi_yrs.xlsx")
xlsx::write.xlsx(mics, "Output/mics.xlsx")

