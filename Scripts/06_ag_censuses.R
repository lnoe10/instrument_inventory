### script to scrape Agricultural Census data from the FAO

library(tidyverse)

# Upload and clean list of 2020 and 2010 round of ag census
ag_census <- read_csv("Input/wca_2020_2010_notes.csv", show_col_types = F) %>%
  janitor::clean_names() %>%
  mutate(year_clean = case_when(
    str_detect(census_year, "-[0-9]{2}$") ~ str_c("20", str_extract(census_year, "[0-9]{2}$")),
    census_year == "-" ~ NA_character_,
    census_year == "AGRIS since 2018" ~ NA_character_,
    str_detect(census_year, "[0-9]{4}$") ~ str_extract(census_year, "[0-9]{4}$"),
    str_detect(census_year, "\\/[0-9]{2}$") ~ str_c("20", str_extract(census_year, "[0-9]{2}$")),
    TRUE ~ census_year),
  year_clean = as.numeric(year_clean),
  iso3c = countrycode::countrycode(country_link_to_national_census_organisation, "country.name", "iso3c"),
  iso3c = case_when(country_link_to_national_census_organisation == "Netherland" ~ "NLD", TRUE ~ iso3c),
  instrument_name = "Agriculture Census",
  instrument_type = "Agricultural Survey/Census",
  status = "completed",
  source = "http://www.fao.org/world-census-agriculture/wcarounds/en/") %>%
  filter(!is.na(iso3c), !is.na(year_clean), !(census_round == "WCA2020" & iso3c == "MOZ")) %>%
  select(country = starts_with("country"), iso3c, year = year_clean, instrument_name, instrument_type, status, source, census_round)
