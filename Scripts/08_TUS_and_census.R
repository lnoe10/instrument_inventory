# Time Use Surveys - UNSD -------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

##### OLD TUS DATA #####
# https://unstats.un.org/unsd/gender/timeuse and manual ODW check of NSO websites
tus <- read_csv("Input/time_use_surveys_sgdf_inventory_2020.csv") %>%
  janitor::clean_names() %>%
  # Clean year column
  separate(tus_years_available, into = c("year1", "year2"), sep = ", ") %>%
  mutate(year1 = case_when(
    year1 == "N/A" ~ NA_character_,
    str_detect(year1, "/") ~ str_extract(year1, "(?<=/)[0-9]{4}"),
    TRUE ~ year1
  ),
  across(year1:year2, as.numeric),
  # Create other category variables
  instrument_type = "Time Use Survey", source = "https://unstats.un.org/unsd/gender/timeuse and manual ODW check of NSO websites",
  status = "Completed") %>%
  # Convert to longer for few countries with more than 1 obs and then drop missing obs
  pivot_longer(year1:year2, names_to = "indicator", values_to = "year") %>%
  filter(!is.na(year)) %>%
  select(country, iso3c = iso, year, instrument_name = time_use_survey, 
         instrument_type, source, status)

##### updated data from https://unstats.un.org/unsd/demographic-social/time-use/ #####
tus_new <- read_csv("Input/tus_unsd_data.csv", show_col_types = F) |> 
  rename(country = Country,
         survey_avail = `Survey Availability`,
         survey_year = `Year of the survey`) |> 
  mutate(instrument_type = "Time Use Survey", 
         source = "https://unstats.un.org/unsd/demographic-social/time-use/",
         status = "Completed")

tus_new |> group_by(country) |> summarise(n=n()) |> arrange(desc(n))



# Census - UNSD -----------------------------------------------------------


# See scraping census dates file
# Census dates scrape.R
census <- readRDS("Input/census_dates_df.rds") %>%
  # Filter out where we only have housing census
  filter(notes!="Housing census only."|is.na(notes)) %>%
  mutate(planned = as.character(planned),
         planned = case_when(
           planned == 1 ~ "Planned",
           TRUE ~ "Complete"
         ),
         instrument_name = "Population Census",
         instrument_type = "Census",
         source = "https://unstats.un.org/unsd/demographic-social/census/censusdates/") %>%
  select(country, iso3c, year, status = planned, instrument_name, instrument_type, source, census_round)