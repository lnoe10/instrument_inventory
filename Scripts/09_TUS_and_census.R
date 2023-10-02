# Time Use Surveys - UNSD -------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

# original TUS data based on Tawheeda's manual search
# https://unstats.un.org/unsd/gender/timeuse and manual ODW check of NSO websites
tus <- read_csv("Input/time_use_surveys_sgdf_inventory_2020.csv", show_col_types = F) %>%
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
         instrument_type, source, status) |> 
  filter(year >= 2013)

# data from https://unstats.un.org/unsd/demographic-social/time-use/
tus_unsd <- read_csv("Input/tus_unsd_data.csv", show_col_types = F) |> 
  rename(country = Country,
         survey_avail = `Survey Availability`,
         survey_year = `Year of the survey`) |> 
  mutate(instrument_type = "Time Use Survey", 
         source = "https://unstats.un.org/unsd/demographic-social/time-use/",
         status = "Completed",
         iso3c = countrycode::countrycode(country, "country.name", "iso3c")) |> 
  # filter to ogdi years
  filter(str_detect(survey_year, "201[3-9]|202") | str_detect(survey_year, "2012-13")) |> 
  rename(year = survey_year) |> 
  select(-survey_avail)

# look at overlap by country -- only Ethiopia -- add other 3 rows
tus_unsd |> filter(country %in% tus$country)

# add 3 rows that do not overlap with tus unsd data
tus_clean <- tus |> filter(country!="Ethiopia") |> 
  mutate(year = as.character(year)) |> 
  bind_rows(tus_unsd) |> 
  arrange(country)

# export clean dataset
#xlsx::write.xlsx(tus_clean, "Output/instrument_data_all_years/tus.xlsx")

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
         source = "https://unstats.un.org/unsd/demographic-social/census/censusdates/",
         # remove parentheses for dates with just a year like (2023)-->2023
         date = ifelse(str_detect(date, "^\\(\\d{4}\\)$"), str_remove_all(date, "\\(|\\)"), date)) %>%
  select(country, iso3c, year, status = planned, instrument_name, instrument_type, source, census_round, date)

# export a list of censuses with status planned and are from 2023 or prior
# we will manually check these ourselves to see if they have actually been completed and the data is just not updated
# census |> filter(status=="Planned" & year<=2023) |> xlsx::write.xlsx("Output/misc_data/planned_censuses.xlsx")

# note that this was added to a google sheets doc and we checked everything manually and then 
# overwrote the originally exported data with the data that had our added notes/changes downloaded from Google sheets

# read in list of censuses which have planned status and are from the year 2023 or prior that we have manually checked for completion
# note - this came from a separate inspection of differences between the old data and the newly scraped data, where many census dates have changed
planned_censuses <- readxl::read_xlsx("Output/misc_data/planned_censuses.xlsx") |> 
  rename(status = status_new) |> 
  mutate(census_round = as.character(census_round),
         # remove random dash from dates with just a year for merging
         date = ifelse(str_detect(date, "^-\\d{4}$"), str_remove(date, "^-"), date))

# left join
census_final <- left_join(census, planned_censuses |> select(-country), by=c("iso3c", "year", "date", "status", "census_round"))

# replace status variable with the update status data, and rename accordingly
census_final <- census_final |> mutate(updated_status = ifelse(is.na(updated_status) & !is.na(status), status, updated_status)) |> 
  select(-status) |> rename(status = updated_status, status_note = updated_status_note)

# export census dataset
# xlsx::write.xlsx(census_final, "Output/instrument_data_all_years/census.xlsx")
