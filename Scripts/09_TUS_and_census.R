# Time Use Surveys - UNSD -------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

#### 1. Import original sheet from State of Gender Data Financing. Contains very old data ####
# original TUS data based on Tawheeda's manual search
# https://unstats.un.org/unsd/gender/timeuse and manual ODW check of NSO websites
tus <- read_csv("Input/time_use_surveys_sgdf_inventory_2020.csv", show_col_types = F) |>
  janitor::clean_names() |>
  # Clean year column
  separate(tus_years_available, into = c("year1", "year2"), sep = ", ") |>
  mutate(year1 = case_when(
    year1 == "N/A" ~ NA_character_,
    str_detect(year1, "/") ~ str_extract(year1, "(?<=/)[0-9]{4}"),
    TRUE ~ year1
  ),
  across(year1:year2, as.numeric),
  # Create other category variables
  instrument_type = "Time Use Survey", source = "https://unstats.un.org/unsd/gender/timeuse and manual ODW check of NSO websites",
  status = "Completed") |>
  # Convert to longer for few countries with more than 1 obs and then drop missing obs
  pivot_longer(year1:year2, names_to = "indicator", values_to = "year") |>
  filter(!is.na(year)) |>
  select(country, iso3c = iso, year, instrument_name = time_use_survey, 
         instrument_type, source, status) |> 
  filter(year >= 2013)

#### 2. Import old UNSD time use survey data, AS OF 2023 ####
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

#### After Manual work was done to add more time use surveys in 2023 to the above files, we get this new workbook ####
tus_final <- readxl::read_excel("Input/tus_all_countries_2013-2022.xlsx", sheet = 1) |>
  mutate(second_year = as.numeric(str_extract(year, "(?<=-)[0-9]*$")),
         second_year = case_when(
           nchar(second_year) == 2 ~ (2000 + second_year),
           TRUE ~ second_year
         ),
         second_year = case_when(
           is.na(second_year) & !is.na(year) ~ as.numeric(year),
           TRUE ~ second_year
         )) |>
  # Dropping notes column, which contains page numbers for select publications if of interest
  select(country, iso3c = country_code, year = second_year, instrument_name = instrument_name_or_type, instrument_type, source, status) |>
  filter(!is.na(year))

#### 2025 update, keeping old data and using to fill in new data where necessary ####
# From https://unstats.un.org/UNSDWebsite/demographic-social/time-use/data-and-metadata
# have to load in via tempfile because we don't have a direct link
temp_file <- tempfile(fileext = ".xlsx")
download.file(
  url = "https://undesa.maps.arcgis.com/sharing/rest/content/items/3182e4041ff3411990c14b2213b9ada6/data",
  destfile = temp_file,
  mode = "wb"
)

# Read new df from tempfile
tus_2025 <- readxl::read_excel(temp_file) |>
  janitor::clean_names()

#Clean up
unlink(temp_file)

# Further work on df
tus_2025_clean <- tus_2025 |>
  mutate(year = as.numeric(year_of_the_survey),
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         status = "Completed",
         source = "https://unstats.un.org/UNSDWebsite/demographic-social/time-use/data-and-metadata",
         instrument_type = "Time Use Survey") |>
  select(country, iso3c, year, instrument_name = name_of_survey, instrument_type, source, status)

# Compare to tus_final, last final update in 2023.
tus_full <- tus_2025_clean |>
  full_join(tus_final, by = c("iso3c", "year")) |>
  mutate(country.x = case_when(iso3c == "TZA" ~ "United Republic of Tanzania", is.na(country.x) ~ country.y, TRUE ~ country.x),
         instrument_name.x = case_when(is.na(instrument_name.x) ~ instrument_name.y, TRUE ~ instrument_name.x),
         instrument_type.x = case_when(is.na(instrument_type.x) ~ instrument_type.y, TRUE ~ instrument_type.x),
         source.x = case_when(is.na(source.x) ~ source.y, TRUE ~ source.x),
         status.x = case_when(is.na(status.x) ~ status.y, TRUE ~ status.x)) |>
  select(country = country.x, iso3c, year, instrument_name = instrument_name.x, instrument_type = instrument_type.x, source = source.x, status = status.x)

# Compare and update with other older data. Adds Afghanistan pilot survey and 2014 Greece survey
tus_full <- tus_full |>
  bind_rows(tus_clean |>
              filter(iso3c %in% c("AFG", "GRC"))|>
              mutate(year = case_when(iso3c == "GRC" ~ "2014", TRUE ~ year),
                     year = as.numeric(year))) |>
  arrange(iso3c, year)

# export clean dataset
xlsx::write.xlsx(tus_full, "Output/instrument_data_all_years/tus.xlsx")

# Census - UNSD -----------------------------------------------------------

# See scraping census dates file
# Census dates scrape.R
census <- readRDS("Input/census_dates_df.rds") |>
  # Filter out where we only have housing census
  filter(notes!="Housing census only."|is.na(notes)) |>
  mutate(planned = as.character(planned),
         planned = case_when(
           planned == 1 ~ "Planned",
           TRUE ~ "Complete"
         ),
         instrument_name = "Population Census",
         instrument_type = "Census",
         source = "https://unstats.un.org/unsd/demographic-social/census/censusdates/",
         # remove parentheses for dates with just a year like (2023)-->2023
         date = ifelse(str_detect(date, "^\\(\\d{4}\\)$"), str_remove_all(date, "\\(|\\)"), date)) |>
  select(country, iso3c, year, status = planned, instrument_name, instrument_type, source, census_round, date)

# Code below involved manual check for census status. Not doing this check for this round
# Already have status variable that indicates Complete/Planned
## export a list of censuses with status planned and are from 2023 or prior
## we will manually check these ourselves to see if they have actually been completed and the data is just not updated
## census |> filter(status=="Planned" & year<=2023) |> xlsx::write.xlsx("Output/misc_data/planned_censuses.xlsx")
#
## note that this was added to a google sheets doc and we checked everything manually and then 
## overwrote the originally exported data with the data that had our added notes/changes downloaded from Google sheets
#
## read in list of censuses which have planned status and are from the year 2023 or prior that we have manually checked for completion
## note - this came from a separate inspection of differences between the old data and the newly scraped data, where many census dates have changed
#planned_censuses <- readxl::read_xlsx("Output/misc_data/planned_censuses.xlsx") |> 
#  rename(status = status_new) |> 
#  mutate(census_round = as.character(census_round),
#         # remove random dash from dates with just a year for merging
#         date = ifelse(str_detect(date, "^-\\d{4}$"), str_remove(date, "^-"), date))
#
## left join
#census_final <- left_join(census, planned_censuses |> select(-country), by=c("iso3c", "year", "date", "status", "census_round"))
#
## replace status variable with the update status data, and rename accordingly
#census_final <- census_final |> mutate(updated_status = ifelse(is.na(updated_status) & !is.na(status), status, updated_status)) |> 
#  select(-status) |> rename(status = updated_status, status_note = updated_status_note)

# export census dataset
xlsx::write.xlsx(census, "Output/instrument_data_all_years/census.xlsx")
