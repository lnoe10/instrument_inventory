# script to combine all scraped instrument data into one spreadsheet

library(tidyverse)

# read in all prepared instrument data
mics <- readxl::read_xlsx("Output/instrument_data_all_years/mics.xlsx") |> mutate(year=as.character(year))
dhs <- readxl::read_xlsx("Output/instrument_data_all_years/dhs.xlsx") |> mutate(year=as.character(year))
lfs <- readxl::read_xlsx("Output/instrument_data_all_years/lfs_all.xlsx") |> mutate(year=as.character(year))
hies <- readxl::read_xlsx("Output/instrument_data_all_years/hies.xlsx") |> mutate(year=as.character(year))
ag_survey <- readxl::read_xlsx("Output/instrument_data_all_years/ag_surveys.xlsx") |> mutate(year=as.character(year))
ag_census <- readxl::read_xlsx("Output/instrument_data_all_years/ag_census.xlsx") |> mutate(year=as.character(year))
ihsn <- readxl::read_xlsx("Output/instrument_data_all_years/ihsn.xlsx")
tus <- readxl::read_xlsx("Output/instrument_data_all_years/tus.xlsx") |> mutate(year=as.character(year))
census <- readxl::read_xlsx("Output/instrument_data_all_years/census.xlsx") |> mutate(year=as.character(year))

# select the same variables across list of all data frames, and bind rows
all_instruments <- list(dhs, mics, hies, lfs, ag_survey, ag_census, tus, census) |> 
  map(~select(., country, iso3c, year, status, instrument_name, instrument_type, source)) |> 
  map_dfr(bind_rows)

##### FILTER FOR OGDI / CENSUS YEARS #####
# 2013-2022 / 2012/13-2021/22 for non-calendar years, 2010 and 2020 census rounds
dhs_clean <- dhs |> filter(year>=2013 & year<=2022) |> select(-`...1`)
mics_clean <- mics |> filter(year>=2013 & year<=2022) |> select(-`...1`)
hies_clean <- hies |> filter(year>=2013 & year<=2022) |> select(-`...1`)
lfs_clean <- lfs |> filter(year>=2013 & year<=2022) |> select(-`...1`)
ag_survey_clean <- ag_survey |> filter(year>=2013 & year<=2022) |> select(-`...1`)
tus_clean <- tus |> filter(year>=2013 & year<=2022) |> select(-`...1`)
dhs_clean <- dhs |> filter(year>=2013 & year<=2022) |> select(-`...1`)
ihsn_clean <- ihsn |> filter((year_start == 2012 & year_end >=2013) | year_start>=2013) |> 
  select(-`...1`) |> 
  mutate(year = case_when(year_start==year_end ~ as.character(year_start),
                          year_start!=year_end & year_end-year_start==1 ~ paste0(year_start, "/", str_extract(year_end, "\\d{2}$")),
                          year_start!=year_end & year_end-year_start>1 ~ paste0(year_start, "-", year_end))) 
ag_census_clean <- ag_census |> 
  filter(census_round==2010|census_round==2020) |> select(-`...1`)
census_clean <- census |> 
  filter(census_round==2010|census_round==2020) |> select(-`...1`)

###### Export filtered individual datasets ######
xlsx::write.xlsx(as.data.frame(dhs_clean), "Output/instrument_data_ogdi_years/dhs_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(mics_clean), "Output/instrument_data_ogdi_years/mics_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(hies_clean), "Output/instrument_data_ogdi_years/hies_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(lfs_clean), "Output/instrument_data_ogdi_years/lfs_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(ag_survey_clean), "Output/instrument_data_ogdi_years/ag_survey_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(tus_clean), "Output/instrument_data_ogdi_years/tus_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(dhs_clean), "Output/instrument_data_ogdi_years/dhs_2013-2022.xlsx", row.names = FALSE)
#xlsx::write.xlsx(as.data.frame(ihsn_clean), "Output/instrument_data_ogdi_years/ihsn_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(ag_census_clean), "Output/instrument_data_ogdi_years/ag_census_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(census_clean), "Output/instrument_data_ogdi_years/census_2013-2022.xlsx", row.names = FALSE)

#################################################

# Combining all the FILTERED datasets, with census round variable
all_surveys_census_filtered <- list(dhs_clean, mics_clean, hies_clean, lfs_clean, 
                                    ag_survey_clean, tus_clean) |> 
  map(~select(., country, iso3c, year, status, instrument_name, instrument_type, source)) |> 
  map_dfr(bind_rows) |> 
  bind_rows(census_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, census_round)) |>
  bind_rows(ag_census_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, census_round))

xlsx::write.xlsx(all_surveys_census_filtered, "Output/instrument_inventory_filtered.xlsx")

# Export
### NOTE - I am modifying this code with an if-else to allow country names that do not have an iso3c code to be maintained
all_surveys_census %>%
  filter(year>=2010, year<=2020) %>%
  rename(country_orig = country) %>%
  mutate(country = ifelse(!is.na(iso3c), countrycode::countrycode(iso3c, "iso3c", "country.name"), country_orig),
         country = case_when(
           iso3c == "ANT" ~ "Netherland Antilles",
           iso3c == "XKX" ~ "Kosovo",
           TRUE ~ country
         ), .before = iso3c) %>%
  arrange(iso3c, year, instrument_type) %>%
  select(-country_orig) |> 
  xlsx::write.xlsx("Output/instrument_inventory.xlsx")