### script to scrape DHS data

library(httr)
library(jsonlite)
library(tidyverse)

# scrape data
dhs_raw <- fromJSON(content(GET("https://api.dhsprogram.com/rest/dhs/surveys?surveyStatus=all"), "text"))$Data %>%
  as_tibble() 

# clean data
dhs <- dhs_raw |> 
  add_row(SurveyType = "DHS", SurveyYearLabel = "2023", SurveyYear = "2023", DHS_CountryCode = "ZM",
          CountryName = "Zambia", SubregionName = "Eastern Africa", SurveyStatus = "Ongoing", RegionName = "Sub-Saharan Africa") %>%
  janitor::clean_names() %>%
  # Filtering out Other surveys, Service Provision Assessments (SPA) and Special DHS, as the latter is mostly conducted at the sub-national level
  # Assessed 35 other surveys, of which 10 were from after 2010 and were deemed gender-relevant and without double-counting MICS, for example.
  filter(survey_type %in% c("AIS", "DHS", "MIS") | (survey_type == "OTH" & survey_id %in% c("AF2010OTH","EG2015OTH","GH2017OTH","GN2016OTH","HT2013OTH",
                                                                                            "ID2012OTH","ID2017OTH","ML2010OTH","PK2019OTH","RW2011OTH"))) %>%
  rename(country = country_name, 
         year = survey_year, 
         status = survey_status) |> 
  mutate(instrument_type = "Household health survey", 
         source = "https://dhsprogram.com/Methodology/survey-search.cfm?pgtype=main&SrvyTp=year#",
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         year = as.numeric(year),
         country_clean = countrycode::countrycode(iso3c, "iso3c", "country.name")) |> 
  select(country = country_clean, iso3c, year, status, instrument_name = survey_type, instrument_type, source, country_original = country)

# export the clean dataset
xlsx::write.xlsx(dhs, "Output/instrument_data_all_years/dhs.xlsx")