#' ---
#' title: Gender-relevant instrument inventory
#' author: Lorenz Noe
#' ---
# setwd("C:/Users/loren/Documents/GitHub/instrument_inventory/")

library(httr)
library(jsonlite)
library(tidyverse)

# Household Health Surveys ------------------------------------------------
# MICS
# DHS

# * MICS - UNICEF -----------------------------------------------------------

# https://mics.unicef.org/surveys
# Also 
# API https://mics.unicef.org/api/survey

# Download raw file, convert to tibble, and restrict to relevant variables
mics_raw <- fromJSON(content(GET("https://mics.unicef.org/api/survey"), "text"), flatten = TRUE) %>%
  as_tibble() %>%
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
                TRUE ~ "Montenegro"
              ),
              country_in_filter = country) %>%
              select(-id)) %>%
  # Correct country variable names where country_in_filter clash but it's actually
  # a national survey
  mutate(country = case_when(
    str_detect(country, "Six Cycles") ~ "Nepal",
    str_detect(country, "including current South Sudan") ~ "Sudan",
    TRUE ~ country
  )) %>%
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
  rename(country = country_clean,
         country_original = country) |>
  # dropping this one because it's the same as country_original
  select(-country_in_filter)
  # Choosing final list of instruments - commenting this out to maintain metadata, just renaming same variables
  #select(country = country_clean, iso3c, year, status, instrument_name, instrument_type, source, country_original = country)

# * DHS - USAID -------------------------------------------------------------

# https://dhsprogram.com/Methodology/survey-search.cfm?pgtype=main&SrvyTp=year#
# Completed and ongoing DHS. Was short of one, Zambia in 2023
# API https://api.dhsprogram.com/rest/dhs/surveys?surveyStatus=all&f=html
dhs <- fromJSON(content(GET("https://api.dhsprogram.com/rest/dhs/surveys?surveyStatus=all"), "text"))$Data %>%
  as_tibble() %>%
  add_row(SurveyType = "DHS", SurveyYearLabel = "2023", SurveyYear = "2023", DHS_CountryCode = "ZM",
          CountryName = "Zambia", SubregionName = "Eastern Africa", SurveyStatus = "Ongoing", RegionName = "Sub-Saharan Africa") %>%
  janitor::clean_names() %>%
  # Filtering out Other surveys, Service Provision Assessments (SPA) and Special DHS, as the latter is mostly conducted at the sub-national level
  # Assessed 35 other surveys, of which 10 were from after 2010 and were deemed gender-relevant and without double-counting MICS, for example.
  filter(survey_type %in% c("AIS", "DHS", "MIS") | (survey_type == "OTH" & survey_id %in% c("AF2010OTH","EG2015OTH","GH2017OTH","GN2016OTH","HT2013OTH",
                                                                                            "ID2012OTH","ID2017OTH","ML2010OTH","PK2019OTH","RW2011OTH"
  ))) %>%
  ### NOTE - THIS STEP MEANS A LOT OF VARIABLES ARE BEING DROPPED - not sure if they are valuable for our purposes though so leaving it as-is
  select(country = country_name, year = survey_year, survey_type, status = survey_status) %>%
  mutate(instrument_type = "Household health survey", source = "https://dhsprogram.com/Methodology/survey-search.cfm?pgtype=main&SrvyTp=year#",
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         year = as.numeric(year),
         country_clean = countrycode::countrycode(iso3c, "iso3c", "country.name")) %>%
  # leaving this because it includes all variables anyway
  select(country = country_clean, iso3c, year, status, instrument_name = survey_type, instrument_type, source, country_original = country)


# Household Income/Expenditure Surveys (HIES) -----------------------------


# * LSMS - WORLD BANK -----------------------------------------------------

# https://microdata.worldbank.org/index.php/catalog/lsms
# https://microdata.worldbank.org/index.php/api/catalog/search?ps=10000
#^ filter for LSMS from there.
lsms_raw <- fromJSON(content(GET("https://microdata.worldbank.org/index.php/api/catalog/search?ps=10000"), "text"))$result$rows %>%
  as_tibble() %>%
  mutate(api_call = str_c("https://microdata.worldbank.org/index.php/api/catalog/", id, "?id_format=id"))

## First acquire more information on surveytypes from metadata  
#
## Initialize empty dataset
#all_wb_metadata <- tibble()
#
## Set up progress bar
#n_iter <- length(lsms_raw$id)
#pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                     max = n_iter, # Maximum value of the progress bar
#                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                     width = 50,   # Progress bar width. Defaults to getOption("width")
#                     char = "=")   # Character used to create the bar
#
## Loop
#for (i in 1:length(lsms_raw$id)){
#  # Set up tibble with basic info
#  study <- tibble(id = lsms_raw$id[i], study_type = NA_character_)
#  # Determine length of list generated from API call to filter out
#  # Where we hit error otherwise of subscript being out of bounds
#  study <- study %>%
#    mutate(length = case_when(
#      !is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)),
#      TRUE ~ NA_integer_))
#  # If list is deep enough, proceed to extract data
#  if(study$length > 3){
#    study <- study %>%
#      # Test first whether response is valid based on tunneling into list
#      mutate(study_type = ifelse(
#        is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
#        fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
#      ))
#  }
#  # Append to dataset
#  all_wb_metadata <- all_wb_metadata %>%
#    bind_rows(study)
#  # Insert brief pause in code to not look like a robot to the API
#  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
#  # Increment progress bar
#  setTxtProgressBar(pb, i)
#}
#
#close(pb) # Close the connection
#
## Missing 539 entries for study type because metadata does not have this field
## 'Data kind' is closest equivalent for many of them. Repeat calls and acquire these
#
## Filter to where we are missing study_type.
## Also create url entry as variable so it's easier to read in API call
#all_wb_missing <- all_wb_metadata %>%
#  filter(is.na(study_type)) %>%
#  mutate(api_call = str_c("https://microdata.worldbank.org/index.php/api/catalog/", id, "?id_format=id"))
#
## Initialize empty dataset
#addl_wb_metadata <- tibble()
#
## Set up progress bar
#n_iter <- length(all_wb_missing$id)
#pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                     max = n_iter, # Maximum value of the progress bar
#                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                     width = 50,   # Progress bar width. Defaults to getOption("width")
#                     char = "=")   # Character used to create the bar
#
## Loop
#for (i in 1:length(all_wb_missing$id)){
#  # Set up tibble with basic info
#  study <- tibble(id = all_wb_missing$id[i], study_type = NA_character_)
#  # Determine length of list generated from API call to filter out
#  # Where we hit error otherwise of subscript being out of bounds
#  study <- study %>%
#    mutate(length = case_when(
#      !is_empty(fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)),
#      TRUE ~ NA_integer_))
#  # If list is deep enough, proceed to extract data
#  if(study$length > 3){
#    study <- study %>%
#      # Test first whether response is valid based on tunneling into list
#      mutate(study_type = ifelse(
#        is_empty(fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
#        fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind
#      ))
#  }
#  # Append to dataset
#  addl_wb_metadata <- addl_wb_metadata %>%
#    bind_rows(study)
#  # Insert brief pause in code to not look like a robot to the API
#  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
#  # Increment progress bar
#  setTxtProgressBar(pb, i)
#}
#
#close(pb) # Close the connection
#
## Merge with other df of metadata tags
#wb_study_description <- all_wb_metadata %>%
#  filter(!is.na(study_type)) %>%
#  mutate(meta_data_field = "Study Type") %>%
#  bind_rows(addl_wb_metadata %>% mutate(meta_data_field = "Data Kind")) %>%
#  select(-length)
#
## ABout 92  remaining with no relevant field. Will have to filter on other metadata.
#
## Save copy of WB study descriptions so we don't have to rerun all entries, just new ones
#saveRDS(wb_study_description, file = "Input/wb_microdata_study_description.rds")

# Load study descriptions from saved file
wb_study_description <- readRDS("Input/wb_microdata_study_description.rds")

# Set up final list of LSMS/HIES
lsms <- lsms_raw %>%
  # Duplicate Serbia and Montenegro and assign to Serbia and Montenegro respectively
  filter(nation != "Serbia and Montenegro") %>%
  bind_rows(
    lsms_raw %>% 
      filter(str_detect(nation, "Serbia and Montenegro")) %>% 
      uncount(2, .id = "id") %>%
      mutate(nation = case_when(
        id == 1 ~ "Serbia",
        TRUE ~ "Montenegro"
      )) %>%
      select(-id)
  ) %>%
  mutate(year = as.numeric(str_extract(idno, "[0-9]{4}")),
         year = case_when(nation == "Malawi" & year == 2010 ~ as.numeric(year_end), TRUE ~ year),
         # Create iso3 code variable.
         # A lot of warnings will pop up for some surveys done in multiple countries.
         # Ok to keep, we'll filter on study_type and then only merge IDA countries anyway conducted in that country only
         iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(nation == "Kosovo" ~ "XKX", TRUE ~ iso3c),
         instrument_type = "HIES",
         status = "completed",
         source = "https://microdata.worldbank.org/index.php/catalog/lsms") %>%
  # Merge in study type description
  left_join(wb_study_description) %>%
  # Filter for correct repository (lsms, hfps) and addl survey types
  filter(repositoryid %in% c("lsms", "hfps") | study_type %in% c("Income/Expenditure/Household Survey [hh/ies]",
                                                                           "Living Standards Measurement Study",
                                                                           "Living Standards Measurement Study [hh/lsms]",
                                                                           "Socio-Economic/Monitoring Survey [hh/sems]")) %>%
  # Keep relevant variables
  select(country = nation, iso3c, year, instrument_name = title, instrument_type, status, source, study_type, authoring_entity, repositoryid)


# Labor Force Surveys -----------------------------------------------------


# * LFS - ILO -------------------------------------------------------------

# https://ilostat.ilo.org/data/national-sources-catalogue/
# https://www.ilo.org/ilostat-files/Documents/sources_en.csv
lfs <- read_csv("https://www.ilo.org/ilostat-files/Documents/sources_en.csv") %>%
  janitor::clean_names() %>%
  # Filter for where instruments yield information on the labor force
  # But don't include MICS, DHS, or HIES
  filter(str_detect(topics_covered, "Labour force"), !str_detect(source, "Multiple Indicator"),
         !str_detect(source, "Demographic and Health Survey"), !str_detect(source_type, "Household income/expenditure survey"),
         !str_detect(source_type, "Population census")) %>%
  # Clean latest period available variable by splitting into years
  # then only keeping years.
  separate(latest_period_available, into = c("year1", "year2", "year3"), sep = ", ", fill = "right") %>%
  mutate(across(year1:year3, ~as.numeric(str_extract(.x, "^[0-9]{4}"))),
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(country == "Kosovo" ~ "XKX", country == "Netherlands Antilles" ~ "ANT", TRUE ~ iso3c),
         year1 = case_when(
           year2 - year1 == 1 ~ NA_real_,
           TRUE ~ year1
         )) %>%
  ### Adding two additional variables, which might be of value?
  select(country, iso3c, source_type, year1, year2, international_classifications, topics_covered) %>%
  pivot_longer(year1:year2, names_to = "period", values_to = "year") %>%
  filter(!is.na(year)) %>%
  mutate(source = "https://ilostat.ilo.org/data/national-sources-catalogue/", status = "Completed", instrument_type = "Labor Force Survey") %>%
  select(country, iso3c, year, instrument_name = source_type, instrument_type, status, source, international_classifications, topics_covered)
  
  
# with IHSN
# Then if there's a trend in every year, then check NSO site
# Kieran's spreadsheet, follow up


# Agriculture Surveys and Censuses ----------------------------------------


# * Ag Surveys - FAO ------------------------------------------------------

# fam: https://microdata.fao.org/index.php/catalog
# https://microdata.fao.org/index.php/catalog/export/csv?ps=5000&sort_by=popularity&sort_order=desc&collection[]=agriculture-census-surveys&view=s&from=1985&to=2021
# World Census of Agriculture http://www.fao.org/world-census-agriculture/wcarounds/wca2020/countries2020/en/

# First acquire more information on surveytypes from metadata
agri_survey_raw <- fromJSON(content(GET("https://microdata.fao.org/index.php/api/catalog/search?ps=10000"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(api_call = str_c("https://microdata.fao.org/index.php/api/catalog/", id, "?id_format=id"))

## Initialize empty dataset
#all_fao_metadata <- tibble()
#
## Set up progress bar
#n_iter <- length(agri_survey_raw$id)
#pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                     max = n_iter, # Maximum value of the progress bar
#                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                     width = 50,   # Progress bar width. Defaults to getOption("width")
#                     char = "=")   # Character used to create the bar
#
## Loop
#for (i in 1:length(agri_survey_raw$id)){
#  # Set up tibble with basic info
#  study <- tibble(id = agri_survey_raw$id[i], study_type = NA_character_)
#  # Determine length of list generated from API call to filter out
#  # Where we hit error otherwise of subscript being out of bounds
#  study <- study %>%
#    mutate(length = case_when(
#      !is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)),
#      TRUE ~ NA_integer_))
#  # If list is deep enough, proceed to extract data
#  if(study$length > 3){
#    study <- study %>%
#      # Test first whether response is valid based on tunneling into list
#      mutate(study_type = ifelse(
#        is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
#        fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
#      ))
#  }
#  # Append to dataset
#  all_fao_metadata <- all_fao_metadata %>%
#    bind_rows(study)
#  # Insert brief pause in code to not look like a robot to the API
#  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
#  # Increment progress bar
#  setTxtProgressBar(pb, i)
#}
#
#close(pb) # Close the connection
#
## Missing XXX entries for study type because metadata does not have this field
## 'Data kind' is closest equivalent for many of them. Repeat calls and acquire these
#
## Filter to where we are missing study_type.
## Also create url entry as variable so it's easier to read in API call
#all_fao_missing <- all_fao_metadata %>%
#  filter(is.na(study_type)) %>%
#  mutate(api_call = str_c("https://microdata.fao.org/index.php/api/catalog/", id, "?id_format=id"))
#
## Initialize empty dataset
#addl_fao_metadata <- tibble()
#
## Set up progress bar
#n_iter <- length(all_fao_missing$id)
#pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                     max = n_iter, # Maximum value of the progress bar
#                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                     width = 50,   # Progress bar width. Defaults to getOption("width")
#                     char = "=")   # Character used to create the bar
#
## Loop
#for (i in 1:length(all_fao_missing$id)){
#  # Set up tibble with basic info
#  study <- tibble(id = all_fao_missing$id[i], study_type = NA_character_)
#  # Determine length of list generated from API call to filter out
#  # Where we hit error otherwise of subscript being out of bounds
#  study <- study %>%
#    mutate(length = case_when(
#      !is_empty(fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)),
#      TRUE ~ NA_integer_))
#  # If list is deep enough, proceed to extract data
#  if(study$length > 3){
#    study <- study %>%
#      # Test first whether response is valid based on tunneling into list
#      mutate(study_type = ifelse(
#        is_empty(fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
#        fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind
#      ))
#  }
#  # Append to dataset
#  addl_fao_metadata <- addl_fao_metadata %>%
#    bind_rows(study)
#  # Insert brief pause in code to not look like a robot to the API
#  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
#  # Increment progress bar
#  setTxtProgressBar(pb, i)
#}
#
#close(pb) # Close the connection
#
## Combine two metadata sets
#fao_study_description <- all_fao_metadata %>%
#  filter(!is.na(study_type)) %>%
#  mutate(meta_data_field = "Study Type") %>%
#  bind_rows(addl_fao_metadata %>% mutate(meta_data_field = "Data Kind")) %>%
#  select(-length)
#
## Save copy of FAO study descriptions so we don't have to rerun all entries, just new ones
#saveRDS(fao_study_description, file = "Input/fao_microdata_study_description.rds")

# Load study descriptions from saved file
fao_study_description <- readRDS("Input/fao_microdata_study_description.rds")

# Set up final list of Agricultural Surveys
agri_survey <- fromJSON(content(GET("https://microdata.fao.org/index.php/api/catalog/search?ps=10000"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  left_join(fao_study_description) %>%
  mutate(year_end = as.numeric(year_end),
         iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         instrument_type = "Agricultural Survey/Census",
         status = "completed",
         source = "https://microdata.fao.org/index.php/catalog") %>%
  # Filter for study type and repositories
  # Add AGRISurvey info from Tawheeda and LSMS+ Ag modules
  bind_rows(tribble(
    ~title, ~nation, ~iso3c, ~year_end, ~instrument_type, ~status, ~source, ~study_type, ~repositoryid,
    "Encuesta Nacional Agropecuaria", "Costa Rica", "CRI", 2019, "Agricultural Survey/Census", "Completed", "http://www.fao.org/documents/card/en/c/cb3976en", "Agriculture Integrated Survey[AGRISurvey]", "agriculture-census-surveys",
    "AGRISurvey Nation-wide", "Nepal", "NPL", 2020, "Agricultural Survey/Census", "Completed", "http://www.fao.org/in-action/agrisurvey/country-work/nepal/en/", "Agriculture Integrated Survey[AGRISurvey]", "agriculture-census-surveys",
    "Annual Agricultural Survey 2019-2020", "Senegal", "SEN", 2020, "Agricultural Survey/Census", "Completed", "http://www.fao.org/in-action/agrisurvey/country-work/senegal/en/", "Agriculture Integrated Survey[AGRISurvey]", "agriculture-census-surveys",
    "National Panel Survey, 2019-2020", "Uganda", "UGA", 2020, "Agricultural Survey/Census", "Completed", "https://www.worldbank.org/en/programs/lsms/initiatives/lsms-ISA#46", "Living Standards Measurement Study [hh/lsms]", "agriculture-census-surveys"
  )) %>%
  filter(repositoryid == "agriculture-census-surveys", 
         !study_type %in% c("Administrative Records", "Agricultural Census [ag/census]", "Enterprise Census [en/census]", "Population and Housing Census [hh/popcen]"), 
         !str_detect(title, "mpact|roduction")) %>%
  select(country = nation, iso3c, year = year_end, instrument_name = title, instrument_type, status, source, authoring_entity, study_type)


# * Ag Censuses - FAO -----------------------------------------------------

# Upload and clean list of 2020 and 2010 round of ag census. Merge with Ag survey set
ag_census <- read_csv("Input/wca_2020_2010_notes.csv") %>%
  janitor::clean_names() %>%
  mutate(year_clean = case_when(
    str_detect(census_year, "-[0-9]{2}$") ~ str_c("20", str_extract(census_year, "[0-9]{2}$")),
    census_year == "-" ~ NA_character_,
    census_year == "AGRIS since 2018" ~ NA_character_,
    str_detect(census_year, "[0-9]{4}$") ~ str_extract(census_year, "[0-9]{4}$"),
    str_detect(census_year, "\\/[0-9]{2}$") ~ str_c("20", str_extract(census_year, "[0-9]{2}$")),
    TRUE ~ census_year
  ),
  year_clean = as.numeric(year_clean),
  iso3c = countrycode::countrycode(country_link_to_national_census_organisation, "country.name", "iso3c"),
  iso3c = case_when(country_link_to_national_census_organisation == "Netherland" ~ "NLD", TRUE ~ iso3c),
  instrument_name = "Agriculture Census",
  instrument_type = "Agricultural Survey/Census",
  status = "completed",
  source = "http://www.fao.org/world-census-agriculture/wcarounds/en/") %>%
  filter(!is.na(iso3c), !is.na(year_clean), !(census_round == "WCA2020" & iso3c == "MOZ")) %>%
  select(country = starts_with("country"), iso3c, year = year_clean, instrument_name, instrument_type, status, source, census_round)

# IHSN + NSO Survey
# List of LSMS-AG
# Make sure they capture people, not production


# Supplemental surveys - IHSN ---------------------------------------------


# IHSN https://catalog.ihsn.org/catalog
# https://catalog.ihsn.org/catalog/export/csv?ps=10000&collection[]=central
# Import all surveys
ihsn_raw <- fromJSON(content(GET("https://catalog.ihsn.org/index.php/api/catalog/search?ps=10000&from=2000&to=2021"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(
           nation == "Kosovo" ~ "XKX",
           nation == "S?n?gal" ~ "SEN",
           TRUE ~ iso3c
         ),
         status = "Completed", source = "https://catalog.ihsn.org/catalog")

#### To acquire more metadata with which to filter data, we loop individual survey API calls for their metadata
#
## Initialize empty dataset
#all_study_metadata <- tibble()
#
## Set up progress bar
#n_iter <- length(ihsn_raw$id)
#pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                     max = n_iter, # Maximum value of the progress bar
#                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                     width = 50,   # Progress bar width. Defaults to getOption("width")
#                     char = "=")   # Character used to create the bar
#
## Loop
#for (i in 1:length(ihsn_raw$id)){
#  # Create a tibble consisting of the id no of the study in IHSN and the field value for "study type"
#  # To acount for where info on metadata is elsewhere other than nested API call,
#  # we first determine whether API call is valid and then proceed.
#  # As of 2 August 2021, this worked for 6727 out of 7277. Other calls
#  # need to be made for the remaining 550
#  study <- tibble(id = ihsn_raw$id[i])
#  study <- study %>%
#    mutate(study_type = ifelse(
#      is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
#      fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
#    ))
#  # Append to dataset
#  all_study_metadata <- all_study_metadata %>%
#    bind_rows(study)
#  # Insert brief pause in code to not look like a robot to the API
#  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
#  # Increment progress bar
#  setTxtProgressBar(pb, i)
#}
#
#close(pb) # Close the connection
#
## Missing 550 entries for study type because metadata does not have this field
## 'Data kind' is closest equivalent for many of them. Repeat calls and acquire these
#
## Filter to where we are missing study_type. Also, id study 8478 shows up in listing but has no information. Probably a duplicate.
## its title is the same as another entry https://catalog.ihsn.org/catalog/8512
## Also create url entry as variable so it's easier to read in API call
#all_study_missing <- all_study_metadata %>%
#  filter(is.na(study_type), id != 8478) %>%
#  mutate(url_list = str_c("https://catalog.ihsn.org/index.php/api/catalog/", id, "?id_format=id"))
#
## Initialize empty dataset
#all_missing_metadata <- tibble()
#
## Set up progress bar
#n_iter <- length(all_study_missing$id)
#pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                     max = n_iter, # Maximum value of the progress bar
#                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                     width = 50,   # Progress bar width. Defaults to getOption("width")
#                     char = "=")   # Character used to create the bar
#
#for (i in 1:length(all_study_missing$id)){
#  study <- tibble(id = all_study_missing$id[i])
#  study <- study %>%
#    mutate(study_type = ifelse(
#      is_empty(fromJSON(content(GET(all_study_missing$url_list[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
#      fromJSON(content(GET(all_study_missing$url_list[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind
#      ))
#  # Append to dataset
#  all_missing_metadata <- all_missing_metadata %>%
#    bind_rows(study)
#  # Insert brief pause in code to not look like a robot to the API
#  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
#  # Increment progress bar
#  setTxtProgressBar(pb, i)
#}
#
#close(pb) # Close the connection
#
## Merge with other df of metadata tags
#study_description <- all_study_metadata %>%
#  filter(!is.na(study_type)) %>%
#  mutate(meta_data_field = "Study Type") %>%
#  bind_rows(all_missing_metadata %>% mutate(meta_data_field = "Data Kind"))
#
## Save copy of WB study descriptions so we don't have to rerun all entries, just new ones
#saveRDS(study_description, file = "Input/ihsn_microdata_study_description.rds")
#
## Load study descriptions from saved file
ihsn_study_description <- readRDS("Input/ihsn_microdata_study_description.rds")

# Final list of IHSN, to be used to supplement other survey groups as appropriate
ihsn <- ihsn_raw %>%
  left_join(ihsn_study_description) %>%
  # drop entirely irrelevant surveys
  filter(!study_type %in% c("Registros administrativos, otros (ad/oth]",
                            "Independent Performance Evaluation",
                            "Impact Evaluation Study",
                            "Public Opinion Survey [ind/pos]",
                            "Public Opinion Survey",
                            "Public Opinion Survey [po]]",
                            "Impact Evaluation",
                            "Impact Evaluation Survey",
                            "Independent Impact Evaluation",
                            "Administrative Records, Education (ad/edu]",
                            "Industrial Statistics",
                            "Impact Evaluation Study [ie/ies]",
                            "Administrative Records, Other (ad/oth]",
                            "Enterprise Survey",
                            "Price Survey [hh/prc]",
                            "Encuesta de empresas [en/oth]",
                            "Encuesta de Buenas Pr?cticas Ambientales",
                            "Donn?es administrative, sant? [ad/hea]",
                            "Enqu?te sur les entreprises [en/oth]",
                            "Estad?stica de precios",
                            "Estad?stica del Sector El?ctrico",
                            "Encuesta",
                            "Establishment Census [en/census]",
                            "Registros Administrativos",
                            "Administrative, Trade",
                            "Administrative Records, Other (ad/oth]- import and export documents",
                            "Administrative Records, Other [ad/oth]",
                            "Country Opinion Survey",
                            "Pay For Performance (P4P) - Tanzania",
                            "Service Provision Assessments",
                            "Performance Evaluation",
                            "Service Delivery Indicators Survey (SDI)",
                            "Other Enterprise Survey",
                            "Country Opnion Survey",
                            "Administrative records data [adm]",
                            "Clinical data [cli]",
                            "Process-produced data [pro]",
                            "Event/Transaction data [evn]",
                            "Other",
                            "Sample survey data [ssd], Administrative records data [adm], other",
                            "Event/transaction data [evn]"
  ))

# Time Use Surveys - UNSD -------------------------------------------------

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


# Combine -----------------------------------------------------------------

### old code - only works if we have the same variables for each tibble we're binding together
# all_surveys_census <- dhs %>%
#   select(country, iso3c, year, status, instrument_name, instrument_type, source) %>%
#   bind_rows(mics %>% select(-country_original)) %>%
#   bind_rows(lsms) %>%
#   bind_rows(lfs) %>%
#   bind_rows(agri_survey) %>%
#   bind_rows(ag_census) %>%
#   bind_rows(tus) %>%
#   bind_rows(census)
# Can add IHSN to each of the above to further add surveys

### New code - select the same variables across list of all data frames, and bind rows
all_surveys_census <- list(dhs, mics, lsms, lfs, agri_survey, ag_census, tus, census) |> 
  map(~select(., country, iso3c, year, status, instrument_name, instrument_type, source)) |> 
  map_dfr(bind_rows)
  
##### FILTER FOR OGDI YEARS #####
# 2013-2022 / 2012/13-2021/22 for non-calendar years
dhs_clean <- dhs |> filter(year>=2013 & year<=2022)
mics_clean <- mics |> filter(year>=2013 & year<=2022)
lsms_clean <- lsms |> filter(year>=2013 & year<=2022)
lfs_clean <- lfs |> filter(year>=2013 & year<=2022)
agri_survey_clean <- agri_survey |> filter(year>=2013 & year<=2022)
tus_clean <- tus |> filter(year>=2013 & year<=2022)
dhs_clean <- dhs |> filter(year>=2013 & year<=2022)
ihsn_clean <- ihsn |> filter((year_start == 2012 & year_end >=2013) | year_start>=2013) |> 
  mutate(year = case_when(year_start==year_end ~ as.character(year_start),
                          year_start!=year_end & year_end-year_start==1 ~ paste0(year_start, "/", str_extract(year_end, "\\d{2}$")),
                          year_start!=year_end & year_end-year_start>1 ~ paste0(year_start, "-", year_end)))

# filter for census years
ag_census_clean <- ag_census |> 
  mutate(census_round = str_remove(census_round, "WCA")) |> 
  filter(census_round==2010|census_round==2020)

# years based on https://unstats.un.org/unsd/demographic-social/census/censusdates/
census_clean <- census |> 
  mutate(census_round = str_remove(census_round, "round")) |> 
  filter(census_round==2010|census_round==2020)

# try to flag agricultural census/survey rows where we need further investigation
# to determine if they capture human level-data
agri_survey_clean <- agri_survey_clean |> 
  mutate(household_flag = ifelse(str_detect(instrument_name, "Household|Living|Socio|Gender|Women"), TRUE, FALSE)) |> 
  # filter out rows where authoring entity is Syngenta. Need explicitly to keep those that are missing as well
  # doesn't appear that government agencies are involved in these
  filter(authoring_entity!="Syngenta" | is.na(authoring_entity))
  

### Combining all the FILTERED datasets, with census round variable
all_surveys_census_filtered <- list(dhs_clean, mics_clean, lsms_clean, lfs_clean, 
                                    agri_survey_clean, tus_clean) |> 
  map(~select(., country, iso3c, year, status, instrument_name, instrument_type, source)) |> 
  map_dfr(bind_rows) |> 
  bind_rows(census_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, census_round)) |>
  bind_rows(ag_census_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, census_round))

xlsx::write.xlsx(all_surveys_census_filtered, "Output/instrument_inventory_filtered.xlsx")

###### EXPORT XLSX FILES OF EACH CLEANED SURVEY INSTRUMENT DATA ######
xlsx::write.xlsx(dhs_clean, "Output/dhs.xlsx")
xlsx::write.xlsx(mics_clean, "Output/mics.xlsx")
xlsx::write.xlsx(lsms_clean, "Output/lsms.xlsx")
xlsx::write.xlsx(lfs_clean, "Output/lfs.xlsx")
xlsx::write.xlsx(agri_survey_clean, "Output/agri_survey.xlsx")
xlsx::write.xlsx(ag_census_clean, "Output/ag_census.xlsx")
xlsx::write.xlsx(tus_clean, "Output/tus.xlsx")
xlsx::write.xlsx(census_clean, "Output/census.xlsx")
xlsx::write.xlsx(ihsn_clean, "Output/ihsn.xlsx")

### Looking at how many rows have only one authoring entity, and it is an IGO like hte UNHCR or World Bank
# distinct authoring entities that are only an IGO
ihsn_clean |> select(authoring_entity) |> distinct() |> 
  filter(!str_detect(authoring_entity, ",") 
         & str_detect(authoring_entity, "UN|United Nations|FAO|World Bank"))

lsms_clean |> select(authoring_entity) |> distinct() |> 
  filter(!str_detect(authoring_entity, ",") 
         & str_detect(authoring_entity, "UN|United Nations|FAO|World Bank"))

# number of rows
ihsn_clean |> filter(!str_detect(authoring_entity, ",") 
                     & str_detect(authoring_entity, "UN|United Nations|FAO|World Bank")) |> 
  nrow()

lsms_clean |> filter(!str_detect(authoring_entity, ",") 
                     & str_detect(authoring_entity, "UN|United Nations|FAO|World Bank")) |> 
  nrow()

### trying to compute proportion of surveys completed
# all_surveys_census |> 
#   filter(year<=2015 | year>=2019) |> 
#   group_by(instrument_type, status) |> 
#   summarise(n_expected = n()) |> 
#   filter(status=="Completed") |> 
#   select(-source) |> 
#   summarise(n_completed = n(),
#             n_expected) |> 
#   summarise(prop_complete = 100*(n_completed/n_expected))

# Export
### NOTE - I am modifying this code with an if-else to allow country names that do not have an iso3c code to be maintained (there are 6 such rows)
### original code meant they were lost (5 of 6 impacted rows are attributed to multiple countries, and 1 attributed to "Pacific Region")
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

# Mock timeline test
all_surveys_census %>% 
  filter(iso3c == "GHA", year>=2010) %>% 
  ggplot(aes(x = year)) + 
  geom_dotplot()

# Adding instrument labels to points
all_surveys_census %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c, year) %>% 
  # Create y coordinate for instrument
  mutate(n = row_number(year)) %>% 
  ungroup() %>%
  # Filter for country
  filter(iso3c == "GHA", year>=2010) %>% 
  ggplot(aes(x = year, y = n, label = instrument_type)) + 
  geom_point() + 
  ggrepel::geom_text_repel() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) 
  

# Administrative Instruments ----------------------------------------------

#### Administrative Instruments - TO BE DEVELOPED, BUT NOT FOR TIMELINE FEATURE
#### CRVS - UNICEF via World Bank
# https://data.worldbank.org/indicator/SP.REG.BRTH.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.MA.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.FE.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.UR.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.RU.ZS

#### HMIS - ODIN

#### EMIS - ODIN
