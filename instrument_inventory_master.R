#### ALL SURVEY INSTRUMENT INVENTORY ####
library(httr)
library(jsonlite)
library(tidyverse)

#### MICS - UNICEF ####
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
  # Choosing final list of instruments
  select(country = country_clean, iso3c, year, status, instrument_name, instrument_type, source, country_original = country)

#### DHS - USAID ####
# https://dhsprogram.com/Methodology/survey-search.cfm?pgtype=main&SrvyTp=year#
# Completed and ongoing DHS. Was short of one, Guatemala in 2022
# API https://api.dhsprogram.com/rest/dhs/surveys?surveyStatus=all&f=html
dhs <- fromJSON(content(GET("https://api.dhsprogram.com/rest/dhs/surveys?surveyStatus=all"), "text"))$Data %>%
  as_tibble() %>%
  add_row(SurveyType = "DHS", SurveyYearLabel = "2022", SurveyYear = "2022", DHS_CountryCode = "GU",
          CountryName = "Guatemala", SubregionName = "Central America", SurveyStatus = "Ongoing", RegionName = "Latin America & Caribbean") %>%
  janitor::clean_names() %>%
  # Filtering out Other surveys, Service Provision Assessments (SPA) and Special DHS, as the latter is mostly conducted at the sub-national level
  # Need to dig a little deeper into Other Surveys, as there are some that definitely don't fit, like
  # MICS duplicates, but others might, such as Egypt 2015
  filter(survey_type %in% c("AIS", "DHS", "MIS"))
  select(country = country_name, year = survey_year, survey_type, status = survey_status) %>%
  mutate(instrument_type = "Household health survey", source = "https://dhsprogram.com/Methodology/survey-search.cfm?pgtype=main&SrvyTp=year#",
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         year = as.numeric(year),
         country_clean = countrycode::countrycode(iso3c, "iso3c", "country.name")) %>%
  select(country = country_clean, iso3c, year, status, instrument_name = survey_type, instrument_type, source, country_original = country)


#### LSMS - World Bank ####
# https://microdata.worldbank.org/index.php/catalog/lsms
# https://microdata.worldbank.org/index.php/api/catalog/search?ps=10000
#^ filter for LSMS from there.
lsms_raw <- fromJSON(content(GET("https://microdata.worldbank.org/index.php/api/catalog/search?ps=10000"), "text"))$result$rows %>%
  as_tibble() %>%
  mutate(api_call = str_c("https://microdata.worldbank.org/index.php/api/catalog/", id, "?id_format=id"))

# First acquire more information on surveytypes from metadata  

# Initialize empty dataset
all_wb_metadata <- tibble()

# Set up progress bar
n_iter <- length(lsms_raw$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(lsms_raw$id)){
  # Set up tibble with basic info
  study <- tibble(id = lsms_raw$id[i], study_type = NA_character_)
  # Determine length of list generated from API call to filter out
  # Where we hit error otherwise of subscript being out of bounds
  study <- study %>%
    mutate(length = case_when(
      !is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)),
      TRUE ~ NA_integer_))
  # If list is deep enough, proceed to extract data
  if(study$length > 3){
    study <- study %>%
      # Test first whether response is valid based on tunneling into list
      mutate(study_type = ifelse(
        is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
        fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
      ))
  }
  # Append to dataset
  all_wb_metadata <- all_wb_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# Missing 539 entries for study type because metadata does not have this field
# 'Data kind' is closest equivalent for many of them. Repeat calls and acquire these

# Filter to where we are missing study_type.
# Also create url entry as variable so it's easier to read in API call
all_wb_missing <- all_wb_metadata %>%
  filter(is.na(study_type)) %>%
  mutate(api_call = str_c("https://microdata.worldbank.org/index.php/api/catalog/", id, "?id_format=id"))

# Initialize empty dataset
addl_wb_metadata <- tibble()

# Set up progress bar
n_iter <- length(all_wb_missing$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(all_wb_missing$id)){
  # Set up tibble with basic info
  study <- tibble(id = all_wb_missing$id[i], study_type = NA_character_)
  # Determine length of list generated from API call to filter out
  # Where we hit error otherwise of subscript being out of bounds
  study <- study %>%
    mutate(length = case_when(
      !is_empty(fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)),
      TRUE ~ NA_integer_))
  # If list is deep enough, proceed to extract data
  if(study$length > 3){
    study <- study %>%
      # Test first whether response is valid based on tunneling into list
      mutate(study_type = ifelse(
        is_empty(fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
        fromJSON(content(GET(all_wb_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
      ))
  }
  # Append to dataset
  addl_wb_metadata <- addl_wb_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection





#### TO BE FIXED
lsms <- lsms_raw %>%
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
         iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(nation == "Kosovo" ~ "XKX", TRUE ~ iso3c),
         instrument_type = "HIES",
         status = "completed",
         source = "https://microdata.worldbank.org/index.php/catalog/lsms") %>%
  # Keep only one survey per year to count.
  distinct(iso3c, year, .keep_all = TRUE) %>%
  select(country = nation, iso3c, year, instrument_name = title, instrument_type, status, source)


# With IHSN

#### LFS - ILO ####
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
  select(country, iso3c, source_type, year1, year2) %>%
  pivot_longer(year1:year2, names_to = "period", values_to = "year") %>%
  filter(!is.na(year)) %>%
  mutate(source = "https://ilostat.ilo.org/data/national-sources-catalogue/", status = "Completed", instrument_type = "Labor Force Survey") %>%
  select(country, iso3c, year, instrument_name = source_type, instrument_type, status, source)
  
  
# with IHSN
# Then if there's a trend in every year, then check NSO site
# Kieran's spreadsheet, follow up

#### AG Surveys and Censuses - FAO ####
# fam: https://microdata.fao.org/index.php/catalog
# https://microdata.fao.org/index.php/catalog/export/csv?ps=5000&sort_by=popularity&sort_order=desc&collection[]=agriculture-census-surveys&view=s&from=1985&to=2021
# World Census of Agriculture http://www.fao.org/world-census-agriculture/wcarounds/wca2020/countries2020/en/
agri_survey <- fromJSON(content(GET("https://microdata.fao.org/index.php/api/catalog/search?ps=10000"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  filter(repositoryid == "agriculture-census-surveys", str_detect(title, "gricul"), !str_detect(title, "mpact|roduction")) %>%
  mutate(year = as.numeric(str_extract(idno, "[0-9]{4}")),
         iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         instrument_type = "Agricultural Survey/Census",
         status = "completed",
         source = "https://microdata.fao.org/index.php/catalog") %>%
  filter((nation!="Philippines")) %>%
  # ^ THIS DROPS ALL OF Philippines, because both surveys/census are only production or too sub-regional
  # Check if Philippines has other surveys/censuses that would qualify
  distinct(nation, year, .keep_all = TRUE) %>%
  select(country = nation, iso3c, year, instrument_name = title, instrument_type, status, source)


agri_survey_raw <- fromJSON(content(GET("https://microdata.fao.org/index.php/api/catalog/search?ps=10000"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(api_call = str_c("https://microdata.fao.org/index.php/api/catalog/", id, "?id_format=id"))

# First acquire more information on surveytypes from metadata  

# Initialize empty dataset
all_fao_metadata <- tibble()

# Set up progress bar
n_iter <- length(agri_survey_raw$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(agri_survey_raw$id)){
  # Set up tibble with basic info
  study <- tibble(id = agri_survey_raw$id[i], study_type = NA_character_)
  # Determine length of list generated from API call to filter out
  # Where we hit error otherwise of subscript being out of bounds
  study <- study %>%
    mutate(length = case_when(
      !is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)),
      TRUE ~ NA_integer_))
  # If list is deep enough, proceed to extract data
  if(study$length > 3){
    study <- study %>%
      # Test first whether response is valid based on tunneling into list
      mutate(study_type = ifelse(
        is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
        fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
      ))
  }
  # Append to dataset
  all_fao_metadata <- all_fao_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# Missing XXX entries for study type because metadata does not have this field
# 'Data kind' is closest equivalent for many of them. Repeat calls and acquire these

# Filter to where we are missing study_type.
# Also create url entry as variable so it's easier to read in API call
all_fao_missing <- all_fao_metadata %>%
  filter(is.na(study_type)) %>%
  mutate(api_call = str_c("https://microdata.fao.org/index.php/api/catalog/", id, "?id_format=id"))

# Initialize empty dataset
addl_fao_metadata <- tibble()

# Set up progress bar
n_iter <- length(all_fao_missing$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(all_fao_missing$id)){
  # Set up tibble with basic info
  study <- tibble(id = all_fao_missing$id[i], study_type = NA_character_)
  # Determine length of list generated from API call to filter out
  # Where we hit error otherwise of subscript being out of bounds
  study <- study %>%
    mutate(length = case_when(
      !is_empty(fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)),
      TRUE ~ NA_integer_))
  # If list is deep enough, proceed to extract data
  if(study$length > 3){
    study <- study %>%
      # Test first whether response is valid based on tunneling into list
      mutate(study_type = ifelse(
        is_empty(fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
        fromJSON(content(GET(all_fao_missing$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
      ))
  }
  # Append to dataset
  addl_fao_metadata <- addl_fao_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection


# IHSN + NSO Survey
# List of LSMS-AG
# Make sure they capture people, not production

#### Supplemental survey - IHSN ####
# IHSN https://catalog.ihsn.org/catalog
# https://catalog.ihsn.org/catalog/export/csv?ps=10000&collection[]=central
# Import all surveys
ihsn <- fromJSON(content(GET("https://catalog.ihsn.org/index.php/api/catalog/search?ps=10000&from=2000&to=2021"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(
           nation == "Kosovo" ~ "XKX",
           nation == "Sénégal" ~ "SEN",
           TRUE ~ iso3c
         ),
         status = "Completed", source = "https://catalog.ihsn.org/catalog")
  # select(country = nation, iso3c, year = year_end, instrument_name = title, instrument_type = repositoryid, status, source)

### To acquire more metadata with which to filter data, we loop individual survey API calls for their metadata

# Initialize empty dataset
all_study_metadata <- tibble()

# Set up progress bar
n_iter <- length(ihsn$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(ihsn$id)){
  # Create a tibble consisting of the id no of the study in IHSN and the field value for "study type"
  # To acount for where info on metadata is elsewhere other than nested API call,
  # we first determine whether API call is valid and then proceed.
  # As of 2 August 2021, this worked for 6727 out of 7277. Other calls
  # need to be made for the remaining 550
  study <- tibble(id = ihsn$id[i])
  study <- study %>%
    mutate(study_type = ifelse(
      is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
      fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name
    ))
  # Append to dataset
  all_study_metadata <- all_study_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# Missing 550 entries for study type because metadata does not have this field
# 'Data kind' is closest equivalent for many of them. Repeat calls and acquire these

# Filter to where we are missing study_type. Also, id study 8478 shows up in listing but has no information. Probably a duplicate.
# its title is the same as another entry https://catalog.ihsn.org/catalog/8512
# Also create url entry as variable so it's easier to read in API call
all_study_missing <- all_study_metadata %>%
  filter(is.na(study_type), id != 8478) %>%
  mutate(url_list = str_c("https://catalog.ihsn.org/index.php/api/catalog/", id, "?id_format=id"))

# Initialize empty dataset
all_missing_metadata <- tibble()

# Set up progress bar
n_iter <- length(all_study_missing$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for (i in 1:length(all_study_missing$id)){
  study <- tibble(id = all_study_missing$id[i])
  study <- study %>%
    mutate(study_type = ifelse(
      is_empty(fromJSON(content(GET(all_study_missing$url_list[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
      fromJSON(content(GET(all_study_missing$url_list[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind
      ))
  # Append to dataset
  all_missing_metadata <- all_missing_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# Merge with other df of metadata tags
study_description <- all_study_metadata %>%
  filter(!is.na(study_type)) %>%
  mutate(meta_data_field = "Study Type") %>%
  bind_rows(all_missing_metadata %>% mutate(meta_data_field = "Data Kind"))

# To Do: merge back with main IHSN and filter appropriately. Discuss with Tawheeda
# ABout 64  remaining with no relevant field. Will have to filter on other metadata.

#### Time use - UNSD ####
# https://unstats.un.org/unsd/gender/timeuse and manual ODW check of NSO websites
tus <- read_csv("C:/Users/lnoe/Documents/GitHub/instrument_inventory/Input/time_use_surveys_sgdf_inventory_2020.csv") %>%
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

#### Census - UNSD ####
# See scraping census dates file
# Census dates scrape.R
census <- df %>%
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
  select(country, iso3c, year, status = planned, instrument_name, instrument_type, source)

#### COMBINE SURVEY INSTRUMENTS ####
all_surveys <- dhs %>%
  bind_rows(mics) %>%
  bind_rows(lsms) %>%
  bind_rows(lfs) %>%
  bind_rows(agri_survey) %>%
  bind_rows(tus) %>%
  bind_rows(census) %>%
  bind_rows(ihsn)

# Mock timeline test
all_surveys %>% 
  filter(iso3c == "GHA", year>=2010) %>% 
  ggplot(aes(x = year)) + 
  geom_dotplot()

# Adding instrument labels to points
all_surveys %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c, year) %>% 
  # Create y coordinate for instrument
  mutate(n = row_number(year)) %>% 
  ungroup() %>%
  # Filter for country
  filter(iso3c == "BGD", year>=2010) %>% 
  ggplot(aes(x = year, y = n, label = instrument_name)) + 
  geom_point() + 
  ggrepel::geom_text_repel() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) 
  

#### Administrative Instruments - TO BE DEVELOPED, BUT NOT FOR TIMELINE FEATURE ####
#### CRVS - UNICEF via World Bank ####
# https://data.worldbank.org/indicator/SP.REG.BRTH.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.MA.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.FE.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.UR.ZS
# https://data.worldbank.org/indicator/SP.REG.BRTH.RU.ZS

#### HMIS - ODIN ####

#### EMIS - ODIN ####
