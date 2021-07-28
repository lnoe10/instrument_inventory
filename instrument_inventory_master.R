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
  select(country, year, status, notes = round) 

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
              )) %>%
              select(-id)) %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           # Papua is a province of Indonesia != Papua New Guinea country
           str_detect(country, "Papua Selected Districts") ~ "IDN",
           str_detect(country, "Kosovo") ~ "XKX",
           country == "Lebanon (Palestinians)" ~ "LBN",
           # This version of MICS was done in the South of Sudan in the regions
           # That now define the present-day country, however, at the time, it was not an independent country
           country == "Sudan (South)" ~ "SDN",
           str_detect(country, "Syrian Arab Republic") ~ "SYR",
           # This version of MICS was done in Sudan, which at the time had part of
           # South Sudan as part of the geographic coverage.
           str_detect(country, "including current South Sudan") ~ "SDN",
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
  # Keep 1 instance of all the cases where multiple surveys were conducted per year
  # in a country. Mostly sub-regions vs overall, or several sub-regions, which we're crediting to the whole country
  distinct(iso3c, year, .keep_all = TRUE) %>%
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
  filter(repositoryid == "lsms")

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


# IHSN + NSO Survey
# List of LSMS-AG
# Make sure they capture people, not production

#### Supplemental survey - IHSN ####
# IHSN https://catalog.ihsn.org/catalog
# https://catalog.ihsn.org/catalog/export/csv?ps=10000&collection[]=central
ihsn <- fromJSON(content(GET("https://catalog.ihsn.org/index.php/api/catalog/search?ps=10000&from=2000&to=2021"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(
           nation == "Kosovo" ~ "XKX",
           nation == "Sénégal" ~ "SEN",
           TRUE ~ iso3c
         ),
         status = "Completed", source = "https://catalog.ihsn.org/catalog") %>%
  # Keep only 2010 onwards, keep only countries with country codes, drop all censuses
  filter(year_end >= 2010, !is.na(iso3c), !str_detect(title, "(C|c)ensus")) %>%
  select(country = nation, iso3c, year = year_end, instrument_name = title, instrument_type = repositoryid, status, source)

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
