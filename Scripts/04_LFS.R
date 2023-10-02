### script to scrape Labor Force Survey data from the ILO

library(httr)
library(jsonlite)
library(tidyverse)

# scrape data from API
lfs <- fromJSON(content(GET("https://www.ilo.org/surveyLib/index.php/api/catalog/search", 
                            query=list(ps=10000)), "text"))$result$rows |> 
  as_tibble() |> 
  # filter for only LFS
  filter(repo_title=="Labour force surveys") |> 
  rename(country = nation,
         instrument_type = repo_title,
         instrument_name = title,
         source = url) |> 
  # add country code var, clean up parsing issues with accents
  mutate(status = "Completed",
         year = ifelse(year_start==year_end, year_end, paste0(year_start, "-", year_end)),
         country = ifelse(country=="TÃ¼rkiye", "Turkey", country),
         country = ifelse(country=="CÃ´te d'Ivoire", "Côte d'Ivoire", country),
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(country == "Kosovo" ~ "XKX", country == "Netherlands Antilles" ~ "ANT", TRUE ~ iso3c)) |> 
  select(country, iso3c, year_start, year_end, year, instrument_name, instrument_type, authoring_entity, status, source, id, type, idno)

# comparing above to data from https://ilostat.ilo.org/data/national-sources-catalogue/
sources_en <- read_csv("~/Documents/ODW/sources_en.csv", show_col_types = F) |> filter(`Source type`=="Labour force survey")

# taking latest year recorded
sources_en <- sources_en |> select(country = Country, source = Source, `Latest period available`, instrument_type=`Source type`) |> 
  mutate(year = ifelse(str_detect(`Latest period available`, ", "), 
                       str_extract(`Latest period available`, "(?<=, )\\d{4}"),
                       str_extract(`Latest period available`, "^\\d{4}")),
         iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(country == "Kosovo" ~ "XKX", country == "Netherlands Antilles" ~ "ANT", TRUE ~ iso3c)) |> 
  select(-`Latest period available`)

# left join, filter where there is a match, flag matching instrument names, export to xlsx
lfs_country_yr_matches <- lfs |> select(country, year, instrument_name) |> left_join(sources_en, by=c("year", "country")) |> filter(!is.na(source)) |> 
  rename(source_catalog_name = source) |> 
  arrange(country) |> 
  mutate(duplicate = case_when(str_detect(str_to_title(instrument_name), "Lab(o|ou)r Force Survey") & 
                                 str_detect(str_to_title(source_catalog_name), "Lab(o|ou)r Force Survey") ~ 1,
                               str_detect(str_to_title(instrument_name), "Employment, Unemployment Survey") &
                                 str_detect(str_to_title(source_catalog_name), "Employment, Unemployment Survey") ~ 1,
                               str_detect(str_to_title(instrument_name), "National Household Survey") &
                                 str_detect(str_to_title(source_catalog_name), "National Household Survey") ~ 1,
                               str_detect(str_to_title(instrument_name), "Labor Market Panel Survey") &
                                 str_detect(str_to_title(source_catalog_name), "Labor Market Panel Survey") ~ 1,
                               str_detect(str_to_title(instrument_name), "Continuous Multi-Purpose Household Survey") &
                                 str_detect(str_to_title(source_catalog_name), "Continuous Multi-Purpose Household Survey") ~ 1,
                               str_detect(str_to_title(instrument_name), "Continuous Sample Survey of the Population") &
                                 str_detect(str_to_title(source_catalog_name), "Continuous Sample Survey of the Population") ~ 1,
                               .default = 0))

# export country-year matches to be checked in Google sheets
# xlsx::write.xlsx(lfs_country_yr_matches, "Output/lfs_country_yr_matches.xlsx")

# read in checked country-year matches
lfs_country_yr_matches_checked <- read_csv("Output/misc_data/lfs_country_yr_matches_checked.csv", show_col_types = F)

# rename variables to match LFS dataframe
sources_en <- sources_en |> rename(instrument_name = source) |> mutate(source = "https://ilostat.ilo.org/data/national-sources-catalogue/")

# add surveys that did not match to countries found in the API data
lfs_all <- lfs |> bind_rows(sources_en |> filter(!(iso3c %in% lfs$iso3c)))

# filter remaining source catalogue data that had country-year combos which were not found in ILO API data, add to lfs_all
lfs_all <- sources_en |> filter(iso3c %in% lfs$iso3c) |> 
  # left joining with lfs_country_yr_matches so we can isolate those that did NOT appear in this df
  left_join(lfs_country_yr_matches |> select(-instrument_name) |> 
              rename(instrument_name=source_catalog_name) |> distinct(),
            by=c("country", "year", "instrument_name", "iso3c", "instrument_type")) |> 
  # filtering for those rows that do not belong in the lfs_country_yr_matches df, all of which we manually checked
  # and are in fact not duplicates
  # do so by filtering for those w/ missing duplicate value (all of which are 1 or 0 in lfs_country_yr_matches df)
  filter(is.na(duplicate)) |> 
  select(-duplicate) %>%
  bind_rows(lfs_all, .)

# add in the only one from country year match that we deemed not to be a duplicate
# Korea's Survey on Immigrants' Living Conditions and Labour Force from 2021
lfs_all <- bind_rows(lfs_all, sources_en |> filter(country=="Korea, Republic of" & year=="2021")) |> mutate(year = as.numeric(year))

# export full dataset
xlsx::write.xlsx(lfs_all, "Output/instrument_data_all_years/lfs.xlsx")
