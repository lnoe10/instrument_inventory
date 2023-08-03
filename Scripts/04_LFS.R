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

# filter for OGDI years of interest based on year end
lfs_clean <- lfs |> filter(year_end >= 2013)

# export filtered and full datasets
#xlsx::write.xlsx(lfs_clean, "Output/lfs_ogdi_yrs.xlsx")
#xlsx::write.xlsx(lfs, "Output/lfs_all_yrs.xlsx")

#### comparing above to data from https://ilostat.ilo.org/data/national-sources-catalogue/ ####
sources_en <- read_csv("~/Documents/ODW/sources_en.csv", show_col_types = F) |> filter(`Source type`=="Labour force survey")

# separating rows on years b/c a number have multiple years
sources_en <- sources_en |> select(country = Country, source = Source, year = `Latest period available`) |> 
  separate_rows(year, sep=", ") |> 
  mutate(year = str_extract(year, "^\\d{4}"))

# left join, filter where there is a match, export to xlsx
lfs |> select(country, year, instrument_name) |> left_join(sources_en, by=c("year", "country")) |> filter(!is.na(source)) |> 
  rename(source_catalog_name = source) |> 
  arrange(country) |> View()


# https://ilostat.ilo.org/data/national-sources-catalogue/
# https://www.ilo.org/ilostat-files/Documents/sources_en.csv
# lfs <- read_csv("https://www.ilo.org/ilostat-files/Documents/sources_en.csv") %>%
#   janitor::clean_names() %>%
#   # Filter for where instruments yield information on the labor force
#   # But don't include MICS, DHS, or HIES
#   filter(str_detect(topics_covered, "Labour force"), !str_detect(source, "Multiple Indicator"),
#          !str_detect(source, "Demographic and Health Survey"), !str_detect(source_type, "Household income/expenditure survey"),
#          !str_detect(source_type, "Population census")) %>%
#   # Clean latest period available variable by splitting into years
#   # then only keeping years.
#   separate(latest_period_available, into = c("year1", "year2", "year3"), sep = ", ", fill = "right") %>%
#   mutate(across(year1:year3, ~as.numeric(str_extract(.x, "^[0-9]{4}"))),
#          iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
#          iso3c = case_when(country == "Kosovo" ~ "XKX", country == "Netherlands Antilles" ~ "ANT", TRUE ~ iso3c),
#          year1 = case_when(
#            year2 - year1 == 1 ~ NA_real_,
#            TRUE ~ year1
#          )) %>%
#   ### Adding two additional variables, which might be of value?
#   select(country, iso3c, source_type, year1, year2, international_classifications, topics_covered) %>%
#   pivot_longer(year1:year2, names_to = "period", values_to = "year") %>%
#   filter(!is.na(year)) %>%
#   mutate(source = "https://ilostat.ilo.org/data/national-sources-catalogue/", status = "Completed", instrument_type = "Labor Force Survey") %>%
#   select(country, iso3c, year, instrument_name = source_type, instrument_type, status, source, international_classifications, topics_covered)