### script to get Labour Force Survey data from ILOSTAT

# The ILO Survey Catalogue (webapps.ilo.org/surveyLib, NADA) used until 2025 was retired in 2026
# (all URLs redirect to "410 Gone | ILO Survey Catalog Retired").
# Labour force surveys now come from ILOSTAT: every annual observation is tagged with the survey
# it comes from (source.label, e.g. "LFS - EU Labour Force Survey"). One row per country, LFS source
# and year in which ILOSTAT has data from that survey. Quarterly/monthly rounds of the same survey
# in a year therefore count once, as one annual LFS.

library(httr)
library(tidyverse)

# Annual indicators that are mostly derived from labour force surveys. A survey-year counts if it
# appears as source for any of them.
ilostat_indicators <- c(
  "EAP_TEAP_SEX_AGE_NB_A",  # labour force by sex and age
  "EMP_TEMP_SEX_AGE_NB_A",  # employment by sex and age
  "UNE_DEAP_SEX_AGE_RT_A"   # unemployment rate by sex and age
)

get_ilostat_sources <- function(id) {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  r <- GET(str_c("https://rplumber.ilo.org/data/indicator/?id=", id, "&lang=en&type=label&format=.csv&channel=ilostat"),
           write_disk(f, overwrite = TRUE), timeout(600))
  stop_for_status(r)
  read_csv(f, show_col_types = FALSE, col_select = c(ref_area.label, source.label, time)) |>
    distinct() |>
    mutate(indicator = id)
}

ilostat_sources <- map_dfr(ilostat_indicators, get_ilostat_sources)

lfs_all <- ilostat_sources |>
  # source type is the prefix of the source label: LFS, HIES, HS (other household survey), PC (census), ...
  mutate(source_type = str_extract(source.label, "^[A-Z]+(?= - )")) |>
  filter(source_type == "LFS") |>
  group_by(country = ref_area.label, survey = str_remove(source.label, "^LFS - "), year = as.numeric(time)) |>
  summarise(indicators = str_c(sort(unique(indicator)), collapse = ", "), .groups = "drop") |>
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c", warn = FALSE),
         iso3c = case_when(country == "Kosovo" ~ "XKX", country == "Netherlands Antilles" ~ "ANT", TRUE ~ iso3c)) |>
  # drop regional aggregates
  filter(!is.na(iso3c)) |>
  mutate(year_start = year,
         year_end = year,
         instrument_name = str_c(survey, " ", year),
         instrument_type = "Labour Force Survey",
         authoring_entity = NA_character_,
         status = "Completed",
         source = "https://ilostat.ilo.org/data/",
         id = NA_character_,
         type = "ILOSTAT source",
         idno = NA_character_) |>
  select(country, iso3c, year_start, year_end, year, instrument_name, instrument_type, authoring_entity, status, source, id, type, idno, indicators) |>
  arrange(country, year)

# export full dataset
writexl::write_xlsx(lfs_all, "Output/instrument_data_all_years/lfs.xlsx")
