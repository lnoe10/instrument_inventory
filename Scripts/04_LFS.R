### script to get Labour Force Survey data from ILOSTAT

# The ILO Survey Catalogue (webapps.ilo.org/surveyLib, NADA) used until 2025 was retired in 2026
# (all URLs redirect to "410 Gone | ILO Survey Catalog Retired").
# Labour force surveys now come from ILOSTAT: every annual observation is tagged with the survey
# it comes from (source.label, e.g. "LFS - EU Labour Force Survey"). One row per country, LFS source
# and year in which ILOSTAT has data from that survey. Quarterly/monthly rounds of the same survey
# in a year therefore count once, as one annual LFS.

library(tidyverse)

# ilostat_sources() helper (download + cache in Input/ilostat_sources.rds)
source("Scripts/00_settings.R")

# cached download, shared with 03_HIES.R
ilostat <- ilostat_sources()

lfs_all <- ilostat |>
  filter(source_type == "LFS", !is.na(iso3c)) |>
  group_by(country = ref_area.label, iso3c, survey = str_remove(source.label, "^LFS - "), year = as.numeric(time)) |>
  summarise(indicators = str_c(sort(unique(indicator)), collapse = ", "), .groups = "drop") |>
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
