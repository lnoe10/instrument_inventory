library(tidyverse)

setwd("C:/Users/loren/Documents/Github/instrument_inventory")

# Reference window, census rounds, STANDARDS and in_window() live in 00_settings.R
source("Scripts/00_settings.R")

# Instrument inventory from Github repository instrument inventory (written by 10_combine_instruments.R)
instrument_inventory <- readxl::read_excel(str_c("Output/instrument_inventory_final_", str_replace(WINDOW_LABEL, "-", "_"), ".xlsx"))

# UNSD M49 country list and regions
unsd_codes <- readxl::read_excel("Input/unsd_codes.xlsx") |>
  janitor::clean_names() |>
  filter(!is.na(iso_alpha3_code))

# Instruments that count towards the scores
counted_instruments <- instrument_inventory |>
  filter(!is.na(iso3c)) |>
  # Only completed instruments count. A blank status is treated as completed: catalogue
  # sources (IHSN, ILO, FAO, World Bank) only list surveys that were conducted and do not
  # record a status. Ongoing, Data processing / analysis and Planned are excluded.
  filter(is.na(status) | status == "Completed") |>
  mutate(instrument_type = if_else(instrument_type == "Census", "Population Census", instrument_type)) |>
  # Population censuses count by UNSD census round, all other instruments by reference window
  filter(case_when(
    instrument_type == "Population Census" ~ as.character(census_round) == as.character(CENSUS_ROUND),
    TRUE ~ in_window(year)
  )) |>
  filter(instrument_type %in% names(STANDARDS))

# Notes describing each indicator
indicator_notes <- tibble(instrument_type = names(STANDARDS), standard = unname(STANDARDS)) |>
  mutate(indicator_notes = case_when(
    instrument_type == "Population Census" ~ str_c(
      "Raw value is the number of completed population censuses in the UNSD ", CENSUS_ROUND,
      " census round (", CENSUS_ROUND - 5, "-", CENSUS_ROUND + 4, "). ",
      "Score is 100 if at least ", standard, " census was conducted, 0 otherwise."),
    TRUE ~ str_c(
      "Raw value is the number of completed ", instrument_type, "s with a reference year in ", WINDOW_LABEL,
      " (multi-year surveys placed in their end year). ",
      "Score is the raw value divided by the standard of ", standard, ", times 100, capped at 100.")
  ))

# Compute instrument threshold achievement per country per survey type
survey_scores <- counted_instruments |>
  count(iso3c, instrument_type, name = "count_surveys") |>
  # full country list (see 00_settings.R): every country gets a row for every indicator,
  # 0 where nothing counted. Country cuts happen at the end of this script.
  right_join(expand_grid(iso3c = sort(union(unsd_codes$iso_alpha3_code,
                                            c("XKX", "TWN", na.omit(instrument_inventory$iso3c)))),
                         instrument_type = names(STANDARDS)),
             by = c("iso3c", "instrument_type")) |>
  left_join(indicator_notes, by = "instrument_type") |>
  mutate(count_surveys = replace_na(count_surveys, 0),
         # Top coded at 100
         indicator_score = pmin(count_surveys / standard * 100, 100),
         indicator_name = instrument_type,
         sub_category_name = "Survey systems",
         category_name = "Capacity",
         indicator_value_raw = count_surveys,
         display_value = as.character(indicator_score)) |>
  select(country_code = iso3c, indicator_name, sub_category_name, category_name, indicator_value_raw, indicator_notes, indicator_score, display_value, standard) |>
  group_by(country_code) |>
  mutate(sub_category_value = mean(indicator_score, na.rm = TRUE),
         sub_category_display = as.character(sub_category_value)) |>
  ungroup() |>
  arrange(country_code, indicator_name)

# Export data for clearinghouse - Country level
survey_country <- survey_scores |>
  mutate(indicator_value_raw = pmin(indicator_value_raw, 10)) |>
  # UN M49 regions (Africa, Americas, Asia, Europe, Oceania); used for the IDA regional averages.
  # Kosovo, Taiwan and the Netherlands Antilles have no M49 code and are assigned by hand.
  left_join(unsd_codes |> select(country_code = iso_alpha3_code, region = region_name),
            by = "country_code") |>
  mutate(region = case_when(
    country_code == "ANT" ~ "Americas",
    country_code == "TWN" ~ "Asia",
    country_code == "XKX" ~ "Europe",
    TRUE ~ region
  )) |>
  # World Bank regions, for reference in the ODIN table
  left_join(readr::read_csv(WB_CLASSIFICATION_FILE, show_col_types = FALSE) |>
              janitor::clean_names() |>
              filter(!is.na(region)) |>
              select(country_code = code, wb_region = region),
            by = "country_code") |>
  mutate(country = countrycode::countrycode(country_code, "iso3c", "country.name"),
  country = case_when(
    country_code == "XKX" ~ "Kosovo",
    country_code == "ANT" ~ "Netherlands Antilles",
    TRUE ~ country
  )) |>
  select(country, country_code, indicator_name, number_of_surveys = indicator_value_raw, percent_complete = indicator_score, standard, region, wb_region)

# Country cuts: the only place countries are removed (COUNTRY_EXCLUDE in 00_settings.R).
survey_country <- survey_country |>
  filter(!country_code %in% COUNTRY_EXCLUDE)

# Country groups (lists set in 00_settings.R)
ida_codes <- ida_countries()
odin_codes <- odin_countries()
message(length(ida_codes), " IDA countries; ",
        if (is.null(odin_codes)) "no ODIN list found, full table covers all countries" else paste(length(odin_codes), "ODIN countries"))
survey_country <- survey_country |> mutate(ida = country_code %in% ida_codes)
date_label <- format(Sys.Date(), "%b %d %Y")

# 1. Full table: all ODIN countries (all countries if no ODIN list), country rows only, no regional averages
survey_country |>
  filter(is.null(odin_codes) | country_code %in% odin_codes) |>
  arrange(country, indicator_name) |>
  select(country, country_code, indicator_name, number_of_surveys, percent_complete, standard, region, wb_region, ida) |>
  write_csv(str_c("Output/survey counts ", if (is.null(odin_codes)) "all countries " else "ODIN countries ", date_label, ".csv"), na = "")

# 2. Clearinghouse table: IDA countries, plus regional averages over IDA countries only
survey_ida <- survey_country |> filter(ida)

survey_region <- survey_ida |>
  group_by(indicator_name, region, standard) |>
  # round halves up (R's round() rounds 0.5 to 0 and 2.5 to 2)
  summarize(number_of_surveys = floor(mean(number_of_surveys, na.rm = TRUE) + 0.5), .groups = "drop") |>
  mutate(percent_complete = pmin(number_of_surveys / standard * 100, 100)) |>
  select(country = region, indicator_name, number_of_surveys, percent_complete, standard)

survey_ida |>
  select(-region, -wb_region, -ida) |>
  bind_rows(survey_region) |>
  # clearinghouse display name; the label stays "Agricultural Survey/Census" everywhere else
  mutate(indicator_name = if_else(indicator_name == "Agricultural Survey/Census",
                                  "Agricultural Survey/Ag Census", indicator_name)) |>
  arrange(country, indicator_name) |>
  select(country, country_code, indicator_name, number_of_surveys, percent_complete, standard) |>
  write_csv(str_c("Output/survey counts for clearinghouse ", date_label, ".csv"), na = "")
