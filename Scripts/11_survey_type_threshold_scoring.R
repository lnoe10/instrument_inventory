library(tidyverse)

setwd("C:/Users/loren/Documents/Github/instrument_inventory")

# Instrument inventory from Github repository instrument inventory
instrument_inventory <- readxl::read_excel("Output/instrument_inventory_final_2015_2024.xlsx")

# Compute instrument threshold achievement per country per surveytype
survey_scores <- instrument_inventory |>
  filter(instrument_type != "Census") |>
  group_by(iso3c, instrument_type) |>
  summarize(count_surveys = n()) |>
  ungroup() %>%
  full_join(instrument_inventory |>
              filter(!instrument_type %in% c("Census", "Gender-based violence survey", "Other household survey")) |> 
              filter(!is.na(iso3c)) |> 
              expand(iso3c, instrument_type)) |>
  mutate(count_surveys = case_when(
    is.na(count_surveys) ~ 0,
    TRUE ~ count_surveys
  ),
  indicator_score = case_when(
    instrument_type == "Household Health Survey" ~ count_surveys/4*100,
    instrument_type == "Household Income and Expenditure Survey" ~ count_surveys/2*100,
    instrument_type == "Labour Force Survey" ~ count_surveys/10*100,
    instrument_type == "Time Use Survey" ~ count_surveys/2*100,
    instrument_type == "Agricultural Survey/Census" ~ count_surveys/2*100,
    TRUE ~ NA_real_
  ),
  # Top code scores
  indicator_score = case_when(
    indicator_score > 100 ~ 100,
    TRUE ~ indicator_score
  ),
  indicator_name = instrument_type,
  sub_category_name = "Survey systems",
  category_name = "Capacity",
  indicator_value_raw = count_surveys,
  indicator_notes = case_when(
    instrument_type == "Household Health Survey" ~ str_c("Raw value is sum of ", instrument_type, " over 2013-2022. Transformed to indicator score (100 or 0) by checking if sum is greater than or equal to 8."),
    instrument_type == "Household Income and Expenditure Survey" ~ str_c("Raw value is sum of ", instrument_type, " over 2013-2022. Transformed to indicator score (100 or 0) by checking if sum is greater than or equal to 2."),
    instrument_type == "Labour Force Survey" ~ str_c("Raw value is sum of ", instrument_type, " over 2013-2022. Transformed to indicator score (100 or 0) by checking if sum is greater than or equal to 10."),
    instrument_type == "Time Use Survey" ~ str_c("Raw value is sum of ", instrument_type, " over 2013-2022. Transformed to indicator score (100 or 0) by checking if sum is greater than or equal to 1."),
    instrument_type == "Agricultural Survey/Census" ~ str_c("Raw value is sum of ", instrument_type, " over 2013-2022. Transformed to indicator score (100 or 0) by checking if sum is greater than or equal to 2."),
  ),
  display_value = as.character(indicator_score)) |>
  filter(!instrument_type %in% c("Gender-based violence survey", "Other household survey")) |>
  filter(!is.na(iso3c)) |>
  select(country_code = iso3c, indicator_name, sub_category_name, category_name, indicator_value_raw, indicator_notes, indicator_score, display_value) |>
  group_by(country_code) %>%
  mutate(sub_category_value = mean(indicator_score, na.rm = TRUE),
         sub_category_display = as.character(sub_category_value)) |>
  ungroup() %>%
  arrange(country_code, indicator_name)

# Export data for clearinghouse - Country level
survey_country <- survey_scores |>
  mutate(indicator_value_raw = case_when(
    indicator_value_raw > 10 ~ 10,
    TRUE ~ indicator_value_raw
  )) |>
  left_join(readxl::read_excel("Input/unsd_codes.xlsx") |>
              janitor::clean_names() |>
              select(country_code = iso_alpha3_code, region = region_name)) |>
  mutate(region = case_when(
    country_code == "ANT" ~ "Americas",
    country_code == "TWN" ~ "Asia",
    country_code == "XKX" ~ "Europe",
    TRUE ~ region
  ),
  country = countrycode::countrycode(country_code, "iso3c", "country.name"),
  country = case_when(
    country_code == "XKX" ~ "Kosovo",
    country_code == "ANT" ~ "Netherlands Antilles",
    TRUE ~ country
  )) |>
  select(country, country_code, indicator_name, number_of_surveys = indicator_value_raw, percent_complete = indicator_score, region)

# Export data for clearinghouse - Region level
survey_region <- survey_country |>
  group_by(indicator_name, region) |>
  summarize(number_of_surveys = round(mean(number_of_surveys, na.rm = TRUE), 0)) |>
  ungroup() |>
  mutate(percent_complete = case_when(
    indicator_name == "Household Health Survey" ~ pmin(number_of_surveys/4*100, 100),
    indicator_name == "Household Income and Expenditure Survey" ~ pmin(number_of_surveys/2*100, 100),
    indicator_name == "Labour Force Survey" ~ pmin(number_of_surveys/10*100, 100),
    indicator_name == "Time Use Survey" ~ pmin(number_of_surveys/2*100, 100),
    indicator_name == "Agricultural Survey/Census" ~ pmin(number_of_surveys/2*100, 100),
    TRUE ~ NA_real_
  )) |>
  select(country = region, indicator_name, number_of_surveys, percent_complete)

# Export
survey_country |>
  select(-region) |>
  bind_rows(survey_region) |>
  arrange(country, indicator_name) |>
  mutate(standard = case_when(
    indicator_name == "Household Health Survey" ~ 4,
    indicator_name == "Household Income and Expenditure Survey" ~ 2,
    indicator_name == "Labour Force Survey" ~ 10,
    indicator_name == "Time Use Survey" ~ 2,
    indicator_name == "Agricultural Survey/Census" ~ 2,
    TRUE ~ NA_real_
  )) |>
  write_csv("Output/survey counts for clearinghouse Nov 18 2025.csv", na = "")
