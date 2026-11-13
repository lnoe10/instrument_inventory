# Supplemental surveys - IHSN ---------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

setwd("C:/Users/loren/Documents/GitHub/instrument_inventory")

# Reference window (WINDOW_START, WINDOW_END, WINDOW_LABEL)
source("Scripts/00_settings.R")

# IHSN https://catalog.ihsn.org/catalog
# https://catalog.ihsn.org/catalog/export/csv?ps=10000&collection[]=central
# Import all surveys
ihsn_raw <- fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/search?ps=10000&from=2010&to=", WINDOW_END)), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(
           nation == "Kosovo" ~ "XKX",
           str_detect(nation, "gal") & nation != "Portugal" ~ "SEN",
           nation == "Micronesia" ~ "FSM",
           TRUE ~ iso3c),
         status = "Completed", 
         source = "https://catalog.ihsn.org/catalog")

# To acquire more metadata with which to filter data, we loop individual survey API calls for their metadata

## Load study descriptions from saved file to check against what other metadata we need
all_study_metadata <- readRDS("Input/ihsn_metadata.rds")

# Both ihsn_raw and all_study_metadata have id as their unique row id, so can merge in terms of what they
# don't share.
new_ihsn_surveys <- ihsn_raw |>
  anti_join(all_study_metadata, by = "id")
# Metadata for new studies is fetched automatically below and appended to the cache.

if (nrow(new_ihsn_surveys) > 0) {
  # one API call per study (nada_metadata_for() in 00_settings.R). The previous loop made about
  # ten calls per study for the same JSON. Extra fields (study_scope, universe) are kept as before.
  new_study_metadata <- nada_metadata_for(
    new_ihsn_surveys$id,
    str_c("https://catalog.ihsn.org/index.php/api/catalog/", new_ihsn_surveys$id, "?id_format=id")
  )

  # save metadata
  all_study_metadata <- all_study_metadata |>
    bind_rows(new_study_metadata |> mutate(id = as(id, class(all_study_metadata$id))))
  saveRDS(all_study_metadata, "Input/ihsn_metadata.rds")
}

## Load study descriptions from saved file
all_study_metadata <- readRDS("Input/ihsn_metadata.rds")

# Final list of IHSN, to be used to supplement other survey groups as appropriate
ihsn <- ihsn_raw %>%
  left_join(all_study_metadata) %>%
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
                            "Event/transaction data [evn]")) |> 
  select(id, idno, instrument_name = title, country=nation, iso3c, year_start, year_end, repositoryid, type, authoring_entity, authoring_entity_detail, study_type:data_kind, status, source)

# export distinct data kind / study type groupings for Lorenz to have a look at
# ihsn |> filter(year_end>=2013)|> group_by(data_kind, study_type) |> summarise(n=n()) |> write.csv("Output/misc_data/data_types_ihsn_2013.csv")

#### add in "keep" classification based on Lorenz' work
# read in Lorenz' classification file based on study type and data kind
data_types_LN <- read_csv("Output/misc_data/data_types_ihsn_2013_LN.csv", show_col_types = F) |> select(data_kind:instrument_type)

# export additional metadata file for Lorenz to do final classification - this is for rows in the above csv that didn't have enough info
# left_join(ihsn, data_types_LN) |> filter(is.na(keep) & year_end>=2013) |> 
#   select(data_kind, study_type, unit_of_analysis, universe) |> 
#   distinct() |> 
#   write.csv("Output/misc_data/ihsn_additional_info.csv")

# read in the completed csv with Lorenz' classification for those which he couldn't previously classify
# and needed additional metadata for
additional_info_LN <- read_csv("Output/misc_data/ihsn_additional_info_LN.csv", show_col_types = F) |> filter(!is.na(keep)) |> 
  mutate(keep = ifelse(keep=="Yes", 1, 0))

# filter for where "keep" is not NA
data_types_LN <- data_types_LN |> filter(!is.na(keep)) |> select(-n)

# bind Lorenz' "keep" flag onto the IHSN data
ihsn_keep <- left_join(ihsn, data_types_LN, by=c("data_kind", "study_type"))

# now add the additionally classified rows which required extra metadata for proper classification
ihsn_keep_all <- left_join(ihsn_keep, additional_info_LN |> 
                         rename(keep_additional = keep, instrument_type_additional = instrument_type), 
                       by=c("data_kind", "study_type", "unit_of_analysis", "universe")) |> 
  # combine the two keep flags, had to keep them separate first for correct merging
  # also, I checked in the console and there were no rows where keep and keep_additional were both filled in
  # same for instrument_type
  mutate(keep = ifelse(is.na(keep) & !is.na(keep_additional), keep_additional, keep),
         instrument_type = ifelse(is.na(instrument_type) & !is.na(instrument_type_additional), instrument_type_additional, instrument_type)) |> 
  select(-keep_additional, -instrument_type_additional)

# # export unclassified rows for Lorenz to classify in Google sheets with first entry for each distinct metadata combo
# ihsn_keep_all |> filter(is.na(keep)) |> 
#   filter((year_start == 2012 & year_end >=2013) | year_start>=2013) |> 
#   select(id, idno, instrument_name, universe, unit_of_analysis, data_kind, authoring_entity_detail, study_type, study_scope, source) |> 
#   group_by(instrument_name, universe, unit_of_analysis, data_kind) |> 
#   slice(1) |> 
#   ungroup() |> 
#   # add id to link so specific instrument's catalog webpage is more accessible
#   mutate(source = paste0(source, "/", id)) |> 
#   write_csv("Output/misc_data/ihsn_unclassified.csv")

# Review file for studies not covered by any earlier classification (written further below).
# Fill in keep (Yes/No), instrument_type and rationale in this file, then rerun this script.
ihsn_review_file <- str_c("Output/misc_data/ihsn_unclassified_", WINDOW_LABEL, ".xlsx")
review_key <- c("instrument_name", "universe", "unit_of_analysis", "data_kind")

# read in the rows from above that Lorenz classified manually, plus any rows reviewed for this update
ihsn_remaining <- list("Output/misc_data/ihsn_unclassified_LN.xlsx", ihsn_review_file) |>
  keep(file.exists) |>
  map(\(f) readxl::read_xlsx(f, col_types = "text")) |>
  bind_rows() |>
  filter(!is.na(keep)) |>
  # to avoid merging issues, convert text NA to actual NA, and remove extra whitespace
  mutate(across(everything(), ~ ifelse(.x=="NA", NA, .x))) |>
  mutate(across(everything(), ~ str_squish(.x))) |>
  distinct(across(all_of(review_key)), .keep_all = TRUE)

# add the classifications for remaining ihsn entries back to main dataframe
ihsn_final <- left_join(ihsn_keep_all |> 
                          # remove excess whitespace so no issues when merging
                          mutate(across(everything(), ~ str_squish(.x))), 
                        ihsn_remaining |> 
                        # rename same variables accordingly so we don't have overlapping names
                          select(instrument_name, universe, unit_of_analysis, data_kind, 
                                 keep_remaining = keep, rationale_remaining = rationale, 
                                 instrument_type_remaining = instrument_type), 
                        by=c("instrument_name", "universe", "unit_of_analysis","data_kind")) |> 
  # fill in the new classifications
  mutate(instrument_type = ifelse(is.na(instrument_type) & !is.na(instrument_type_remaining),
                                  instrument_type_remaining, instrument_type),
         keep = ifelse(is.na(keep) & !is.na(keep_remaining), keep_remaining, keep),
         rationale = ifelse(is.na(rationale) & !is.na(rationale_remaining), rationale_remaining, rationale)) |> 
  # drop extra variables which are now redundant b/c we filled everything in
  select(-c(keep_remaining, rationale_remaining, instrument_type_remaining))

# standardize "keep" classification to logical true/false
ihsn_final <- ihsn_final |> mutate(keep = case_when(keep %in% c("Yes", "1") ~ TRUE,
                                                    keep %in% c("No", "0") ~ FALSE))

# standardize instrument type naming with those in the rest of the instrument inventory data
ihsn_final <- ihsn_final |> 
  mutate(instrument_type = case_when(instrument_type=="Ag survey" ~ "Agricultural Survey/Census",
                                     instrument_type=="Agricultural survey" ~ "Agricultural Survey/Census",
                                     instrument_type=="Time Use survey" ~ "Time Use Survey",
                                     instrument_type=="Time Use" ~ "Time Use Survey",
                                     instrument_type=="Household income/expenditure survey" ~ "HIES",
                                     instrument_type=="household health survey" ~ "Household Health Survey",
                                     instrument_type=="Household health survey" ~ "Household Health Survey",
                                     .default = instrument_type))

# export full dataset for all years, overwriting old one with updated information
writexl::write_xlsx(ihsn_final, "Output/instrument_data_all_years/ihsn.xlsx")

# filter for years of interest (we only classified entries in this time frame)
# Multi-year studies are placed in their end year, as in 10_combine_instruments.R
ihsn_final <- ihsn_final |> filter(in_window(year_end))

# check that all rows have been classified with "keep". Rows that are not get added to the review file
# (one row per distinct name/universe/unit/data kind, as before) and are left out until classified.
unclassified <- ihsn_final |> filter(is.na(keep))
if (nrow(unclassified) > 0) {
  existing_review <- if (file.exists(ihsn_review_file)) readxl::read_xlsx(ihsn_review_file, col_types = "text") else tibble()
  new_review <- unclassified |>
    select(id, idno, instrument_name, country, year_start, year_end, universe, unit_of_analysis, data_kind,
           authoring_entity_detail, study_type, study_scope, source) |>
    group_by(across(all_of(review_key))) |>
    slice(1) |>
    ungroup() |>
    # add id to link so specific instrument's catalog webpage is more accessible
    mutate(source = paste0(source, "/", id), keep = NA_character_, instrument_type = NA_character_, rationale = NA_character_) |>
    mutate(across(everything(), as.character))
  if (nrow(existing_review) > 0) new_review <- anti_join(new_review, existing_review, by = review_key)
  # never overwrite classifications already entered: only append rows not yet in the file
  writexl::write_xlsx(bind_rows(existing_review, new_review) |> arrange(country, instrument_name), ihsn_review_file)
  warning(nrow(unclassified), " IHSN studies in the window are not classified yet and are left out. ",
          "Classify them in ", ihsn_review_file, " (", nrow(new_review), " rows added) and rerun.")
}

# filter for only rows where keep is TRUE
ihsn_final_keep <- ihsn_final |> filter(keep==TRUE)

# export final dataset
writexl::write_xlsx(ihsn_final_keep, str_c("Output/instrument_data_ogdi_years/ihsn_", WINDOW_LABEL, ".xlsx"))
