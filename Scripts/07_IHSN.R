# Supplemental surveys - IHSN ---------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)

# IHSN https://catalog.ihsn.org/catalog
# https://catalog.ihsn.org/catalog/export/csv?ps=10000&collection[]=central
# Import all surveys
ihsn_raw <- fromJSON(content(GET("https://catalog.ihsn.org/index.php/api/catalog/search?ps=10000&from=2000&to=2021"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(
           nation == "Kosovo" ~ "XKX",
           nation == "S?n?gal" ~ "SEN",
           TRUE ~ iso3c),
         status = "Completed", 
         source = "https://catalog.ihsn.org/catalog")

# To acquire more metadata with which to filter data, we loop individual survey API calls for their metadata

# Initialize empty dataset
all_study_metadata <- tibble()

# Set up progress bar
n_iter <- length(ihsn_raw$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(ihsn_raw$id)){
  # Create a tibble consisting of the id no of the study in IHSN and the metadata of interest
  # To account for where info on metadata is elsewhere other than nested API call, we first determine whether API call is valid and then proceed.
  study <- tibble(id = ihsn_raw$id[i])
  
  # grab metadata
  study <- study %>%
    mutate(study_type = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), 
                               NA_character_,
                               fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name),
           unit_of_analysis = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit),
                                     NA_character_,
                                     fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit),
           study_scope = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$study_scope),
                                NA_character_,
                                fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$study_scope),
           universe = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$universe),
                             NA_character_,
                             fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$universe),
           data_kind = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind),
                              NA_character_,
                              fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind),
           producers = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$producers),
                              NA_character_,
                              fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$producers |> 
                                as_tibble() %>%
                                mutate(across(everything(), ~str_squish(.))) |> 
                                mutate(all_data = case_when(affiliation!="" & role!="" ~ paste0(name, " (AFFILIATION: ", affiliation, ", ROLE: ", role, ")"),
                                                            affiliation=="" ~ paste0(name, ", (ROLE: ", role, ")"),
                                                            role=="" ~ paste0(name, ", (AFFILIATION: ", affiliation, ")"))) |> 
                                select(all_data) |> 
                                summarise(producers = paste0(all_data, collapse = " // ")) |> 
                                pull()),
           funding_agencies = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$funding_agencies),
                                     NA_character_,
                                     fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$funding_agencies |> 
                                       as_tibble() %>%
                                       mutate(across(everything(), ~str_squish(.))) %>% 
                                       mutate(abbreviation = ifelse("abbr" %in% names(.), abbr, abbreviation)) |> 
                                       mutate(all_data = ifelse(role!="", paste0(name, " (", abbreviation, "), ROLE: ", role), paste0(name, " (", abbreviation, ")"))) |> 
                                       select(all_data) |> 
                                       summarise(funding_agencies = paste0(all_data, collapse = " // ")) |> 
                                       pull()))
  
  ### authoring entity detail - special case b/c sometimes there are two variables in the metadata, and sometimes just one
  if (is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity)==TRUE) {
    # case when metadata field is empty
    study <- study |> mutate(authoring_entity_detail = NA)
    
  } else if ("affiliation" %in% names(as_tibble(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity))==FALSE) {
    # case when there is only a "name" available
    study <- study |> mutate(authoring_entity_detail = fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
                               as_tibble() %>%
                               # need to use magrittr pipe for the period syntax to work
                               mutate(across(everything(), ~str_squish(.))) |> 
                               summarise(authoring_entity_detail = paste0(name, collapse = ", ")) |> 
                               pull())
  } else {
    study <- study |> mutate(authoring_entity_detail = ifelse(is_empty(fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity),
                                                              NA_character_,
                                                              fromJSON(content(GET(str_c("https://catalog.ihsn.org/index.php/api/catalog/", ihsn_raw$id[i], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
                                                                as_tibble() %>%
                                                                # need to use magrittr pipe for the period syntax to work
                                                                mutate(across(everything(), ~str_squish(.)))  %>%
                                                                mutate(authoring_entity = ifelse("affiliation" %in% names(.) & affiliation!="", paste0(name, " (", affiliation, ")"), name)) |> 
                                                                summarise(authoring_entity = paste0(authoring_entity, collapse = ", ")) |> 
                                                                pull()))
  }
  
  
  # Append to dataset
  all_study_metadata <- all_study_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

# save metadata
# saveRDS(all_study_metadata, "Input/ihsn_metadata.rds")

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
  select(id, idno, instrument_name = title, country=nation, iso3c, year_start, year_end, repositoryid, type, authoring_entity, authoring_entity_detail, funding_agencies, study_type:producers, status, source)

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

# read in the rows from above that Lorenz classified manually
ihsn_remaining <- readxl::read_xlsx("Output/misc_data/ihsn_unclassified_LN.xlsx") |> 
  # to avoid merging issues, convert text NA to actual NA, and remove extra whitespace
  mutate(across(everything(), ~ ifelse(.x=="NA", NA, .x))) |> 
  mutate(across(everything(), ~ str_squish(.x)))

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
xlsx::write.xlsx(ihsn_final, "Output/instrument_data_all_years/ihsn.xlsx")

# filter for years of interest (we only classified entries in this time frame)
ihsn_final <- ihsn_final |> filter((year_start == 2012 & year_end >=2013) | year_start>=2013)

# check that all rows have been classified with "keep"
stopifnot(ihsn_final |> filter(is.na(keep)) |> nrow() == 0)

# filter for only rows where keep is TRUE
ihsn_final_keep <- ihsn_final |> filter(keep==TRUE)

# export final dataset
xlsx::write.xlsx(ihsn_final_keep, "Output/instrument_data_ogdi_years/ihsn_2013-2022.xlsx")
