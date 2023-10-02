### script to scrape HIES data from LSMS and ILO

library(httr)
library(jsonlite)
library(tidyverse)

############ LSMS - WORLD BANK ############

# scrape data from API
lsms_raw <- fromJSON(content(GET("https://microdata.worldbank.org/index.php/api/catalog/search?ps=10000"), "text"))$result$rows %>%
  as_tibble() %>%
  mutate(api_call = str_c("https://microdata.worldbank.org/index.php/api/catalog/", id, "?id_format=id"))

# First acquire more information on surveytypes from metadata - series name and data kind

# Initialize empty dataset
# all_wb_metadata <- tibble()
# 
# # Set up progress bar
# n_iter <- length(lsms_raw$id)
# pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                      max = n_iter, # Maximum value of the progress bar
#                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                      width = 50,   # Progress bar width. Defaults to getOption("width")
#                      char = "=")   # Character used to create the bar
# 
# # Loop
# for (i in 1:length(lsms_raw$id)){
#   # Set up tibble with basic info
#   study <- tibble(id = lsms_raw$id[i], study_type = NA_character_)
#   # Determine length of list generated from API call to filter out
#   # Where we hit error otherwise of subscript being out of bounds
#   study <- study %>%
#     mutate(length = case_when(
#       !is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)),
#       TRUE ~ NA_integer_))
#   # If list is deep enough, proceed to extract data
#   if(study$length > 3){
#     study <- study %>%
#       # Test first whether response is valid based on tunneling into list
#       mutate(study_type = ifelse(is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
#                                  fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name),
#         data_kind = ifelse(is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
#                             fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind),
#         unit_of_analysis = ifelse(is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit), NA_character_,
#                                   fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit))
#     
#     ### authoring entity detail - special case b/c sometimes there are two variables in the metadata, and sometimes just one
#     if (is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity)==TRUE) {
#       # case when metadata field is empty
#       study <- study |> mutate(authoring_entity_detail = NA)
#       
#     } else if ("affiliation" %in% names(as_tibble(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity))==FALSE) {
#       # case when there is only a "name" available
#       study <- study |> mutate(authoring_entity_detail = fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
#                                  as_tibble() %>%
#                                  # need to use magrittr pipe for the period syntax to work
#                                  mutate(across(everything(), ~str_squish(.))) |> 
#                                  summarise(authoring_entity_detail = paste0(name, collapse = ", ")) |> 
#                                  pull())
#     } else {
#       study <- study |> mutate(authoring_entity_detail = ifelse(is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity),
#                                                                 NA_character_,
#                                                                 fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
#                                                                   as_tibble() %>%
#                                                                   # need to use magrittr pipe for the period syntax to work
#                                                                   mutate(across(everything(), ~str_squish(.)))  %>%
#                                                                   mutate(authoring_entity = ifelse("affiliation" %in% names(.) & affiliation!="", paste0(name, " (", affiliation, ")"), name)) |> 
#                                                                   # remove empty rows
#                                                                   filter(authoring_entity!="") |> 
#                                                                   summarise(authoring_entity = paste0(authoring_entity, collapse = ", ")) |> 
#                                                                   pull()))
#     }
#   }
#   # Append to dataset
#   all_wb_metadata <- all_wb_metadata %>%
#     bind_rows(study)
#   # Insert brief pause in code to not look like a robot to the API
#   Sys.sleep(sample(seq(0,0.3,by=0.001),1))
#   # Increment progress bar
#   setTxtProgressBar(pb, i)
# }
# 
# # close the connection
# close(pb)
# 
# # drop length variable
# all_wb_metadata <- all_wb_metadata %>% select(-length)

# save copy of WB study descriptions so we don't have to rerun all entries, just new ones
# saveRDS(all_wb_metadata, file = "Input/all_wb_metadata.rds")

# Load study descriptions from saved file
all_wb_metadata <- readRDS("Input/all_wb_metadata.rds")

# create list of newly scraped instruments
new_lsms <- lsms_raw |> filter(!(id %in% all_wb_metadata$id))

for (i in 1:nrow(new_lsms)){
  # Set up tibble with basic info
  study <- tibble(id = new_lsms$id[i], study_type = NA_character_)
  # Determine length of list generated from API call to filter out
  # Where we hit error otherwise of subscript being out of bounds
  study <- study %>%
    mutate(length = case_when(
      !is_empty(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)),
      TRUE ~ NA_integer_))
  # If list is deep enough, proceed to extract data
  if(study$length > 3){
    study <- study %>%
      # Test first whether response is valid based on tunneling into list
      mutate(study_type = ifelse(is_empty(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
                                 fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name),
             data_kind = ifelse(is_empty(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
                                fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind),
             unit_of_analysis = ifelse(is_empty(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit), NA_character_,
                                       fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit))
    
    ### authoring entity detail - special case b/c sometimes there are two variables in the metadata, and sometimes just one
    if (is_empty(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity)==TRUE) {
      # case when metadata field is empty
      study <- study |> mutate(authoring_entity_detail = NA)
      
    } else if ("affiliation" %in% names(as_tibble(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity))==FALSE) {
      # case when there is only a "name" available
      study <- study |> mutate(authoring_entity_detail = fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
                                 as_tibble() %>%
                                 # need to use magrittr pipe for the period syntax to work
                                 mutate(across(everything(), ~str_squish(.))) |> 
                                 summarise(authoring_entity_detail = paste0(name, collapse = ", ")) |> 
                                 pull())
    } else {
      study <- study |> mutate(authoring_entity_detail = ifelse(is_empty(fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity),
                                                                NA_character_,
                                                                fromJSON(content(GET(new_lsms$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
                                                                  as_tibble() %>%
                                                                  # need to use magrittr pipe for the period syntax to work
                                                                  mutate(across(everything(), ~str_squish(.)))  %>%
                                                                  mutate(authoring_entity = ifelse("affiliation" %in% names(.) & affiliation!="", paste0(name, " (", affiliation, ")"), name)) |> 
                                                                  # remove empty rows
                                                                  filter(authoring_entity!="") |> 
                                                                  summarise(authoring_entity = paste0(authoring_entity, collapse = ", ")) |> 
                                                                  pull()))
    }
  }
  
  # Append to dataset
  all_wb_metadata_new <- all_wb_metadata %>%
    bind_rows(study |> select(-length))
}

# export updated metadata
saveRDS(all_wb_metadata_new, file = "Input/all_wb_metadata.rds")

# Load updated metadata from saved file
all_wb_metadata <- readRDS("Input/all_wb_metadata.rds")

# Set up final list of LSMS/HIES
lsms <- lsms_raw %>%
  # Duplicate Serbia and Montenegro and assign to Serbia and Montenegro respectively
  filter(nation != "Serbia and Montenegro") %>%
  bind_rows(
    lsms_raw %>% 
      filter(str_detect(nation, "Serbia and Montenegro")) %>% 
      uncount(2, .id = "index") %>%
      mutate(nation = case_when(
        index == 1 ~ "Serbia",
        TRUE ~ "Montenegro"
      )) %>%
      select(-index)
  ) %>%
  mutate(year = as.numeric(str_extract(idno, "[0-9]{4}")),
         year = case_when(nation == "Malawi" & year == 2010 ~ as.numeric(year_end), TRUE ~ year),
         # Create iso3 code variable.
         # A lot of warnings will pop up for some surveys done in multiple countries.
         # Ok to keep, we'll filter on study_type and then only merge IDA countries anyway conducted in that country only
         iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(nation == "Kosovo" ~ "XKX", TRUE ~ iso3c),
         instrument_type = "HIES",
         status = "completed",
         source = "https://microdata.worldbank.org/index.php/catalog/lsms") %>%
  # Merge in study type description
  left_join(all_wb_metadata, by="id") %>%
  # Filter for correct repository (lsms) and add survey types
  filter(repositoryid %in% c("lsms") | study_type %in% c("Income/Expenditure/Household Survey [hh/ies]",
                                                         "Living Standards Measurement Study",
                                                         "Living Standards Measurement Study [hh/lsms]",
                                                         "Socio-Economic/Monitoring Survey [hh/sems]",
                                                         "Income/Expenditure/Household Survey",
                                                         "Socio-Economic/Monitoring Survey",
                                                         "Core Welfare Indicators Questionnaire [hh/cwiq]")) %>%
  # Keep relevant variables
  select(id, idno, country = nation, iso3c, year, instrument_name = title, instrument_type, status, source, study_type, authoring_entity, repositoryid, data_kind, unit_of_analysis, authoring_entity_detail)

# export the clean dataset
xlsx::write.xlsx(lsms, "Output/instrument_data_all_years/lsms.xlsx")

################ ILO HIES data ################

# API call, add additional variables
ilo_hies <- fromJSON(content(GET("https://www.ilo.org/surveyLib/index.php/api/catalog",
                                 query = list(ps=10000, repo="HIES")), "text"))$result$rows |>
  as_tibble() |> 
  mutate(iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(nation == "Kosovo" ~ "XKX", TRUE ~ iso3c),
         status="completed",
         source="https://www.ilo.org/surveyLib/index.php/api/catalog",
         instrument_type = "HIES") |> 
  # filter for OGDI years (2012/13-present)
  rename(year = year_end) |> 
  filter(year >= 2013) |> 
  mutate(year = as.numeric(year)) |> 
  select(id, idno, instrument_name = title, country=nation, iso3c, instrument_type, year, repo_title, repositoryid, type, authoring_entity, status, source)

################ checking for duplicates and combining ILO and LSMS data ################

# extract beginning of IDNO for both to merge on
lsms <- lsms |> mutate(short_idno = str_extract(idno, ".+?(?=_(v|V)\\d)"))
ilo_hies <- ilo_hies |> mutate(short_idno = str_extract(idno, ".+(?=_(v|V)\\d)"))

# look at overlap by short id
lsms |> select(id, short_idno, instrument_name, country, year, authoring_entity) |>
  left_join(ilo_hies |> select(id, instrument_name, country, short_idno, year, authoring_entity), by=c("short_idno", "year", "country")) |> 
  filter(!is.na(id.y) & instrument_name.x!=instrument_name.y) |> 
  select(country, year, instrument_name_lsms = instrument_name.x, instrument_name_ilo = instrument_name.y, authoring_entity_lsms = authoring_entity.x, authoring_entity_ilo=authoring_entity.y)

# overlap by shortid with same instrument name
shortidno_overlap <- lsms |> select(id, short_idno, instrument_name, country, year) |>
  left_join(ilo_hies |> select(id, instrument_name, country, short_idno, year), by=c("short_idno", "year", "country")) |> 
  filter(!is.na(id.y) & instrument_name.x==instrument_name.y) |> 
  pull(short_idno)

# look at countries that have data in ILO but not LSMS
ilo_countries <- setdiff(ilo_hies$iso3c, lsms$iso3c)

# grab ILO data for those countries not in LSMS, and bind together in new HIES dataset
hies_all <- ilo_hies |> filter(iso3c %in% ilo_countries) |> 
  mutate(id = as.numeric(id)) |> 
  bind_rows(lsms)

# filter ilo dataframe for those remaining for manual checking
ilo_hies_filtered <- ilo_hies |> filter(!(iso3c %in% ilo_countries) & !(short_idno %in% shortidno_overlap))

######################
# export the remaining ILO data to do assessment of duplicates manually on google sheets
# xlsx::write.xlsx(ilo_hies_filtered |> select(id, idno, country, year, instrument_name, 
#                                              instrument_type, repositoryid, authoring_entity), 
#                  "Output/misc_data/hies_additional_ilo_data.xlsx")

# # create dataframe with ILO and LSMS data, with country year matches specified
# ilo_hies_filtered_new <- ilo_hies_filtered |> select(id_ilo=id, country, year, instrument_name_ilo=instrument_name, instrument_type_ilo=instrument_type, authoring_entity_ilo=authoring_entity) %>%
#   left_join(lsms |> select(id_lsms=id, country, year, instrument_name_lsms=instrument_name, instrument_type_lsms=instrument_type, authoring_entity_lsms=authoring_entity), ., by=c("year", "country")) |>
#   select(id_ilo, id_lsms, country, year, instrument_name_ilo, instrument_name_lsms,
#          instrument_type_ilo, instrument_type_lsms, authoring_entity_ilo, authoring_entity_lsms) |>
#   mutate(country_yr_match = ifelse(!is.na(id_lsms) & !is.na(id_ilo), 1, 0))

# read in checked dataframe with my classification/notes, merge with above dataframe so we have LSMS and ILO data
# this exports the country-year matches to a CSV file
# readxl::read_xlsx("Output/misc_data/hies_additional_ilo_data_checked.xlsx") |>
#   select(id, instrument_name, `name diff from LSMS data?`:possible_duplicate) %>%
#   left_join(ilo_hies_filtered_new, ., by=c("id_ilo" = "id", "instrument_name_ilo" = "instrument_name")) |>
#   filter(!is.na(`name diff from LSMS data?`)) |>
#   write_csv("Output/misc_data/hies_country_yr_matches.csv")
######################

# read in country year matches between ILO and LSMS data that has been checked for duplicates
ilo_country_yr_matches_checked <- read_csv("Output/misc_data/hies_additional_ilo_data.xlsx - country_yr_matches.csv", show_col_types = F)

# filter above to only include non-duplicates, grab distinct ILO IDs to bind onto hies_all dataframe
ilo_not_duplicate_ids <- ilo_country_yr_matches_checked |> filter(final_duplicate_flag==0) |> select(id_ilo) |> distinct() |> pull()

# bind those rows that are not duplicates onto the hies_all dataframe
hies_all <- ilo_hies_filtered |> filter(id %in% ilo_not_duplicate_ids) |> 
  mutate(id = as.numeric(id)) %>% 
  bind_rows(hies_all, .)

# update ilo dataframe for those remaining that didn't have a country-yr match / haven't added to the hies_all dataframe
ilo_hies_filtered <- ilo_hies_filtered |> filter(!(id %in% ilo_country_yr_matches_checked$id_ilo))

# checked manually in Google sheets, none are suspected duplicates, so we can add them into the hies_all df
# see sheet 3 of https://docs.google.com/spreadsheets/d/1WxHgcohiy6EWjeBoIpAxj4xHq-FkA3NP/edit#gid=262908178&fvid=1823687230
# sheet is titled "not_country_yr_matches"
hies_all <- ilo_hies_filtered |> 
  mutate(id = as.numeric(id)) %>% 
  bind_rows(hies_all, .)

# export combined data to excel
xlsx::write.xlsx(hies_all, "Output/instrument_data_all_years/hies.xlsx")