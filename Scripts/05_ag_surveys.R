### script to scrape Agricultural Survey data from the FAO

library(httr)
library(jsonlite)
library(tidyverse)
library(openxlsx)

# First get data from API
agri_survey_raw <- fromJSON(content(GET("https://microdata.fao.org/index.php/api/catalog/search?ps=10000"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(api_call = str_c("https://microdata.fao.org/index.php/api/catalog/", id, "?id_format=id"))

# Check existing list of metadata in terms of what else we need. After importing new metadata,
# we will override this file so next time we run it, we will have updated metadata and can check
# it against new surveys again.
all_fao_metadata <- readRDS("Input/fao_ag_survey_metadata.rds")

# Both agri_survey_raw and all_fao_metadata have id as their unique row id, so can merge in terms of what they
# don't share.
new_ag_surveys <- agri_survey_raw |>
  anti_join(all_fao_metadata, by = "id")
# IF THIS df is not empty, you need to uncomment the below and rerun for the extra surveys.

## acquire more information on surveytypes from metadata. Commented out if you don't need new metadata
#
## Initialize empty dataset
#new_fao_metadata <- tibble()
#
## Set up progress bar
#n_iter <- length(new_ag_surveys$id)
#pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
#                    max = n_iter, # Maximum value of the progress bar
#                    style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                    width = 50,   # Progress bar width. Defaults to getOption("width")
#                    char = "=")   # Character used to create the bar
#
## Loop
#for (i in 1:length(new_ag_surveys$id)){
# 
# # Set up tibble with basic info
# study <- tibble(id = new_ag_surveys$id[i], study_type = NA_character_)
# 
# # Determine length of list generated from API call to filter out
# # Where we hit error otherwise of subscript being out of bounds
# study <- study %>%
#   mutate(length = case_when(
#     !is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)),
#     TRUE ~ NA_integer_))
# # If list is deep enough, proceed to extract data
# if(study$length > 3){
#   study <- study %>%
#     # Test first whether response is valid based on tunneling into list
#     mutate(study_type = ifelse(is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), 
#                                NA_character_,
#                                fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name),
#            unit_of_analysis = ifelse(is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit), 
#                                      NA_character_,
#                                   fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit),
#            data_kind = ifelse(is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), 
#                               NA_character_,
#                               fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind),
#            universe = ifelse(is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$universe), 
#                              NA_character_,
#                              fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$universe))
#   
#   #### producers info - special case b/c sometimes role is not available in the metadata, causing code to break
#   #if (is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$producers)==TRUE) {
#   #  
#   #  # case when metadata field is empty
#   #  study <- study |> mutate(producers = NA)
#   #  
#   #} else if ("role" %in% names(as_tibble(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$producers))==FALSE) {
#   #  
#   #  # case when there is only a "name" and "abbreviation" available
#   #  study <- study |> mutate(producers = fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$producers |> 
#   #                             as_tibble() %>%
#   #                             mutate(across(everything(), ~str_squish(.))) |> 
#   #                             mutate(all_data = case_when(affiliation!="" ~ paste0(name, " (AFFILIATION: ", affiliation, ")"),
#   #                                                         affiliation=="" ~ paste0(name))) |> 
#   #                             select(all_data) |> 
#   #                             summarise(producers = paste0(all_data, collapse = " // ")) |> 
#   #                             pull())
#   #} else {
#   #  study <- study |> mutate(producers = fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$producers |> 
#   #                             as_tibble() %>%
#   #                             mutate(across(everything(), ~str_squish(.))) |> 
#   #                             mutate(all_data = case_when(affiliation!="" & role!="" ~ paste0(name, " (AFFILIATION: ", affiliation, ", ROLE: ", role, ")"),
#   #                                                         affiliation=="" ~ paste0(name, ", (ROLE: ", role, ")"),
#   #                                                         role=="" ~ paste0(name, ", (AFFILIATION: ", affiliation, ")"))) |> 
#   #                             select(all_data) |> 
#   #                             summarise(producers = paste0(all_data, collapse = " // ")) |> 
#   #                             pull())
#   #}
#   #  
#   #
#   #### funding agency info - special case b/c sometimes role is not available in the metadata, causing code to break
#   #if (is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$funding_agencies)==TRUE) {
#   #  
#   #  # case when metadata field is empty
#   #  study <- study |> mutate(funding_agencies = NA)
#   #  
#   #} else if ("role" %in% names(as_tibble(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$funding_agencies))==FALSE) {
#   #  # case when there is only a "name" and "abbreviation" available
#   #  study <- study |> mutate(funding_agencies = fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$funding_agencies |> 
#   #                             as_tibble() |> 
#   #                             mutate(across(everything(), ~str_squish(.))) %>% 
#   #                             mutate(abbreviation = ifelse("abbr" %in% names(.), abbr, abbreviation)) %>% 
#   #                             mutate(all_data = paste0(name, " (", abbreviation, ")")) |> 
#   #                             select(all_data) |> 
#   #                             summarise(funding_agencies = paste0(all_data, collapse = " // ")) |> 
#   #                             pull())
#   #} else {
#   #  study <- study |> mutate(funding_agencies = fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$production_statement$funding_agencies |> 
#   #                             as_tibble() |> 
#   #                             mutate(across(everything(), ~str_squish(.))) %>% 
#   #                             mutate(abbreviation = ifelse("abbr" %in% names(.), abbr, abbreviation)) %>% 
#   #                             mutate(all_data = ifelse(role!="", paste0(name, " (", abbreviation, "), ROLE: ", role), paste0(name, " (", abbreviation, ")"))) |> 
#   #                             select(all_data) |> 
#   #                             summarise(funding_agencies = paste0(all_data, collapse = " // ")) |> 
#   #                             pull())
#   #}
#   
#   ### authoring entity detail - special case b/c sometimes there are two variables in the metadata, and sometimes just one
#   if (is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity)==TRUE) {
#     # case when metadata field is empty
#     study <- study |> mutate(authoring_entity_detail = NA)
#     
#   } else if ("affiliation" %in% names(as_tibble(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity))==FALSE) {
#     # case when there is only a "name" available
#     study <- study |> mutate(authoring_entity_detail = fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
#                                as_tibble() %>%
#                                # need to use magrittr pipe for the period syntax to work
#                                mutate(across(everything(), ~str_squish(.))) |> 
#                                summarise(authoring_entity_detail = paste0(name, collapse = ", ")) |> 
#                                pull())
#   } else {
#     study <- study |> mutate(authoring_entity_detail = ifelse(is_empty(fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity),
#                                                               NA_character_,
#                                                               fromJSON(content(GET(new_ag_surveys$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
#                                                                 as_tibble() %>%
#                                                                 # need to use magrittr pipe for the period syntax to work
#                                                                 mutate(across(everything(), ~str_squish(.)))  %>%
#                                                                 mutate(authoring_entity = ifelse("affiliation" %in% names(.) & affiliation!="", paste0(name, " (", affiliation, ")"), name)) |> 
#                                                                 # remove empty rows
#                                                                 filter(authoring_entity!="") |> 
#                                                                 summarise(authoring_entity = paste0(authoring_entity, collapse = ", ")) |> 
#                                                                 pull()))
#   }
#   
# }
# 
# # Append to dataset
# new_fao_metadata <- new_fao_metadata %>%
#   bind_rows(study)
# # Insert brief pause in code to not look like a robot to the API
# Sys.sleep(sample(seq(0,0.3,by=0.001),1))
# # Increment progress bar
# setTxtProgressBar(pb, i)
#}
#
#close(pb) # Close the connection
#
## Append new metadata to old metadata in same df, then export. So if we run again, won't have to rerun metadata
#all_fao_metadata <- all_fao_metadata |>
#  bind_rows(new_fao_metadata)
#
## Save copy of FAO metadata so we don't have to rerun all entries, just new ones
#saveRDS(all_fao_metadata, file = "Input/fao_ag_survey_metadata.rds")

# Set up final list of Agricultural Surveys
agri_survey <- agri_survey_raw %>%
  left_join(all_fao_metadata, by="id") |> 
  mutate(year_end = as.numeric(year_end),
         iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         instrument_type = "Agricultural Survey/Census",
         status = "completed",
         source = "https://microdata.fao.org/index.php/catalog") %>%
  # Filter for study type and repositories
  # Add AGRISurvey info from Tawheeda and LSMS+ Ag modules
  bind_rows(tribble(
    ~title, ~nation, ~iso3c, ~year_end, ~instrument_type, ~status, ~source, ~study_type, ~repositoryid,
    "Encuesta Nacional Agropecuaria", "Costa Rica", "CRI", 2019, "Agricultural Survey/Census", "Completed", "http://www.fao.org/documents/card/en/c/cb3976en", "Agriculture Integrated Survey[AGRISurvey]", "agricultural-surveys",
    "AGRISurvey Nation-wide", "Nepal", "NPL", 2020, "Agricultural Survey/Census", "Completed", "http://www.fao.org/in-action/agrisurvey/country-work/nepal/en/", "Agriculture Integrated Survey[AGRISurvey]", "agricultural-surveys",
    "Annual Agricultural Survey 2019-2020", "Senegal", "SEN", 2020, "Agricultural Survey/Census", "Completed", "http://www.fao.org/in-action/agrisurvey/country-work/senegal/en/", "Agriculture Integrated Survey[AGRISurvey]", "agricultural-surveys",
    "National Panel Survey, 2019-2020", "Uganda", "UGA", 2020, "Agricultural Survey/Census", "Completed", "https://www.worldbank.org/en/programs/lsms/initiatives/lsms-ISA#46", "Living Standards Measurement Study [hh/lsms]", "agricultural-surveys"
  )) %>%
  filter(repositoryid %in% c("agricultural-surveys", "50x2030"),
         !study_type %in% c("Administrative Records", "Agricultural Census [ag/census]", "Enterprise Census [en/census]", 
                            "Population and Housing Census [hh/popcen]", "Living Standards Measurement Study [hh/lsms]", 
                            "Income/Expenditure/Household Survey [hh/ies]", "Socio-Economic/Monitoring Survey [hh/sems]",
                            "Other Household Survey [hh/oth]", "Integrated Survey (non-LSMS) [hh/is]"), 
         !str_detect(title, "mpact|roduction|ensus|oldings|Good Growth Plan|Stock|isheries|ivestock|oultry|Corn|Fish|fish|Avian|Aquaculture|Losses|Conservation|Pesticide|Commodities")) %>%
  select(id, country = nation, iso3c, year = year_end, instrument_name = title, repositoryid, instrument_type, status, source, authoring_entity, 
         study_type, unit_of_analysis, data_kind, universe, producers, authoring_entity_detail, funding_agencies)

# export distinct study type / unit of analysis groupings data for Lorenz to classify with "keep" variable manually
# agri_survey |> group_by(study_type, unit_of_analysis) |> summarise(n=n()) |> write.csv("Output/misc_data/ag_survey_study_info.csv")

# import ag survey filtering from Lorenz
ag_filter_info <- read_csv("Output/misc_data/ag_survey_study_info_LN.csv", show_col_types = F) |> select(study_type:keep, -n)

# merge above with survey df, filter for those where keep = 1
agri_survey_filtered <- left_join(agri_survey, ag_filter_info, by = c("study_type", "unit_of_analysis")) |> filter(keep==1)

# export clean dataset
openxlsx::write.xlsx(agri_survey_filtered, "Output/instrument_data_all_years/ag_surveys.xlsx")
