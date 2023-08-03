### script to scrape Agricultural Survey data from the FAO

library(httr)
library(jsonlite)
library(tidyverse)

# First 
agri_survey_raw <- fromJSON(content(GET("https://microdata.fao.org/index.php/api/catalog/search?ps=10000"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  mutate(api_call = str_c("https://microdata.fao.org/index.php/api/catalog/", id, "?id_format=id"))

# acquire more information on surveytypes from metadata

# Initialize empty dataset
all_fao_metadata <- tibble()

# Set up progress bar
n_iter <- length(agri_survey_raw$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(agri_survey_raw$id)){
  # Set up tibble with basic info
  study <- tibble(id = agri_survey_raw$id[i], study_type = NA_character_)
  # Determine length of list generated from API call to filter out
  # Where we hit error otherwise of subscript being out of bounds
  study <- study %>%
    mutate(length = case_when(
      !is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)),
      TRUE ~ NA_integer_))
  # If list is deep enough, proceed to extract data
  if(study$length > 3){
    study <- study %>%
      # Test first whether response is valid based on tunneling into list
      mutate(study_type = ifelse(is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
                                 fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name),
             analysis_unit = ifelse(is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit), NA_character_,
                                    fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit),
             data_kind = ifelse(is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
                                fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind),
             universe = ifelse(is_empty(fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$universe), NA_character_,
                               fromJSON(content(GET(agri_survey_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$universe))
  }
  
  # Append to dataset
  all_fao_metadata <- all_fao_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close the connection

##### ADDITIONAL METADATA
# Authoring Entity Metadata
fromJSON(content(GET(str_c("https://microdata.fao.org/index.php/api/catalog//", agri_survey_raw$id[366], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
  as_tibble() %>%
  # need to use magrittr pipe for the period syntax to work
  mutate(across(everything(), ~str_squish(.)))  %>%
  mutate(authoring_entity = ifelse("affiliation" %in% names(.), paste0(name, " (", affiliation, ")"), name)) |> 
  summarise(authoring_entity = paste0(authoring_entity, collapse = ", ")) |> 
  pull()

# Keywords
fromJSON(content(GET(str_c("https://microdata.fao.org/index.php/api/catalog//", agri_survey_raw$id[10], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$keywords |> 
  as_tibble() |> select(keyword) |> summarise(keywords = paste0(keyword, collapse=", ")) |> pull()
####


# Save copy of FAO metadata so we don't have to rerun all entries, just new ones
saveRDS(all_fao_metadata, file = "Input/fao_ag_survey_metadata.rds")










# Load study descriptions from saved file
# fao_study_description <- readRDS("Input/fao_microdata_study_description.rds")

all_fao_metadata <- readRDS("Input/fao_ag_survey_metadata.rds")

# Set up final list of Agricultural Surveys
agri_survey <- fromJSON(content(GET("https://microdata.fao.org/index.php/api/catalog/search?ps=10000"), "text"), flatten = TRUE)$result$rows %>%
  as_tibble() %>%
  #left_join(fao_study_description) %>%
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
    "Encuesta Nacional Agropecuaria", "Costa Rica", "CRI", 2019, "Agricultural Survey/Census", "Completed", "http://www.fao.org/documents/card/en/c/cb3976en", "Agriculture Integrated Survey[AGRISurvey]", "agriculture-census-surveys",
    "AGRISurvey Nation-wide", "Nepal", "NPL", 2020, "Agricultural Survey/Census", "Completed", "http://www.fao.org/in-action/agrisurvey/country-work/nepal/en/", "Agriculture Integrated Survey[AGRISurvey]", "agriculture-census-surveys",
    "Annual Agricultural Survey 2019-2020", "Senegal", "SEN", 2020, "Agricultural Survey/Census", "Completed", "http://www.fao.org/in-action/agrisurvey/country-work/senegal/en/", "Agriculture Integrated Survey[AGRISurvey]", "agriculture-census-surveys",
    "National Panel Survey, 2019-2020", "Uganda", "UGA", 2020, "Agricultural Survey/Census", "Completed", "https://www.worldbank.org/en/programs/lsms/initiatives/lsms-ISA#46", "Living Standards Measurement Study [hh/lsms]", "agriculture-census-surveys"
  )) %>%
  filter(repositoryid == "agriculture-census-surveys", 
         !study_type %in% c("Administrative Records", "Agricultural Census [ag/census]", "Enterprise Census [en/census]", "Population and Housing Census [hh/popcen]"), 
         !str_detect(title, "mpact|roduction")) %>%
  select(country = nation, iso3c, year = year_end, instrument_name = title, instrument_type, status, source, authoring_entity, study_type, analysis_unit, data_kind, universe)
