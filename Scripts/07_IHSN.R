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
# length(ihsn_raw$id)
for (i in 1:200){
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
saveRDS(all_study_metadata, "Input/ihsn_metadata.rds")

## Load study descriptions from saved file
ihsn_study_description <- readRDS("Input/ihsn_microdata_study_description.rds")

# Final list of IHSN, to be used to supplement other survey groups as appropriate
ihsn <- ihsn_raw %>%
  left_join(ihsn_study_description) %>%
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
                            "Event/transaction data [evn]"
  ))