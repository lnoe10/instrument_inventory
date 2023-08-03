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
all_wb_metadata <- tibble()

# Set up progress bar
n_iter <- length(lsms_raw$id)
pb <- txtProgressBar(min = 1,      # Minimum value of the progress bar
                     max = n_iter, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

# Loop
for (i in 1:length(lsms_raw$id)){
  # Set up tibble with basic info
  study <- tibble(id = lsms_raw$id[i], study_type = NA_character_)
  # Determine length of list generated from API call to filter out
  # Where we hit error otherwise of subscript being out of bounds
  study <- study %>%
    mutate(length = case_when(
      !is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)) ~ vec_depth(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)),
      TRUE ~ NA_integer_))
  # If list is deep enough, proceed to extract data
  if(study$length > 3){
    study <- study %>%
      # Test first whether response is valid based on tunneling into list
      mutate(study_type = ifelse(is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name), NA_character_,
                                 fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$series_statement$series_name),
        data_kind = ifelse(is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind), NA_character_,
                            fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind),
        unit_of_analysis = ifelse(is_empty(fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit), NA_character_,
                                  fromJSON(content(GET(lsms_raw$api_call[i]), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit))
  }
  # Append to dataset
  all_wb_metadata <- all_wb_metadata %>%
    bind_rows(study)
  # Insert brief pause in code to not look like a robot to the API
  Sys.sleep(sample(seq(0,0.3,by=0.001),1))
  # Increment progress bar
  setTxtProgressBar(pb, i)
}

# close the connection
close(pb)

# drop length variable
all_wb_metadata <- all_wb_metadata %>% select(-length)

# save copy of WB study descriptions so we don't have to rerun all entries, just new ones
saveRDS(all_wb_metadata, file = "Input/all_wb_metadata.rds")

# Load study descriptions from saved file
all_wb_metadata <- readRDS("Input/all_wb_metadata.rds")

# Set up final list of LSMS/HIES
lsms <- lsms_raw %>%
  # Duplicate Serbia and Montenegro and assign to Serbia and Montenegro respectively
  filter(nation != "Serbia and Montenegro") %>%
  bind_rows(
    lsms_raw %>% 
      filter(str_detect(nation, "Serbia and Montenegro")) %>% 
      uncount(2, .id = "id") %>%
      mutate(nation = case_when(
        id == 1 ~ "Serbia",
        TRUE ~ "Montenegro"
      )) %>%
      select(-id)
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
  left_join(all_wb_metadata) %>%
  # Filter for correct repository (lsms) and add survey types
  filter(repositoryid %in% c("lsms") | study_type %in% c("Income/Expenditure/Household Survey [hh/ies]",
                                                         "Living Standards Measurement Study",
                                                         "Living Standards Measurement Study [hh/lsms]",
                                                         "Socio-Economic/Monitoring Survey [hh/sems]",
                                                         "Income/Expenditure/Household Survey",
                                                         "Socio-Economic/Monitoring Survey",
                                                         "Core Welfare Indicators Questionnaire [hh/cwiq]")) %>%
  # Keep relevant variables
  select(id, idno, country = nation, iso3c, year, instrument_name = title, instrument_type, status, source, study_type, authoring_entity, repositoryid, data_kind, unit_of_analysis)

# filter for OGDI years of interest
lsms_clean <- lsms |> filter(year>=2013 & year<=2022)

# export filtered and full datasets
#xlsx::write.xlsx(lsms_clean, "Output/lsms_ogdi_yrs.xlsx")
#xlsx::write.xlsx(lsms, "Output/lsms_all_yrs.xlsx")

################################################

############ ILO HIES data ############

# API call, add additional variables
ilo_hies <- fromJSON(content(GET("https://www.ilo.org/surveyLib/index.php/api/catalog",
                                 query = list(ps=10000, repo="HIES")), "text"))$result$rows |>
  as_tibble() |> 
  mutate(year = as.numeric(str_extract(idno, "\\d{4}")),
         iso3c = countrycode::countrycode(nation, "country.name", "iso3c"),
         iso3c = case_when(nation == "Kosovo" ~ "XKX", TRUE ~ iso3c),
         status="completed",
         source="https://www.ilo.org/surveyLib/index.php/api/catalog")

# filter for OGDI years (2012/13-present)


# extract beginning of IDNO for both to merge on
lsms <- lsms |> mutate(short_idno = str_extract(idno, ".+?(?=_(v|V)\\d)"))
ilo_hies <- ilo_hies |> mutate(short_idno = str_extract(idno, ".+(?=_(v|V)\\d)"))

# look at overlap by short id
lsms |> select(short_idno, title=instrument_name, nation=country, year) |> left_join(ilo_hies |> select(id, title, nation, short_idno), by="short_idno") |> filter(!is.na(id))

# look at overlap by title, year, nation matching
left_join(lsms |> select(title=instrument_name, nation=country, year, idno_wb = idno),
          ilo_hies |> select(title, nation, year, idno_ilo = idno), by=c("title", "nation", "year")) |> 
  filter(!is.na(idno_ilo) & !is.na(idno_wb))

### less matches this way because of discrepancies in titles (ex ALB_2002_LSMS, LSMS has "Wave 1 Panel" in title but ILO data doesn't)
### also due to diff. in nation spelling (e.g. Bosnia-Herzegovina vs Bosnia and Herzegovina)

### note - also checked ID variable and found they are not the same across surveys. Example:
lsms_raw |> filter(str_detect(idno, "ALB_2012_LSMS")) |> pull(id) == ilo_hies |> filter(str_detect(idno, "ALB_2012_LSMS")) |> pull(id)

# ##### ADDITIONAL METADATA
# # Authoring Entity Metadata
# fromJSON(content(GET(str_c("https://www.ilo.org/surveyLib/index.php/api/catalog//", ilo_hies$id[10], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$authoring_entity |> 
#   as_tibble() %>%
#   # need to use magrittr pipe for the period syntax to work
#   mutate(across(everything(), ~str_squish(.)))  %>%
#   mutate(authoring_entity = ifelse("affiliation" %in% names(.), paste0(name, " (", affiliation, ")"), name)) |> 
#   summarise(authoring_entity = paste0(authoring_entity, collapse = ", ")) |> 
#   pull()
# 
# # Unit of Analysis Metadata
# fromJSON(content(GET(str_c("https://www.ilo.org/surveyLib/index.php/api/catalog//", ilo_hies[10], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$analysis_unit
# 
# # Data Kind
# fromJSON(content(GET(str_c("https://www.ilo.org/surveyLib/index.php/api/catalog//", ilo_hies$id[10], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$data_kind
# 
# # Keywords
# fromJSON(content(GET(str_c("https://www.ilo.org/surveyLib/index.php/api/catalog//", ilo_hies$id[10], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$keywords |> 
#   as_tibble() |> select(keyword) |> summarise(keywords = paste0(keyword, collapse=", ")) |> pull()
# 
# # Universe
# fromJSON(content(GET(str_c("https://www.ilo.org/surveyLib/index.php/api/catalog//", ilo_hies$id[10], "?id_format=id")), "text"), flatten = TRUE)$dataset$metadata$study_desc$study_info$universe
# ####