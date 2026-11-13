### script to scrape HIES data from LSMS and ILO

library(httr)
library(jsonlite)
library(tidyverse)
library(openxlsx)

# nada_search() and ilostat_sources() helpers
source("Scripts/00_settings.R")

############ LSMS - WORLD BANK ############

# scrape data from API (paged: the catalogue returns at most 100 studies per request)
lsms_raw <- nada_search("https://microdata.worldbank.org") %>%
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

# metadata for new studies is appended to the cache. Each study's metadata is fetched once.
# (Previously every field made its own API call, each iteration overwrote the result so only the
# last new study was kept, and an empty new_lsms made 1:nrow() loop over 1 and 0.)
first_or_na <- function(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_character_ else as.character(x[[1]])

wb_study_metadata <- function(id, api_call) {
  study <- tibble(id = id, study_type = NA_character_, data_kind = NA_character_,
                  unit_of_analysis = NA_character_, authoring_entity_detail = NA_character_)
  meta <- tryCatch(fromJSON(content(GET(api_call, timeout(60)), "text", encoding = "UTF-8"), flatten = TRUE),
                   error = function(e) NULL)
  sd <- meta$dataset$metadata$study_desc
  if (is.null(sd)) return(study)
  study$study_type <- first_or_na(sd$series_statement$series_name)
  study$data_kind <- first_or_na(sd$study_info$data_kind)
  study$unit_of_analysis <- first_or_na(sd$study_info$analysis_unit)
  # authoring entity: "name", optionally with "affiliation"
  ae <- sd$authoring_entity
  if (!is.null(ae) && length(ae) > 0) {
    ae <- as_tibble(ae) |> mutate(across(everything(), ~str_squish(as.character(.))))
    if (!"name" %in% names(ae)) ae$name <- NA_character_
    if ("affiliation" %in% names(ae)) ae <- ae |> mutate(name = ifelse(!is.na(affiliation) & affiliation != "", paste0(name, " (", affiliation, ")"), name))
    ae <- ae |> filter(!is.na(name), name != "")
    if (nrow(ae) > 0) study$authoring_entity_detail <- paste0(ae$name, collapse = ", ")
  }
  study
}

new_wb_metadata <- map2_dfr(new_lsms$id, new_lsms$api_call, \(id, url) {
  Sys.sleep(0.1)
  wb_study_metadata(id, url)
}, .progress = TRUE)

all_wb_metadata_new <- all_wb_metadata |>
  bind_rows(new_wb_metadata)

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
openxlsx::write.xlsx(lsms, "Output/instrument_data_all_years/lsms.xlsx")


################ ILO HIES data ################

# The ILO Survey Catalogue (www.ilo.org/surveyLib) was retired in 2026 ("410 Gone").
# 1. Snapshot: the ILO HIES surveys that were kept in hies.xlsx in Oct 2025, i.e. after the manual
#    duplicate checks against LSMS (Output/misc_data/hies_additional_ilo_data*.xlsx/csv).
# 2. ILOSTAT: surveys ILOSTAT tags as "HIES - ..." for any country-year not already covered.

# extract beginning of IDNO to compare LSMS and ILO studies
lsms <- lsms |> mutate(short_idno = str_extract(idno, ".+?(?=_(v|V)\\d)"))

ilo_hies <- read_csv("Input/ilo_hies_snapshot_2025.csv", show_col_types = FALSE,
                     col_types = cols(.default = "c", id = "d", year = "d")) |>
  # an LSMS study added since Oct 2025 may be the same survey: drop the ILO row (same short IDNO and year)
  anti_join(lsms |> filter(!is.na(short_idno)) |> select(short_idno, year), by = c("short_idno", "year"))

hies_all <- lsms |>
  bind_rows(ilo_hies)

# ILOSTAT HIES sources, one row per country, survey and year
ilostat_hies <- ilostat_sources() |>
  filter(source_type == "HIES", !is.na(iso3c)) |>
  distinct(country = ref_area.label, iso3c, survey = str_remove(source.label, "^HIES - "), year = as.numeric(time)) |>
  # only add country-years that LSMS or the ILO snapshot do not already have
  anti_join(hies_all |> distinct(iso3c, year), by = c("iso3c", "year")) |>
  mutate(instrument_name = str_c(survey, " ", year),
         instrument_type = "HIES",
         status = "completed",
         source = "https://ilostat.ilo.org/data/",
         type = "ILOSTAT source") |>
  select(-survey)

hies_all <- hies_all |>
  bind_rows(ilostat_hies)

# export combined data to excel
writexl::write_xlsx(hies_all, "Output/instrument_data_all_years/hies.xlsx")
