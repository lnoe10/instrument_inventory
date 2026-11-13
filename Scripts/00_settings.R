# Shared settings for the instrument inventory update.
# Sourced by 07_IHSN.R, 10_combine_instruments.R and 11_survey_type_threshold_scoring.R
# so the reference window only has to be changed in one place.

# Survey reference window (inclusive). Surveys spanning several years
# (e.g. "2015-2016") are placed in their end year.
WINDOW_START <- 2016
WINDOW_END <- 2025
WINDOW_LABEL <- paste0(WINDOW_START, "-", WINDOW_END)

# Census rounds used instead of the survey window
# UNSD 2020 round of population and housing censuses = 2015-2024
CENSUS_ROUND <- 2020
# FAO World Census of Agriculture 2020 round = 2016-2025
AG_CENSUS_ROUND <- 2020

# Standards: number of instruments over the window needed to score 100
STANDARDS <- c(
  "Agricultural Survey/Census" = 2,
  "Household Health Survey" = 4,
  "Household Income and Expenditure Survey" = 2,
  "Labour Force Survey" = 10,
  "Population Census" = 1,
  "Time Use Survey" = 2
)

# Country list: every UNSD M49 country/area (Input/unsd_codes.xlsx) plus Kosovo, Taiwan and any
# other code found in the inventory gets a row, with 0 where no instrument is found.
# Countries to leave out of the final table are applied in one step at the very end of
# 11_survey_type_threshold_scoring.R. Add ISO3 codes here once the cuts are decided.
COUNTRY_EXCLUDE <- c("ATA")  # Antarctica

# End year of a year string such as "2019", "2018-2019" or "2014-2016"
end_year <- function(year) {
  suppressWarnings(as.numeric(sub(".*([0-9]{4})$", "\\1", as.character(year))))
}

# TRUE where the (end) year falls inside the reference window
in_window <- function(year) {
  y <- end_year(year)
  !is.na(y) & y >= WINDOW_START & y <= WINDOW_END
}

# NADA catalogue search with paging ---------------------------------------------
# Some NADA catalogues cap the page size (World Bank microdata returns at most 100 rows per
# request since 2026, whatever ps is set to), so fetch page by page until all `found` rows are in.
# base: catalogue root, e.g. "https://microdata.worldbank.org"; query: extra search parameters.
nada_search <- function(base, query = list(), ps = 100, pause = 0.2) {
  get_page <- function(page) {
    r <- httr::GET(paste0(base, "/index.php/api/catalog/search"),
                   query = c(query, list(ps = ps, page = page)), httr::timeout(120))
    httr::stop_for_status(r)
    jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$result
  }
  first <- get_page(1)
  found <- as.numeric(first$found)
  pages <- list(tibble::as_tibble(first$rows))
  n_pages <- ceiling(found / NROW(first$rows))
  if (n_pages > 1) for (p in 2:n_pages) {
    Sys.sleep(pause)
    pages[[p]] <- tibble::as_tibble(get_page(p)$rows)
  }
  out <- dplyr::bind_rows(pages)
  out <- out[!duplicated(out$id), ]
  if (nrow(out) < found) warning(base, ": got ", nrow(out), " of ", found, " studies")
  out
}

# NADA study metadata, one API call per study ------------------------------------
# Returns the fields used to classify studies: study_type (series name), unit_of_analysis,
# study_scope, universe, data_kind and authoring_entity_detail ("name (affiliation), ...").
# api_call: e.g. "https://catalog.ihsn.org/index.php/api/catalog/<id>?id_format=id".
# Fields hold the first value when the metadata has several, as the earlier ifelse() code did.
nada_study_metadata <- function(id, api_call) {
  first_or_na <- function(x) if (is.null(x) || length(x) == 0 || all(is.na(unlist(x)))) NA_character_ else as.character(unlist(x)[[1]])
  study <- tibble::tibble(id = id, study_type = NA_character_, unit_of_analysis = NA_character_,
                          study_scope = NA_character_, universe = NA_character_, data_kind = NA_character_,
                          authoring_entity_detail = NA_character_)
  meta <- tryCatch(jsonlite::fromJSON(httr::content(httr::GET(api_call, httr::timeout(60)), "text", encoding = "UTF-8"), flatten = TRUE),
                   error = function(e) NULL)
  sd <- meta$dataset$metadata$study_desc
  if (is.null(sd)) return(study)
  study$study_type <- first_or_na(sd$series_statement$series_name)
  study$unit_of_analysis <- first_or_na(sd$study_info$analysis_unit)
  study$study_scope <- first_or_na(sd$study_info$study_scope)
  study$universe <- first_or_na(sd$study_info$universe)
  study$data_kind <- first_or_na(sd$study_info$data_kind)
  ae <- sd$authoring_entity
  if (is.data.frame(ae) && nrow(ae) > 0 && "name" %in% names(ae)) {
    ae[] <- lapply(ae, function(v) stringr::str_squish(as.character(v)))
    nm <- ae$name
    if ("affiliation" %in% names(ae)) nm <- ifelse(!is.na(ae$affiliation) & ae$affiliation != "", paste0(nm, " (", ae$affiliation, ")"), nm)
    nm <- nm[!is.na(nm) & nm != ""]
    if (length(nm) > 0) study$authoring_entity_detail <- paste0(nm, collapse = ", ")
  }
  study
}

# Metadata for many studies, with a short pause between calls and a progress bar
nada_metadata_for <- function(ids, api_calls, pause = 0.1) {
  purrr::map2_dfr(ids, api_calls, function(id, url) {
    Sys.sleep(pause)
    nada_study_metadata(id, url)
  }, .progress = TRUE)
}

# ILOSTAT survey sources --------------------------------------------------------
# Used by 03_HIES.R and 04_LFS.R after the ILO Survey Catalogue was retired (2026).
# Every annual ILOSTAT observation is tagged with its source survey, e.g. "LFS - EU Labour Force Survey"
# or "HIES - Living Standards Survey". Returns one row per country, source and year that appear in
# any of the indicators below. Cached in Input/ilostat_sources.rds and re-downloaded (about 240 MB)
# when the cache is missing or more than max_age_days old; refresh = TRUE forces a download.
ILOSTAT_INDICATORS <- c(
  "EAP_TEAP_SEX_AGE_NB_A",  # labour force by sex and age
  "EMP_TEMP_SEX_AGE_NB_A",  # employment by sex and age
  "UNE_DEAP_SEX_AGE_RT_A"   # unemployment rate by sex and age
)

ilostat_sources <- function(refresh = FALSE, cache = "Input/ilostat_sources.rds", max_age_days = 7) {
  if (!refresh && file.exists(cache)) {
    cached <- readRDS(cache)
    downloaded <- attr(cached, "downloaded")
    if (!is.null(downloaded) && as.numeric(Sys.Date() - downloaded) <= max_age_days) return(cached)
  }
  out <- lapply(ILOSTAT_INDICATORS, function(id) {
    f <- tempfile(fileext = ".csv")
    on.exit(unlink(f))
    r <- httr::GET(paste0("https://rplumber.ilo.org/data/indicator/?id=", id, "&lang=en&type=label&format=.csv&channel=ilostat"),
                   httr::write_disk(f, overwrite = TRUE), httr::timeout(600))
    httr::stop_for_status(r)
    d <- readr::read_csv(f, show_col_types = FALSE, col_select = c("ref_area.label", "source.label", "time"))
    d <- unique(d)
    d$indicator <- id
    d
  })
  src <- do.call(rbind, out)
  # source type is the prefix of the label: LFS, HIES, HS (other household survey), PC (census), ...
  src$source_type <- sub("^([A-Z]+) - .*$", "\\1", src$source.label)
  src$source_type[!grepl("^[A-Z]+ - ", src$source.label)] <- NA
  src$iso3c <- countrycode::countrycode(src$ref_area.label, "country.name", "iso3c", warn = FALSE)
  src$iso3c[src$ref_area.label == "Kosovo"] <- "XKX"
  src$iso3c[src$ref_area.label == "Netherlands Antilles"] <- "ANT"
  attr(src, "downloaded") <- Sys.Date()
  saveRDS(src, cache)
  src
}
