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
