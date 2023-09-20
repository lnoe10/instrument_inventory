# script to combine all scraped instrument data into one spreadsheet

library(tidyverse)

# read in all prepared instrument data (except ihsn because we are dealing with that separately in case of overlap)
mics <- readxl::read_xlsx("Output/instrument_data_all_years/mics.xlsx") |> mutate(year=as.character(year))
dhs <- readxl::read_xlsx("Output/instrument_data_all_years/dhs.xlsx") |> mutate(year=as.character(year))
lfs <- readxl::read_xlsx("Output/instrument_data_all_years/lfs_all.xlsx") |> mutate(year=as.character(year))
hies <- readxl::read_xlsx("Output/instrument_data_all_years/hies.xlsx") |> mutate(year=as.character(year))
ag_survey <- readxl::read_xlsx("Output/instrument_data_all_years/ag_surveys.xlsx") |> mutate(year=as.character(year))
ag_census <- readxl::read_xlsx("Output/instrument_data_all_years/ag_census.xlsx") |> mutate(year=as.character(year))
tus <- readxl::read_xlsx("Output/instrument_data_all_years/tus.xlsx") |> mutate(year=as.character(year))
census <- readxl::read_xlsx("Output/instrument_data_all_years/census.xlsx") |> mutate(year=as.character(year))

######### FILTER FOR OGDI / CENSUS YEARS #########

# 2013-2022 / 2012/13-2021/22 for non-calendar years, 2010 and 2020 census rounds
dhs_clean <- dhs |> filter(year>=2013 & year<=2022) |> select(-`...1`)
mics_clean <- mics |> filter(year>=2013 & year<=2022) |> select(-`...1`)
hies_clean <- hies |> filter(year>=2013 & year<=2022) |> select(-`...1`)
lfs_clean <- lfs |> filter(year>=2013 & year<=2022) |> select(-`...1`)
ag_survey_clean <- ag_survey |> filter(year>=2013 & year<=2022) |> select(-`...1`)
tus_clean <- tus |> filter(year>=2013 & year<=2022) |> select(-`...1`)
dhs_clean <- dhs |> filter(year>=2013 & year<=2022) |> select(-`...1`)
ag_census_clean <- ag_census |> 
  filter(census_round==2010|census_round==2020) |> select(-`...1`)
census_clean <- census |> 
  filter(census_round==2010|census_round==2020) |> select(-`...1`)

##################################################

###### Export filtered individual datasets ######
xlsx::write.xlsx(as.data.frame(dhs_clean), "Output/instrument_data_ogdi_years/dhs_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(mics_clean), "Output/instrument_data_ogdi_years/mics_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(hies_clean), "Output/instrument_data_ogdi_years/hies_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(lfs_clean), "Output/instrument_data_ogdi_years/lfs_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(ag_survey_clean), "Output/instrument_data_ogdi_years/ag_survey_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(tus_clean), "Output/instrument_data_ogdi_years/tus_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(dhs_clean), "Output/instrument_data_ogdi_years/dhs_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(ag_census_clean), "Output/instrument_data_ogdi_years/ag_census_2013-2022.xlsx", row.names = FALSE)
xlsx::write.xlsx(as.data.frame(census_clean), "Output/instrument_data_ogdi_years/census_2013-2022.xlsx", row.names = FALSE)

##################################################

# combining all the FILTERED datasets, with census round variable - we will use this to look for duplicates with IHSN data
all_surveys_census_filtered <- list(dhs_clean, mics_clean, tus_clean) |>
  map(~select(., country, iso3c, year, status, instrument_name, instrument_type, source)) |>
  map_dfr(bind_rows) |>
  bind_rows(hies_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, idno, authoring_entity)) |>
  bind_rows(lfs_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, idno, authoring_entity)) |>
  bind_rows(ag_survey_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, authoring_entity)) |>
  bind_rows(census_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, census_round)) |>
  bind_rows(ag_census_clean |> select(country, iso3c, year, status, instrument_name, instrument_type, source, census_round))

#################################################

# incorporating IHSN data
# import the 2013-2022 IHSN data that Lorenz has manually classified
ihsn <- readxl::read_xlsx("Output/instrument_data_ogdi_years/ihsn_2013-2022.xlsx") |> select(-`...1`)

# drop "Good Growth Plan" rows as decided by Tawheeda and Lorenz
ihsn <- ihsn |> filter(!(authoring_entity=="Syngenta" & str_detect(instrument_name, "Good Growth Plan")))

# add single year variable to ihsn dataframe for consistency with others
ihsn_clean <- ihsn |> mutate(across(c(year_start, year_end), ~ as.numeric(.x))) |> 
  mutate(year = case_when(year_start==year_end ~ as.character(year_start),
                          year_start!=year_end ~ paste0(year_start, "-", year_end))) 

# ihsn_clean <- ihsn |> mutate(across(c(year_start, year_end), ~ as.numeric(.x))) |> 
#   mutate(year = case_when(year_start==year_end ~ as.character(year_start),
#                           year_start!=year_end & year_end-year_start==1 ~ paste0(year_start, "/", str_extract(year_end, "\\d{2}$")),
#                           year_start!=year_end & year_end-year_start>1 ~ paste0(year_start, "-", year_end))) 

# we need to now look for overlap in the ihsn data with the rest of the instrument inventory data 
# to avoid double-counting surveys
# note that to do this we need to merge with year, but this is tricky since ihsn data comes with a year start and year end variable
# upon manual inspection of the rest of the instrument inventory data, the year chosen is the end year of surveys 
# (see by looking at instrument names with a time frame in the title) - so, we will merge on the year_end variable

# there are 5 rows in the all_surveys_census_filtered dataframe where a timeframe is given in the year variable
# those are all TUS, so we can inspect them manually - there are no overlaps we need to be concerned about
all_surveys_census_filtered |> filter(!str_detect(year, "^\\d{4}$")) |> select(-census_round, -source) |> 
  left_join(ihsn_clean |> select(country, iso3c, year_start, year_end, instrument_name_ihsn = instrument_name, instrument_type) |> 
              filter(instrument_type=="Time Use Survey"), by=c("iso3c", "instrument_type", "country")) |> 
  filter(!is.na(instrument_name_ihsn))

# merge on year, country and instrument type, then drop rows where there was no match by filtering for rows where instrument name ihsn is not null
# left_join(all_surveys_census_filtered |> select(-census_round, -source, -status),
#           ihsn_clean |> select(country, iso3c, year_ihsn = year, instrument_name_ihsn = instrument_name, instrument_type, idno),
#           by = c("iso3c", "country", "instrument_type"), multiple="all") |> 
#   filter(!is.na(instrument_name_ihsn)) |> 
#   # very important - we are not doing a single year to year comparison b/c IHSN data has a start and end year
#   # so, we concatenate them, and str_detect if the year from the instrument inventory dataframe is detected in the concatenated IHSN years
#   filter(str_detect(year_ihsn, year)==TRUE) |> 
#   select(idno_ihsn = idno, iso3c, country, year_ihsn, year, instrument_type, instrument_name, instrument_name_ihsn) |> 
#   mutate(duplicate = ifelse(instrument_name_ihsn==instrument_name, TRUE, "")) |> 
#   # export rows where possible overlap has been identified so we can manually inspect these
#   xlsx::write.xlsx("Output/misc_data/ihsn_possible_overlap.xlsx")

# read in the possible overlap file with classifications added by Lorenz and Lindsay
ihsn_possible_overlap <- readxl::read_xlsx("Output/misc_data/ihsn_possible_overlap.xlsx") |> select(-note)

# drop rows with an idno equal to one of those in the ihsn_possible_overlap df identified as a possible duplicate
ihsn_clean <- ihsn_clean |> filter(!(idno %in% c(ihsn_possible_overlap |> filter(possible_duplicate==TRUE) |> select(idno_ihsn) |> distinct() |> pull())))

# look for additional overlap through reference ID - this will let us catch duplicates that weren't identified by the merge done above
more_overlap <- left_join(all_surveys_census_filtered |> mutate(idno_caps = str_to_upper(idno)), 
                          ihsn_clean |> mutate(idno_caps = str_to_upper(idno)) |> select(idno_caps, instrument_name, authoring_entity_detail, idno_ihsn = idno), 
          by="idno_caps") |> filter(!is.na(instrument_name.y)) |> 
  select(idno_ihsn) |> pull()

# remove the reference IDs identified above from the ihsn_clean dataframe
ihsn_clean <- ihsn_clean |> filter(!(idno %in% more_overlap))

# export clean ihsn data
xlsx::write.xlsx(as.data.frame(ihsn_clean), "Output/instrument_data_ogdi_years/ihsn_2013-2022.xlsx", row.names = FALSE)

#########################################################
############## putting everything together ##############

# let's create a dataframe with all instruments data that has all possible variables
# this will help with specific instrument filtering later on
full_instruments_df <- bind_rows(dhs_clean, mics_clean, tus_clean, hies_clean |> mutate(id=as.character(id)), 
                                 lfs_clean |> mutate(year_start = as.numeric(year_start), year_end = as.numeric(year_end)), 
                                 ag_survey_clean, census_clean, ag_census_clean, ihsn_clean |> select(-keep)) |> 
  select(-c(keep, notes, country_original)) |> arrange(country)

# export this dataframe for later use
# xlsx::write.xlsx(as.data.frame(full_instruments_df), "Output/instrument_data_ogdi_years/full_instruments_df.xlsx", row.names = FALSE)

#########################################################
######### filtering out irrelevant instruments ##########

# read in full instruments df
full_instruments_df  <- readxl::read_xlsx("Output/instrument_data_ogdi_years/full_instruments_df.xlsx")

# filter out all food insecurity experience scale surveys and IFAD impact assessment surveys
filtered_instruments_df <- full_instruments_df |> 
  filter(!str_detect(instrument_name, "Food Insecurity Experience Scale")) |> 
  filter(!str_detect(instrument_name, "IFAD Impact Assessment Surveys"))
  
# filter out all impact evaluation surveys (keywords: baseline, midline, endline, evidence)
filtered_instruments_df |> filter(str_detect(instrument_name, "Impact Evaluation|Baseline|Midline|Endline|Evidence")) |> View()

# filter out poultry/palay production type surveys
all_surveys_census_filtered |> filter(str_detect(instrument_name, "Poultry|Impact Assessment|Palay"))

# collapse surveys w/ multiple waves into one instrument if they have the same year (keywords: wave, round)
all_surveys_census_filtered |> filter(str_detect(instrument_name, "(W|w)ave|(R|r)ound"))

# # Export
# ### NOTE - I am modifying this code with an if-else to allow country names that do not have an iso3c code to be maintained
# all_surveys_census %>%
#   filter(year>=2010, year<=2020) %>%
#   rename(country_orig = country) %>%
#   mutate(country = ifelse(!is.na(iso3c), countrycode::countrycode(iso3c, "iso3c", "country.name"), country_orig),
#          country = case_when(
#            iso3c == "ANT" ~ "Netherland Antilles",
#            iso3c == "XKX" ~ "Kosovo",
#            TRUE ~ country
#          ), .before = iso3c) %>%
#   arrange(iso3c, year, instrument_type) %>%
#   select(-country_orig) |> 
#   xlsx::write.xlsx("Output/instrument_inventory.xlsx")