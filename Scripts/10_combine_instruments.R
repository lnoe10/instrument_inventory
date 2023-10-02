# script to combine all scraped instrument data into one spreadsheet
library(tidyverse)

# read in all prepared instrument data (except ihsn because we are dealing with that separately in case of overlap)
mics <- readxl::read_xlsx("Output/instrument_data_all_years/mics.xlsx") |> mutate(year=as.character(year))
dhs <- readxl::read_xlsx("Output/instrument_data_all_years/dhs.xlsx") |> mutate(year=as.character(year))
lfs <- readxl::read_xlsx("Output/instrument_data_all_years/lfs.xlsx") |> mutate(year=as.character(year))
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
ihsn <- readxl::read_xlsx("Output/instrument_data_ogdi_years/ihsn_2013-2022.xlsx")

# drop "Good Growth Plan" rows as decided by Tawheeda and Lorenz
ihsn <- ihsn |> filter(!(authoring_entity=="Syngenta" & str_detect(instrument_name, "Good Growth Plan")))

# add single year variable to ihsn dataframe for consistency with others
ihsn_clean <- ihsn |> mutate(across(c(year_start, year_end), ~ as.numeric(.x))) |> 
  mutate(year = case_when(year_start==year_end ~ as.character(year_start),
                          year_start!=year_end ~ paste0(year_start, "-", year_end))) 

# we need to now look for overlap in the ihsn data with the rest of the instrument inventory data to avoid double-counting surveys

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

# filter out study types related to labour force surveys (based on discussion w/ Lorenz and Tawheeda)
ihsn_clean <- ihsn_clean |> 
  filter(!(study_type %in% c("Encuesta Continua de Empleo", 
                             "Encuesta Continua de Empleo (ECE)",
                             "Encuesta de empleo [hh/lfs]",
                             "Labor Force Survey",
                             "Labor Force Survey [hh/lfs]",
                             "Labour Force Survey [hh/lfs]")))

# export clean ihsn data
# xlsx::write.xlsx(as.data.frame(ihsn_clean), "Output/instrument_data_ogdi_years/ihsn_2013-2022.xlsx", row.names = FALSE)

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
full_instruments_df <- readxl::read_xlsx("Output/instrument_data_ogdi_years/full_instruments_df.xlsx")

# filter out all food insecurity experience scale surveys and IFAD impact assessment surveys
filtered_instruments_df <- full_instruments_df |> 
  filter(!str_detect(instrument_name, "Food Insecurity Experience Scale")) |> 
  filter(!str_detect(instrument_name, "IFAD Impact Assessment Surveys"))

# add keyword flag to identify impact evaluation surveys
# define keywords
keywords <- c("(I|i)mpact","(B|b)ase-?line","(M|m)id-?line","(E|e)nd-?line", "(E|e)vidence",
              "(M|m)onitor","(A|a)ssessment", "(E|e)valuation")

# add flag for keywords
filtered_instruments_df <- filtered_instruments_df |> mutate(keyword_flag = ifelse(str_detect(instrument_name, paste0(keywords, collapse = "|")), 1, 0))

# export rows w/ keyword flag so that we can manually check and assign keep status
# filtered_instruments_df |> filter(keyword_flag==1) |> 
#   select(country, year, instrument_name, instrument_type, keyword_flag) |>
#   distinct() |> 
#   xlsx::write.xlsx("Output/misc_data/keyword_matches.xlsx")

# read in keyword matches spreadsheet with "keep" variable filled in
keyword_matches <- readxl::read_xlsx("Output/misc_data/keyword_matches.xlsx") |> select(-`...1`)

# left join w/ filtered_instruments_df on all vars and filter for keep==TRUE, then drop the keep variable and the keyword variable
filtered_instruments_df <- left_join(filtered_instruments_df, keyword_matches, by=c("country", "year", "instrument_name", "instrument_type", "keyword_flag")) |> 
  filter(keep!=FALSE | is.na(keep)) |> 
  select(-c(keep, keyword_flag))

# we need to drop rows with possible duplicates that weren't previously identified
# grab # of rows of distinct country/year/instrument_name combos by counting # of rows where index for that group == 1
# this is the # of rows we want to have after we do our filtering for duplicates, so save it and we will check against this after filtering
nrow_distinct <- filtered_instruments_df |> group_by(country, year, instrument_name) |> mutate(index = 1:n()) |> filter(index==1) |> nrow()

# there are duplicate Ag Censuses for Congo and Cote d'Ivoire we identified (rows have diff census rounds, correct one is 2020) - let's fix those manually here
filtered_instruments_df <- filtered_instruments_df |> 
  mutate(census_round = ifelse(instrument_name=="Agriculture Census" & iso3c=="CIV" & year=="2016", "2020", census_round),
         census_round = ifelse(instrument_name=="Agriculture Census" & iso3c=="COG" & year=="2015", "2020", census_round)) |> 
  distinct()

# group by country, year and instrument name, and choose the first available - where IHSN is one of the entries, choose the row from the other source
filtered_instruments_df_new <- filtered_instruments_df |> group_by(country, year, instrument_name) |>
  mutate(index = 1:n()) |> 
  mutate(both_ihsn = ifelse(all(str_detect(source, "ihsn")), 1, 0),
         one_ihsn = ifelse(both_ihsn==0 & any(str_detect(source, "ihsn")), 1, 0)) |> 
  select(country:id, authoring_entity, study_type:authoring_entity_detail, census_round, date, index, both_ihsn, one_ihsn, rationale, status_note) |> 
  # keep rows that are not a duplicate (i.e. only one item in the group)
  filter((n()==1) | 
           # where there are multiple rows in the group and neither are IHSN, take the first in the group
           (both_ihsn==0 & one_ihsn==0 & index==1 & n()>1) | 
           # where there are multiple rows in the group and both are IHSN, take the first in the group
           (both_ihsn==1 & one_ihsn==0 & index==1 & n()>1) | 
           # where there are multiple rows in the group and only one is IHSN, take the one that is not IHSN
           (both_ihsn==0 & one_ihsn==1 & !str_detect(source, "ihsn") & n()>1)) |> 
  select(-c(index, both_ihsn, one_ihsn))
  
# check that the number of rows post-filtering matches our expectation from before
stopifnot(nrow(filtered_instruments_df_new)==nrow_distinct)

# make sure no duplicates are left
stopifnot(filtered_instruments_df_new |> group_by(country, year, instrument_name) |> filter(n()>1) |> nrow()==0)

# create smaller df with variable for rows with a wave or round specification by string extraction
# doing this so we can more carefully collapse rows in a separate dataframe from the main one
waves <- filtered_instruments_df_new |> 
  mutate(wave_or_round = case_when(str_detect(instrument_name, "Waves ") ~ str_extract(instrument_name, "Waves \\d( and |-)\\d"),
                                   str_detect(instrument_name, "Wave ") ~ str_extract(instrument_name, "Wave \\d"),
                                   str_detect(instrument_name, "Rounds ") ~ str_extract(instrument_name, "Rounds \\d-\\d"),
                                   str_detect(instrument_name, "Round ") ~ str_extract(instrument_name, "Round \\d{1,2}"),
                                   .default = NA)) |> 
  filter(!is.na(wave_or_round)) |> 
  select(country:instrument_type, wave_or_round)

# export instruments with multiple waves so we can manually collapse into single rows
# waves |> arrange(country, year, instrument_name) |> xlsx::write.xlsx("Output/misc_data/waves.xlsx")

# read in waves dataframe with collapsed instrument names
waves <- readxl::read_xlsx("Output/misc_data/waves.xlsx", sheet=1)

# after merging and filtering out extra wave rows, we expect the number of rows to equal the current # minus the number of rows
# which have been combined with other instruments of the same survey/country group that are not the very first row of that grouping
nrows_waves_collapsed <- nrow(filtered_instruments_df_new)-(waves |> filter(`combined?`=="yes") |> group_by(iso3c, combined_title) |> mutate(index=1:n()) |> filter(index!=1) |> nrow())

# we want to keep the rows where combined?==no, and then keep one of the rows from each country/year/combined_title grouping when combined?==yes
filtered_instruments_df_new <- left_join(filtered_instruments_df_new, waves, 
                                         by=c("country", "iso3c", "year", "status", "instrument_name", "instrument_type")) |> 
  # for rows where we want the rows to be collapsed b/c of waves, replace instrument_name with combined_title
  mutate(instrument_name = ifelse(!is.na(wave_or_round) & `combined?`=="yes", combined_title, instrument_name)) |>
  group_by(country, instrument_name, combined_title, `combined?`) |> 
  # create index for groupings
  mutate(index = 1:n()) |> 
  # create boolean keep variable to maintain rows where wave/round wasn't detected in the name, rows where wave/round was detected but we do not want to collapse,
  # and the first instrument belonging to each group of rows which we want to collapse together (we do this to maintain metadata for that instrument)
  mutate(keep = case_when(`combined?`=="no" ~ TRUE,
                          is.na(`combined?`) ~ TRUE,
                          `combined?`=="yes" & index==1 ~ TRUE,
                          `combined?`=="yes" & index>1 ~ FALSE)) |> 
  ungroup() |> 
  filter(keep==TRUE) |> 
  select(-c(keep, index))

# check that the number of rows is as expected
stopifnot(nrow(filtered_instruments_df_new)==nrows_waves_collapsed)

# filter out poultry/palay production type surveys
filtered_instruments_df_new <- filtered_instruments_df_new |> filter(!str_detect(instrument_name, "Poultry|Impact Assessment|Palay|Cassava|Sweet Potato"))

# check that for country/year groups where we detected at least one instrument with "wave" or "round" in the name,
# there is not another instrument for that country/year with the same name that isn't attributed to a specific wave or round
# if there is, we should drop the row where wave or round was detected b/c that would be considered a duplicate (b/c it represents the same survey)
# filtered_instruments_df_new |> group_by(country) |> filter(any(!is.na(`combined?`))) |> select(country:instrument_type, wave_or_round:note) |> View()

# detected 3 duplicates in the manually looking through the df produced by lined 246-247 - let's remove manually here, keeping the one that represents the full survey rather than one wave
filtered_instruments_df_new <- filtered_instruments_df_new |> 
                          # drop this because we also have "National Panel Survey 2014-2015"
  mutate(drop = case_when(country=="Tanzania" & year=="2014" & instrument_name=="National Panel Survey 2014-2015, Wave 4" ~ TRUE,
                          # drop this because we also have have "Household Risk and Vulnerability Survey, Full Panel 2016-2018"
                          country=="Nepal" & year=="2016" & instrument_name=="Household Risk and Vulnerability Survey 2016, Wave 1" ~ TRUE,
                          # drop this because we also have "Rural Livelihood Survey 2012-2013"
                          country=="Malawi" & year=="2013" & instrument_name=="Rural Livelihood Survey 2012-2013, Round 2" ~ TRUE,
                          .default = FALSE)) |> 
  filter(drop==FALSE) |> select(-c(drop, wave_or_round, `combined?`, combined_title))

# look at distinct instrument types
# filtered_instruments_df_new |> select(instrument_type) |> distinct()

# standardize instrument types to full names
filtered_instruments_df_new <- filtered_instruments_df_new |> 
  mutate(instrument_type = case_when(instrument_type=="Household health survey" ~ "Household Health Survey",
                                     instrument_type=="Labour force surveys" ~ "Labour Force Survey",
                                     instrument_type=="LFS" ~ "Labour Force Survey",
                                     instrument_type=="Labour force survey" ~ "Labour Force Survey",
                                     instrument_type=="HIES" ~ "Household Income and Expenditure Survey",
                                     # found one row where instrument type is NA, assign manually
                                     instrument_name=="National Socio-Economic Survey 2015, March (Core)" & country=="Indonesia" ~ "Other household survey",
                                     .default = instrument_type))

# standardize completed status for consistency
filtered_instruments_df_new  <- filtered_instruments_df_new |> mutate(status = ifelse(str_detect(status, "completed||Complete"), "Completed", status))

###### export dataframe before doing more filtering ######
xlsx::write.xlsx(filtered_instruments_df_new |> arrange(country, instrument_name, year), "Output/instrument_inventory_filtered.xlsx")
##########################################################

################## read in the dataframe with added "drop" column ##################
# this was to check for duplicates manually
df <- readxl::read_xlsx("Output/instrument_inventory_filtered_deduped.xlsx")

# clean up year based on remaining comments left by Tawheeda
df_final <- df |> mutate(year = case_when(drop=="This should be 2017" ~ "2017",
                                          drop=="This should be 2013" ~ "2013",
                                          drop=="This should be 2014" ~ "2014",
                                          drop=="Change to 2018 for year" ~ "2018",
                                          .default = year)) |> 
  # convert drop to all binary 0/1 for filtering
  mutate(drop = case_when(is.na(drop) ~ "0",
                          drop=="This should be 2017" ~ "0",
                          drop=="This should be 2013" ~ "0",
                          drop=="This should be 2014" ~ "0",
                          drop=="Change to 2018 for year" ~ "0",
                          .default = drop),
         drop = as.numeric(drop)) |> 
  filter(drop==0) |> 
  select(-drop)

