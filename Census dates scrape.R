library(rvest)
library(tidyverse)

setwd("C:/Users/lnoe/Documents/R")

census <- "https://unstats.un.org/unsd/demographic-social/census/censusdates/"

# Import ODW income and region groups groups. From https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
odw_master_codes <- read_csv("Data/Input Data/2021 ODW Country and Region Codes.csv") %>% 
  janitor::clean_names() %>% 
  # Clear out extra lines at the bottom that just contain notes
  filter(!is.na(country_name)) %>%
  # Clear out duplicate Faroe Islands
  distinct(iso_3_code, .keep_all = TRUE) %>%
  select(iso3c = iso_3_code, country = country_name, odw_region_name, un_code = un_m49_code,
         incgroup = world_bank_income_group_name, lending_cat = world_bank_lending_code_july_2020,
         wbregion = world_bank_all_income_region_names) %>%
  mutate(un_code = as.numeric(un_code), 
         incgroup = fct_relevel(incgroup, "Low income", "Lower middle income", "Upper middle income", "High income"))


#####
# Solution here: https://stackoverflow.com/questions/57829762/web-scraping-columns-from-web-with-r

# Create vector of regions:
regions <- c("Africa", "North", "South", "Asia", "Europe", "Oceania")

# Download page
webpage <- census %>%
  read_html()

# Initialize empty dataframe
df <- data.frame()

# Loop through rows. Max number of rows set by region with most entries.
# Was Europe in this case. Check xpath of last line of table manually
for (r in regions){
for (i in 1:66){
  country <- setNames(data.frame(cbind(
    html_text(html_nodes(webpage, xpath = str_c('//*[@id="',r,'"]/div[',i,']/div[1]', sep = ""))),
    html_text(html_nodes(webpage, xpath = str_c('//*[@id="',r,'"]/div[',i,']/div[2]', sep = ""))),
    html_text(html_nodes(webpage, xpath = str_c('//*[@id="',r,'"]/div[',i,']/div[3]', sep = ""))),
    html_text(html_nodes(webpage, xpath = str_c('//*[@id="',r,'"]/div[',i,']/div[4]', sep = ""))),
    html_text(html_nodes(webpage, xpath = str_c('//*[@id="',r,'"]/div[',i,']/div[5]', sep = "")))
  )),c("countryname", "round1990", "round2000", "round2010", "round2020"))

df <- df %>%
  rbind(country)
  
}}

# Clean up
df <- df %>%
  # Remove headers that have been imported with first line of every region
  mutate(countryname = str_remove(countryname, "(AFRICA|AMERICA, NORTH|AMERICA, SOUTH|ASIA|EUROPE|OCEANIA)\\s*Countries or areas"),
         round1990 = str_remove(round1990, "1990 round\\(1985-1994\\)"),
         round2000 = str_remove(round2000, "2000 round\\(1995-2004\\)"),
         round2010 = str_remove(round2010, "2010 round\\(2005-2014\\)"),
         round2020 = str_remove(round2020, "2020 round\\(2015-2024\\)")) %>%
  # Remove leading and trailing white-space
  mutate_all(str_trim) %>%
  # Come up with count of characters in countryname column to replace with NA
  mutate(num_char = nchar(countryname),
         countryname = case_when(
           num_char == 0 ~ NA_character_,
           TRUE ~ countryname
         )) %>%
  # Fill countryname with name of country before if missing
  fill(countryname, .direction = "down") %>%
  # Drop num_char variable, don't need it anymore
  select(-num_char) %>%
  # Fix Great Britain. All four component countries have the same dates
  # BUt GB has no data. So drop UK, Scotland, Northern Ireland and rename
  # England and Wales to United Kingdom
  filter(!countryname %in% c("United Kingdom", "- Scotland", "- Northern Ireland")) %>%
  mutate(countryname = str_replace(countryname, "- England and Wales", "United Kingdom")) %>%
  # Reshape dataframe to long, with every census date or round as one row
  pivot_longer(-countryname, names_to = "census_round", values_to = "date") %>%
  # Create variables of interest, such as year by extracting four number year
  # Extract year
  mutate(year = as.numeric(str_extract(date, "[0-9]{4}")),
  # Couple of cases where census starts in one year, ends the next
  # Count cases of year occurence, increment first year by 1 to reflect 
  # 2nd year, since we are mainly interested in how many years it's been 
  # since the last census, which is where later dates will be more accurate.
         count_year = str_count(date, "[0-9]{4}"),
         year = case_when(
           count_year == 2 ~ year + 1,
           TRUE ~ year
         ),
  # Create binary indicator for whether a census is planned in the future
  # or happened in the past.
  planned = case_when(
    str_detect(date, "[0-9]{2,}\\)$") ~ 1,
    TRUE ~ 0
  ),
  # Annotate dates using footnotes section
  notes = case_when(
    str_detect(date, "\\(P\\)") ~ "Population census only.",
    str_detect(date, "\\(H\\)") ~ "Housing census only.",
    str_detect(date, "\\(1\\)") ~ "Urban census only.",
    str_detect(date, "\\(2\\)") ~ "Enumeration of settled population was in November 1986 and of nomads in February 1987.",
    str_detect(date, "\\(3\\)") ~ "Population figures compiled from administrative registers.",
    # If country has population compiled from admin registers, apply note to census
    str_detect(countryname, "\\(3\\)") ~ "Population figures compiled from administrative registers.",
    str_detect(date, "\\(4\\)") ~ "Population figures compiled from administrative registers in combination with other sources of data, such as sample surveys.",
    # If country has population compiled from admin registers, apply note to census
    str_detect(countryname, "\\(4\\)") ~ "Population figures compiled from administrative registers in combination with other sources of data, such as sample surveys.",
    str_detect(date, "\\(5\\)") ~ "The population by-censuses for 1986 and 1996 were based on one-in-seven sample of the population, while that for 2006 was based on one-in-ten sample of the population.",
    str_detect(date, "\\(6\\)") ~ "Enumeration of former Yemen Arab Republic.",
    str_detect(date, "\\(7\\)") ~ "Enumeration of former Democratic Yemen.",
    str_detect(date, "\\(8\\)") ~ "Through accession of the German Democratic Republic to the Federal Republic of Germany with effect from 3 October 1990, the two German States have united to form one sovereign State. As from the date of unification, the Federal Republic of Germany acts in the United Nations under the designation 'Germany'.",
    str_detect(date, "\\(9\\)") ~ "Enumeration of former Federal Republic of Germany.",
    str_detect(date, "\\(10\\)") ~ "Combined with agricultural census.",
    str_detect(date, "\\(11\\)") ~ "No formal census conducted. A count of numbers of each family group by name, sex, age and whether permanent or expatriate resident is made on 30 or 31 December each year.",
    str_detect(date, "\\(12\\)") ~ "A register-based test census was conducted on 5 December 2001 on a sample of 1.2% of the population.",
    str_detect(date, "\\(13\\)") ~ "Due to the circumstances, the census was conducted again in 2004.",
    str_detect(date, "\\(14\\)") ~ "Census not carried out on the territory of Kosovo and Metohia.",
    str_detect(date, "\\(15\\)") ~ "Rolling Census based on continuous sample survey.",
    str_detect(date, "\\(16\\)") ~ "Census was planned to be conducted using staggered enumerations province by province. At the end of 2014, only 6 of the 34 provinces had been enumerated.",
    str_detect(date, "\\(17\\)") ~ "Traditional decennial census with full field enumeration, and a continuous sample survey.",
    str_detect(date, "\\(18\\)") ~ "Population figures compiled from administrative registers and sample surveys while data on housing characteristics are collected through full field enumeration.",
    str_detect(date, "\\(19\\)") ~ "Cancelled.",
    TRUE ~ NA_character_
  ),
  # Clean countryname variable, several countries have footnotes in 
  # the name about their pop numbers being from admin registers
  # From review of website, doesn't seem like those countries with it in their
  # countryname also have additional footnotes in their dates
  countryname = str_remove(countryname, "\\(3\\)|\\(4\\)"),
  # Create iso code. Netherlands Antilles will be without code (no longer exists)
  # And therefore will not have any info on lending groups and regions
  iso3c = countrycode::countrycode(countryname, "country.name", "iso3c"),
  iso3c = case_when(
    countryname == "Eswatini" ~ "SWZ",
    countryname == "Saint-Martin" ~ "MAF",
    TRUE ~ iso3c
  )) %>%
  # Drop years where there was no census (census == NA)
  # This drops Afghanistan 2010 obs, where census is incomplete
  # North Macedonia 2010, which was cancelled
  # Andorra, San Marino and Holy See obs (which have censuses in other year)
  # but who only have info that "Population figures compiled from administrative registers."
  filter(!is.na(year)) %>%
  # Merge in region and income groups
  left_join(odw_master_codes)

# Create df of years since last census
last_census <- df %>%
  # Keep only last year of actual census
  filter(planned == 0) %>%
  group_by(iso3c) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  # Append using planned censuses
  rbind(df %>% filter(planned == 1)) %>%
  # For countries with multiple censuses in one year, only keep one
  distinct(iso3c, year, .keep_all = TRUE) %>%
  arrange(iso3c, year) %>%
  # Reshape into one obs per country
  select(countryname, iso3c, year, planned, wbregion, incgroup, lendingcat) %>%
  pivot_wider(names_from = planned, names_prefix = "year", values_from = year) %>%
  # Years since last census
  # Whether or not a census is planned for 2020 round.
    # 1 means yes, it's planned
    # 0 means none is planned
    # Blank means none is planned but the last census was in the 2020 round
    # and therefore we don't know about plans for the 2030 round yet.
  mutate(time_since_last = 2020 - year0,
         planned = case_when(
           !is.na(year1) ~ 1,
           is.na(year1) & year0 >= 2015 ~ NA_real_,
           TRUE ~ 0
         )) %>%
  rename(last_census = year0, planned_census = year1)


# Export to CSV and share with Eric
df %>%
  # Add a space in front of dates so Excel doesn't auto-format
  mutate(date = str_c(" ", date),
         # Add a plus in front of future dates (2020) for example
         # So excel doesn't think it's a minus (accounting format)
         date = case_when(
           str_detect(date, "\\([0-9]{4}\\)") ~ str_c("+", date),
           TRUE ~ date
         )) %>%
  select(countryname, iso3c, wbregion, incgroup, lending_cat, census_round, date, year, planned, notes) %>%
  arrange(iso3c, year) %>%
  write_csv("Data/Output Data/full census dataset_2Feb.csv", na = "")
last_census %>%
  arrange(iso3c) %>%
  write_csv("Data/Output Data/last census.csv", na = "")
