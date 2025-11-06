## script to get census dates

library(rvest)
library(tidyverse)

setwd("C:/Users/loren/Documents/GitHub/instrument_inventory/")

# Define url
ag_census <- "https://www.fao.org/world-census-agriculture/wca-round/status-of-world-censuses-of-agriculture/en"

# Import WB income and region groups groups. From https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
wb_codes <- read_csv("Input/wb_countries_fy26.csv", show_col_types = F) |> 
  janitor::clean_names() |>
  filter(!is.na(region)) |>
  # Venezuela is not classified in FY2022. Manually assign to last year's income classification "UM"
  mutate(income_group = fct_relevel(income_group, "Low income", "Lower middle income", "Upper middle income", "High income"))

#####
# Solution here: https://stackoverflow.com/questions/57829762/web-scraping-columns-from-web-with-r

# Create vector of regions:
regions <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

# Download page
agcensus_webpage <- ag_census |>
  read_html()

# define empty tibble
df <- tibble()

pre_xpath <- '/html/body/div/main/div/div[5]/div['
middle_xpath <- ']/div[2]/div/div/table/tbody[2]/tr['

# Loop through rows. Max number of rows set by region with most entries.
# Was Africa in this case. Check xpath of last line of table manually
for (r in 1:length(regions)){
  
  for (i in 1:56){
  
    country <- tibble(countryname = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[1]', sep = ""))))==0, NA, 
                                         html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[1]', sep = "")))),
                    round1930 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[2]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[2]', sep = "")))),
                    round1950 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[3]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[3]', sep = "")))),
                    round1960 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[4]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[4]', sep = "")))),
                    round1970 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[5]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[5]', sep = "")))),
                    round1980 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[6]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[6]', sep = "")))),
                    round1990 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[7]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[7]', sep = "")))),
                    round2000 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[8]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[8]', sep = "")))),
                    round2010 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[9]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[9]', sep = "")))),
                    round2020 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[10]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[10]', sep = "")))),
                    round2030 = ifelse(length(html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[11]', sep = ""))))==0, NA,
                                       html_text(html_nodes(agcensus_webpage, xpath = str_c(pre_xpath, r, middle_xpath, i, ']/td[11]', sep = "")))))
  
  df <- df |> rbind(country)
  
  }
}

# Clean up
df_clean <- df |>
  # Remove leading and trailing white-space
  mutate_all(str_squish) |>
  # drop rows where everything is null
  janitor::remove_empty(which = "rows") |> 
  # Reshape dataframe to long, with every census date or round as one row
  pivot_longer(-countryname, names_to = "ag_census_round", values_to = "date") |> 
  # Create variables of interest, such as year by extracting four number year
  # Extract year
  mutate(year = as.numeric(str_extract(date, "[0-9]{4}")),
  # Couple of cases where census starts in one year, ends the next
  # Count cases of year occurence, increment first year by 1 to reflect 
  # 2nd year, since we are mainly interested in how many years it's been 
  # since the last census, which is where later dates will be more accurate.
         count_year = str_count(date, "[0-9]{4}"),
  count_year = case_when(str_detect(date, "/") ~ 2,
  TRUE ~ count_year),
         year = case_when(
           count_year == 2 ~ year + 1,
           TRUE ~ year
         ),
  # Create iso code. Netherlands Antilles will be without code (no longer exists)
  # And therefore will not have any info on lending groups and regions
  iso3c = countrycode::countrycode(countryname, "country.name", "iso3c"),
  iso3c = case_when(
    countryname == "Eswatini" ~ "SWZ",
    countryname == "Kosovo" ~ "XKX",
    countryname == "Saint-Martin" ~ "MAF",
    TRUE ~ iso3c
  ),
  did_agcensus_take_place = case_when(
    !is.na(year) ~ TRUE,
    TRUE ~ FALSE
  )) |>
  # Merge in region and income groups
  left_join(wb_codes |> rename(iso3c=code)) |> 
  rename(country = countryname) |>
  mutate(ag_census_round = str_remove(ag_census_round, "round"))

# Save copy of census scrape to load into main instrument inventory
saveRDS(df_clean, file = "Input/ag_census_dates_df.rds")
