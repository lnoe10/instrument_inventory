# Gender Data Compass and Clearinghouse gender data channel Instrument Inventory
A repository to automate creating an inventory of statistical instruments, starting with gender-relevant instruments for the [Gender Data Compass](https://genderdatacompass.org/) and [Clearinghouse for Financing Development Data](https://smartdatafinance.org/)

## 2026 update
- **Reference window:** surveys conducted 2016–2025. Surveys spanning several years (e.g. 2018–2019) count in their end year.
- **Censuses:** population censuses are scored on the UNSD 2020 census round (2015–2024) and agricultural censuses on the FAO World Census of Agriculture 2020 round (2016–2025), not on the survey window.
- **Status:** only completed instruments count. Phone, COVID-19 and high-frequency surveys never count.
- **Standards** (instruments over the window needed to score 100%): Agricultural survey/census 2, Household health survey 4, HIES 2, Labour force survey 10, Population census 1, Time use survey 2.
- Window, census rounds, standards and country groups are set in one place: [Scripts/00_settings.R](Scripts/00_settings.R).

## Running the pipeline
Run the scripts in [Scripts](Scripts) in order, from the repository root:
- `01`–`09` pull each source and write one file per instrument type to `Output/instrument_data_all_years/`.
- `10_combine_instruments.R` filters to the reference window, removes IHSN rows that duplicate another source, and writes `Output/instrument_inventory_final_2016_2025.xlsx`.
- `11_survey_type_threshold_scoring.R` scores each country against the standards and writes the survey count tables in `Output/`.

Metadata for catalogue studies is cached in `Input/*.rds`, so a re-run only fetches studies that are new since the last run.

## Data sources (2026)
### Household Health Surveys (DHS & MICS)
Scripts: [01_MICS.R](Scripts/01_MICS.R), [02_DHS.R](Scripts/02_DHS.R)
- **DHS:** [DHS Program API](https://api.dhsprogram.com/rest/dhs/surveys?surveyStatus=all) (USAID). Service Provision Assessments, Special DHS and most "Other" surveys are excluded.
- **MICS:** survey list downloaded from [UNICEF MICS](https://mics.unicef.org/surveys) and saved as `Input/mics_surveys_catalogue.csv`. Only national surveys are kept.

### Household Income and Expenditure Surveys (HIES)
Script: [03_HIES.R](Scripts/03_HIES.R)
- **World Bank:** LSMS and other household surveys from the [World Bank Microdata Library](https://microdata.worldbank.org/index.php/catalog/lsms) API. Since 2026 the API returns at most 100 studies per request, so results are fetched page by page.
- **ILO:** the ILO Survey Catalogue was retired in 2026. ILO HIES surveys come from:
  1. `Input/ilo_hies_snapshot_2025.csv`: the ILO HIES surveys kept in the October 2025 update, after manual duplicate checks against LSMS.
  2. [ILOSTAT](https://ilostat.ilo.org/data/): surveys ILOSTAT tags as "HIES" sources, added only for country-years not already covered.
- Monitoring, evaluation, phone, refugee and project surveys are dropped by the shared rules in `00_settings.R`.

### Labour Force Surveys (LFS)
Script: [04_LFS.R](Scripts/04_LFS.R)
- **[ILOSTAT](https://ilostat.ilo.org/data/)** replaces the retired ILO Survey Catalogue. Each annual ILOSTAT observation names the survey it comes from. The inventory has one LFS per country, survey and year with ILOSTAT labour force, employment or unemployment data, so quarterly or monthly rounds in a year count once.
- The download (about 240 MB) is cached in `Input/ilostat_sources.rds` and refreshed when the cache is more than 7 days old.

### Agricultural Surveys and Censuses
Scripts: [05_ag_surveys.R](Scripts/05_ag_surveys.R), [06_ag_censuses.R](Scripts/06_ag_censuses.R)
- **Surveys:** [FAO Food and Agriculture Microdata Catalogue](https://microdata.fao.org/index.php/catalog) API, plus two AGRISurvey rounds added by hand (Costa Rica 2019, Nepal 2020). Each survey round counts once; modules of one survey are not counted separately.
- **Censuses, 2020 round:** FAO's [WCA 2020 round page](https://www.fao.org/world-census-agriculture/wca-round/wca-round-2020/en). A core module plus supplementary modules counts as one census, and an agricultural module in a population and housing census counts as an agricultural census.
- **Censuses, other rounds:** FAO's [status of World Censuses of Agriculture](https://www.fao.org/world-census-agriculture/wca-round/status-of-world-censuses-of-agriculture/en) page.

### Time Use Surveys (TUS)
Script: [09_TUS_and_census.R](Scripts/09_TUS_and_census.R)
- **UNSD time use survey catalogue, June 2026** (`Input/TUSurveyInfo202606_web.xlsx`), from [UNSD time use data and metadata](https://unstats.un.org/UNSDWebsite/demographic-social/time-use/data-and-metadata). It replaces the UN DESA ArcGIS download, which no longer works. Replace the file when UNSD publishes a newer version.
- Earlier lists (the 2023 UNSD data and the original manual NSO search) fill in surveys the UNSD catalogue does not have.

### Population and Housing Censuses
Scripts: [08_census_dates_scrape.R](Scripts/08_census_dates_scrape.R), [09_TUS_and_census.R](Scripts/09_TUS_and_census.R)
- **[UNSD census dates](https://unstats.un.org/unsd/demographic-social/census/censusdates/)**, scraped by `08_census_dates_scrape.R`. Housing-only censuses are dropped.
- **Kosovo:** its 2024 Population and Housing Census is added by hand from the [Kosovo Agency of Statistics](https://ask.rks-gov.net/), since UNSD does not list Kosovo.

### Supplemental Surveys (IHSN)
Script: [07_IHSN.R](Scripts/07_IHSN.R)
- **[IHSN catalogue](https://catalog.ihsn.org/catalog)** API, used to add surveys of the scored types that the sources above do not list.
- Studies are classified by earlier manual reviews where available, and otherwise by the rules in `00_settings.R`. Rule-classified studies of a scored type are written to `Output/misc_data/ihsn_unclassified_2016-2025.xlsx` for a manual check.
- Labour force surveys are not taken from IHSN (ILOSTAT is the LFS source). IHSN rows that duplicate a survey from another source are removed in `10_combine_instruments.R`.

### Country lists and regions
- **IDA countries:** World Bank FY26 [country and lending groups](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups), IDA and Blend (78 countries), in `Input/wb_countries_fy26.csv`.
- **ODIN countries:** countries covered by the Open Data Inventory, in `Input/odin_countries.csv`.
- **Regions:** UN M49 regions (Africa, Americas, Asia, Europe, Oceania), from `Input/unsd_codes.xlsx`.

## Outputs
- `Output/instrument_inventory_final_2016_2025.xlsx`: every instrument counted in the 2016–2025 update.
- `Output/survey counts ODIN countries <date>.csv`: survey counts and scores for all ODIN countries.
- `Output/survey counts for clearinghouse <date>.csv`: survey counts and scores for IDA countries, plus regional averages over IDA countries.
