# Gender Data Compass and Clearinghouse gender data channel Instrument Inventory
A repository to automate creating an inventory of statistical instruments, starting with gender-relevant instruments for the [Gender Data Compass](https://genderdatacompass.org/) and [Clearinghouse for Financing Development Data](https://smartdatafinance.org/)

## Data Sources:
### Household Health Surveys (DHS & MICS)
Exact specifications in script [instrument_inventory_master.R](https://github.com/lnoe10/instrument_inventory/blob/main/instrument_inventory_master.R)  
Data Source: API pulls information from UNICEF [MICS](https://mics.unicef.org/surveys) & USAID [DHS](https://dhsprogram.com/data/available-datasets.cfm)

### Household Income/Expenditure Surveys (HIES)
Exact specifications in script [instrument_inventory_master.R](https://github.com/lnoe10/instrument_inventory/blob/main/instrument_inventory_master.R)  
Data Source: API pulls LSMS information from World Bank [microdata catalog](https://microdata.worldbank.org/index.php/catalog/lsms)

### Labor Force Surveys (LFS)
Exact specifications in script [instrument_inventory_master.R](https://github.com/lnoe10/instrument_inventory/blob/main/instrument_inventory_master.R)  
Data Source: API pulls information from ILO [Survey Catalogue](https://www.ilo.org/surveyLib/index.php/catalog/LFS)

### Ag Surveys and Census
Exact specifications in script [instrument_inventory_master.R](https://github.com/lnoe10/instrument_inventory/blob/main/instrument_inventory_master.R)  
Data Source: API pulls information from FAO [Food and Agriculture Microdata Catalog](https://microdata.fao.org/index.php/catalog) and supplements with manual upload of [World Census of Agriculture data](https://www.fao.org/world-census-agriculture/wcarounds/wca2020/countries2020/en/) for 2010 and 2020.

### Time Use Surveys
Exact specifications in script [instrument_inventory_master.R](https://github.com/lnoe10/instrument_inventory/blob/main/instrument_inventory_master.R)  
Data Source: Manual lookup of [UNSD website](https://unstats.un.org/unsd/gender/timeuse) and manual ODW check of NSO websites.

### Population and Housing Census
Exact specifications in script [instrument_inventory_master.R](https://github.com/lnoe10/instrument_inventory/blob/main/instrument_inventory_master.R)  
Data Source: Use [script census dates scrap.R](https://github.com/lnoe10/instrument_inventory/blob/main/Census%20dates%20scrape.R) file to import data on population and housing censuses from UNSD. Then filtered to drop Housing only censuses. Supplemented with manual check for 2023 census operations without specific dates in order to verify whether census is ongoing or has been conducted. UNSD census dates website as of 22 August 2023.

### Supplemental Surveys (IHSN)
Exact specifications in script [instrument_inventory_master.R](https://github.com/lnoe10/instrument_inventory/blob/main/instrument_inventory_master.R)  
Data Source: API pulls information from [IHSN](https://catalog.ihsn.org/catalog)  
IHSN data supplemented for different survey types if existing repositories above do not contain a survey for a year yet.
