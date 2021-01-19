# Occupational Risk of COVID-19 Datatool Code Repository
#### Updated as of Jan, 19 2021
For detailed information on the background, objectives, and data sources used in the tool, please refer to the Technical Document accessed via this link: 
https://www.dropbox.com/s/69cmswjo91ynnec/occupational-occupational-exposure-covid-risk-tool-technical-notes-January-19-2021.pdf?dl=0

## Purpose of README
The purpose of this file is to describe the code used to generate datasets used in the tool. 
3 Primary data sources are used: Customized Census of Population 2016 (3 datatables); Occupational Information Network (O-NET); Essential service lists from Provincial/Territorial Governments
The code was written using R-Studio (Version 1.2.5019) & R-Shiny software.

There are no datasets in this repository.

## Code Repository
### Step1_Step2_brookfield_onet_essential_designation
Step1) Link Occupational Information Network (O-NET) measures to the National Occupational Classification (NOC)-2016 4-digit occupations used in the Census 2016 via the Brookfield Institue Standard Occupational Classification (SOC) - National Occupational Classification (NOC) crosswalk (see: https://brookfieldinstitute.ca/connecting-the-dots-linking-canadian-occupations-to-skills-data/).

Step2) Designate Essential Services, mapped to the North American Industry Classification System (NAICS)-2012 4-digit industries used in the Census 2016 for each Province/Territory.

### Step3_MapEssential_To_Census
Step3) Link Essential Services to the customized Census 2016 Tables

### Step4_Province_Datatool_Create
Step4) Create Province level datasets (Table 1 and Table 2 from Census 2016) used in the Tool. Final dataset is mapped to Essential Service and O-NET measures from Step1,2, & 3.

### Step5_Regions_Datatool_Create
Step5) Create Regional level datasets (Table 3 from Census 2016) used in the Tool. Final dataset is mapped to Essential Service and O-NET measures from Step1,2, & 3.

### Step6_ComparisonsTab_Datatool_Create
Step6) Create Comparisons Tab datasets, looking at O-NET measures mapped to the Census 2016 across NAICS-2012.

### Occup_Covid19_App.Rmd
R Markdown datafile using R-Shiny to create the Occupational Risk of COVID-19 datatool. 
