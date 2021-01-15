####################
## Occupational Risk Covid19 Code Repository

#######################
## Step5: Create Datatool Datasets for health regions (table3)
#######################

library(dplyr)
library(scales)
library(tidyverse)
rm(list=ls())
load("table3_r.RData")
load("onet_naics_noc.RData")
load("table3_median_income.RData")

##########################################
## Sum NOC codes within services
##########################################
table3_r_tool <- table3_r %>% 
  group_by(geography, health_region, essential, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)

######################################################################
## create designations for industry variable for each service strategy
######################################################################
table3_r_tool$industry <- ifelse(table3_r_tool$essential==1, "Essential", "Non-essential")
table3_r_tool <- table3_r_tool %>%
  ungroup() %>% 
  select(-essential)

###################################################
## now create an "all occupations" dataset 
## which includes both essential and other services
###################################################
table3_r_tool_all <- table3_r_tool %>% 
  group_by(geography, health_region, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)
table3_r_tool_all$industry <- c("Total")
##### View(sum_all)

############################
## append the datasets to have essential, non-essential, and all
############################
table3_r_tool_f <- rbind(table3_r_tool, table3_r_tool_all)

#############################
## socio-dem characteristics
#############################

table3_r_tool_f$sum_nonimmig1 <- ifelse(table3_r_tool_f$sum_total1 >=(table3_r_tool_f$sum_immig1 + table3_r_tool_f$sum_nonpermres1),table3_r_tool_f$sum_total1 - (table3_r_tool_f$sum_immig1 + table3_r_tool_f$sum_nonpermres1),0)

table3_r_tool_f$sum_white1 <- ifelse(table3_r_tool_f$sum_total1 >=(table3_r_tool_f$sum_vismin1 + table3_r_tool_f$sum_aboriginal1),table3_r_tool_f$sum_total1 -(table3_r_tool_f$sum_vismin1 + table3_r_tool_f$sum_aboriginal1),0)

table3_r_tool_f$percent_immig <- ifelse(table3_r_tool_f$sum_total1>0, table3_r_tool_f$sum_immig1 / table3_r_tool_f$sum_total1*100, 0)
table3_r_tool_f$percent_immig <- ifelse(table3_r_tool_f$percent_immig>100, 100, table3_r_tool_f$percent_immig)

table3_r_tool_f$percent_nonpermres <- ifelse(table3_r_tool_f$sum_total1>0, table3_r_tool_f$sum_nonpermres1 / table3_r_tool_f$sum_total1*100, 0)
table3_r_tool_f$percent_nonpermres <- ifelse(table3_r_tool_f$percent_nonpermres>100, 100, table3_r_tool_f$percent_nonpermres)

table3_r_tool_f$percent_vismin <- ifelse(table3_r_tool_f$sum_total1>0, table3_r_tool_f$sum_vismin1 / table3_r_tool_f$sum_total1*100, 0)
table3_r_tool_f$percent_vismin <- ifelse(table3_r_tool_f$percent_vismin>100, 100, table3_r_tool_f$percent_vismin)

################################################################################
## create overall female and over 65 percents (for SLIDER in population group)
################################################################################
female_slider <- table3_r_tool_f %>% 
  filter(age == "Total - 15 years and over") %>% 
  select(geography,health_region,industry,noc_code,sex,age,sum_total1) %>%
  spread(sex,sum_total1) 
female_slider$Female <- ifelse(is.na(female_slider$Female),0,female_slider$Female)
female_slider$`Total - Sex` <- ifelse(is.na(female_slider$`Total - Sex`),0,female_slider$`Total - Sex`)
female_slider$overall_percent_female <- ifelse(female_slider$`Total - Sex` >0,female_slider$Female/female_slider$`Total - Sex`*100,0)
female_slider$overall_percent_female <- ifelse(female_slider$overall_percent_female>100, 100, female_slider$overall_percent_female)
female_slider <- female_slider %>% ungroup() %>% distinct(geography,health_region,industry,noc_code,overall_percent_female)
# # View(female_slider)                                                    

age65_slider <- table3_r_tool_f %>% 
  filter(sex == "Total - Sex") %>% 
  select(geography,health_region,industry,noc_code,sex,age,sum_total1) %>%
  spread(age,sum_total1) 
age65_slider$`65 years and over` <- ifelse(is.na(age65_slider$`65 years and over`),0,age65_slider$`65 years and over`)
age65_slider$`Total - 15 years and over` <- ifelse(is.na(age65_slider$`Total - 15 years and over`),0,age65_slider$`Total - 15 years and over`)
age65_slider$overall_percent_65 <- ifelse(age65_slider$`Total - 15 years and over` >0,age65_slider$`65 years and over`/age65_slider$`Total - 15 years and over`*100,0)
age65_slider$overall_percent_65 <- ifelse(age65_slider$overall_percent_65>100, 100, age65_slider$overall_percent_65)
age65_slider <-age65_slider %>% ungroup() %>% distinct(geography,health_region,industry,noc_code,overall_percent_65)
# # View(age65_slider)

sliders <- merge(female_slider,age65_slider,by=c('geography','health_region', 'industry','noc_code'),all=T)
sliders$overall_percent_female <- ifelse(is.na(sliders$overall_percent_female),0,sliders$overall_percent_female)
sliders$overall_percent_65 <- ifelse(is.na(sliders$overall_percent_65),0,sliders$overall_percent_65)
# # View(sliders)

table3_datatool <- merge(table3_r_tool_f,sliders,by=c('geography','health_region', 'industry','noc_code'),all=T)
table3_datatool$overall_percent_female <- ifelse(is.na(table3_datatool$overall_percent_female),0,table3_datatool$overall_percent_female)
table3_datatool$overall_percent_65 <- ifelse(is.na(table3_datatool$overall_percent_65),0,table3_datatool$overall_percent_65)

###########################################
## MERGE WITH OCCUPATION MEASURES from ONET
############################################
# # View(onet)
table3_datatool <- merge(table3_datatool,onet, by=c("noc_code"),all.x=T)

###########################################
## MERGE WITH INCOME
############################################
table3_median_income <- table3_median_income %>% select(-noc_code_class)
# # View(table3_median_income)
table3_median_income$noc_code <- as.numeric(table3_median_income$noc_code)
table3_median_income$noc_code<-formatC(table3_median_income$noc_code, width = 4, format = "d", flag = "0")
table3_median_income$noc_code<-as.character(table3_median_income$noc_code)

table3_datatool  <- merge(table3_datatool,table3_median_income , by=c("health_region","noc_code"),all.x=T)

table3_datatool  <- table3_datatool %>% dplyr::rename("median_income"="median_total1")
# # View(table3_datatool)

###########################################
## MERGE WITH NOC_MERGE & SAVE
############################################
table3_datatool <- merge(table3_datatool,NOC_MERGE,by="noc_code",all.x=T)
table3_datatool <- table3_datatool %>% 
  select(geography,health_region,industry,noc_broad,noc_broad_descript,noc_code,noc_code_class,sex,age,everything()) %>% 
  arrange(geography,health_region,industry,noc_code,sex,age) %>%
  mutate_if(is.numeric, round, 0)

table3_datatool <- table3_datatool%>%mutate(noc_code_class=substring(noc_code_class,6))
table3_datatool<-table3_datatool%>%mutate(noc_code_class=gsub("\\s*\\([^\\)]+\\)","",as.character(noc_code_class)))
table3_datatool<-table3_datatool%>%mutate(noc_code_class= gsub('[0-9]+', '', noc_code_class))

table3_datatool <- table3_datatool%>%mutate(health_region=substring(health_region,6))

##########################################
## Apply PHO Operational names for PHUs in Ontario
##########################################

table3_datatool$health_region_ontario = factor(
  table3_datatool$health_region,
  levels = c(
    'The District of Algoma Health Unit',
    'Brant County Health Unit',
    'Durham Regional Health Unit',
    'Grey Bruce Health Unit',
    'Haldimand-Norfolk Health Unit',
    'Haliburton, Kawartha, Pine Ridge District Health Unit',
    'Halton Regional Health Unit',
    'City of Hamilton Health Unit',
    'Hastings and Prince Edward Counties Health Unit',
    'Huron County Health Unit',
    'Chatham-Kent Health Unit',
    'Kingston, Frontenac and Lennox and Addington Health Unit',
    'Lambton Health Unit',
    'Leeds, Grenville and Lanark District Health Unit',
    'Middlesex-London Health Unit',
    'Niagara Regional Area Health Unit',
    'North Bay Parry Sound District Health Unit',
    'Northwestern Health Unit',
    'City of Ottawa Health Unit',
    'Peel Regional Health Unit',
    'Perth District Health Unit',
    'Peterborough County-City Health Unit',
    'Porcupine Health Unit',
    'Renfrew County and District Health Unit',
    'The Eastern Ontario Health Unit',
    'Simcoe Muskoka District Health Unit',
    'Sudbury and District Health Unit',
    'Thunder Bay District Health Unit',
    'Timiskaming Health Unit',
    'Waterloo Health Unit',
    'Wellington-Dufferin-Guelph Health Unit',
    'Windsor-Essex County Health Unit',
    'York Regional Health Unit',
    'Oxford Elgin St. Thomas Health Unit',
    'City of Toronto Health Unit'
    
  ),
  labels = c(
    
    'Algoma Public Health',
    'Brant County Health Unit',
    'Durham Region Health Department',
    'Grey Bruce Health Unit',
    'Haldimand-Norfolk Health Unit',
    'Haliburton, Kawartha, Pine Ridge District Health Unit',
    'Halton Region Public Health',
    'City of Hamilton Public Health Services',
    'Hastings Prince Edward Public Health',
    'Huron Public Health',
    'Chatham-Kent Public Health',
    'Kingston, Frontenac and Lennox & Addington Public Health',
    'Lambton Public Health',
    'Leeds, Grenville & Lanark District Health Unit',
    'Middlesex-London Health Unit',
    'Niagara Region Public Health',
    'North Bay Parry Sound District Health Unit',
    'Northwestern Health Unit',
    'Ottawa Public Health',
    'Peel Public Health',
    'Perth Public Health',
    'Peterborough Public Health',
    'Porcupine Health Unit',
    'Renfrew County and District Health Unit',
    'Eastern Ontario Health Unit',
    'Simcoe Muskoka District Health Unit',
    'Public Health Sudbury & Districts',
    'Thunder Bay District Health Unit',
    'Timiskaming Health Unit',
    'Region of Waterloo Public Health and Emergency Services',
    'Wellington-Dufferin-Guelph Public Health',
    'Windsor-Essex County Health Unit',
    'York Region Public Health',
    'Southwestern Public Health',
    'Toronto Public Health'
    
  )
)

table3_datatool$health_region <- ifelse(is.na(table3_datatool$health_region),"Peterborough Public Health",table3_datatool$health_region)

table3_datatool$health_region_ontario <- as.character(table3_datatool$health_region_ontario)
table3_datatool$health_region <- ifelse(table3_datatool$geography %in% "Ontario", table3_datatool$health_region_ontario,table3_datatool$health_region)

# View(table3_datatool)
summary(table3_datatool)
table3_datatool <- table3_datatool %>% select(-health_region_ontario)

saveRDS(table3_datatool,file = "table3_datatool.rds")



######
## Specific sectors (NAICS)
######
library(dplyr)
library(scales)
library(tidyverse)
rm(list=ls())
load("table3_r.RData")
load("onet_naics_noc.RData")
load("table3_median_income.RData")

# View(table3_r)
##########################################
## Sum NOC codes within services
##########################################
table3_r_tool <- table3_r %>% 
  group_by(geography, health_region, essential,naics_sector_name, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)
# View(table3_r)
######################################################################
## create designations for industry variable for each service strategy
######################################################################
table3_r_tool$industry <- ifelse(table3_r_tool$essential==1, "Essential", "Non-essential")
table3_r_tool <- table3_r_tool %>%
  ungroup() %>% 
  select(-essential)

# View(table3_r_tool)
###################################################
## now create an "all occupations" dataset 
## which includes both essential and other services
###################################################
table3_r_tool_all <- table3_r_tool %>% 
  group_by(geography, health_region, naics_sector_name,noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)
table3_r_tool_all$industry <- c("Total")
##### View(sum_all)

############################
## append the datasets to have essential, non-essential, and all
############################
table3_r_tool_f <- rbind(table3_r_tool, table3_r_tool_all)

# View(table3_r_tool_f)
#############################
## socio-dem characteristics
#############################

table3_r_tool_f$sum_nonimmig1 <- ifelse(table3_r_tool_f$sum_total1 >=(table3_r_tool_f$sum_immig1 + table3_r_tool_f$sum_nonpermres1),table3_r_tool_f$sum_total1 - (table3_r_tool_f$sum_immig1 + table3_r_tool_f$sum_nonpermres1),0)

table3_r_tool_f$sum_white1 <- ifelse(table3_r_tool_f$sum_total1 >=(table3_r_tool_f$sum_vismin1 + table3_r_tool_f$sum_aboriginal1),table3_r_tool_f$sum_total1 -(table3_r_tool_f$sum_vismin1 + table3_r_tool_f$sum_aboriginal1),0)

table3_r_tool_f$percent_immig <- ifelse(table3_r_tool_f$sum_total1>0, table3_r_tool_f$sum_immig1 / table3_r_tool_f$sum_total1*100, 0)
table3_r_tool_f$percent_immig <- ifelse(table3_r_tool_f$percent_immig>100, 100, table3_r_tool_f$percent_immig)

table3_r_tool_f$percent_nonpermres <- ifelse(table3_r_tool_f$sum_total1>0, table3_r_tool_f$sum_nonpermres1 / table3_r_tool_f$sum_total1*100, 0)
table3_r_tool_f$percent_nonpermres <- ifelse(table3_r_tool_f$percent_nonpermres>100, 100, table3_r_tool_f$percent_nonpermres)

table3_r_tool_f$percent_vismin <- ifelse(table3_r_tool_f$sum_total1>0, table3_r_tool_f$sum_vismin1 / table3_r_tool_f$sum_total1*100, 0)
table3_r_tool_f$percent_vismin <- ifelse(table3_r_tool_f$percent_vismin>100, 100, table3_r_tool_f$percent_vismin)

# # View(table3_r_tool_f)
################################################################################
## create overall female and over 65 percents (for SLIDER in population group)
################################################################################
female_slider <- table3_r_tool_f %>% 
  filter(age == "Total - 15 years and over") %>% 
  select(geography,health_region,industry,naics_sector_name,noc_code,sex,age,sum_total1) %>%
  spread(sex,sum_total1) 
female_slider$Female <- ifelse(is.na(female_slider$Female),0,female_slider$Female)
female_slider$`Total - Sex` <- ifelse(is.na(female_slider$`Total - Sex`),0,female_slider$`Total - Sex`)
female_slider$overall_percent_female <- ifelse(female_slider$`Total - Sex` >0,female_slider$Female/female_slider$`Total - Sex`*100,0)
female_slider$overall_percent_female <- ifelse(female_slider$overall_percent_female>100, 100, female_slider$overall_percent_female)
female_slider <- female_slider %>% ungroup() %>% distinct(geography,health_region,industry,naics_sector_name,noc_code,overall_percent_female)
# # View(female_slider)                                                    

age65_slider <- table3_r_tool_f %>% 
  filter(sex == "Total - Sex") %>% 
  select(geography,health_region,industry,naics_sector_name,noc_code,sex,age,sum_total1) %>%
  spread(age,sum_total1) 
age65_slider$`65 years and over` <- ifelse(is.na(age65_slider$`65 years and over`),0,age65_slider$`65 years and over`)
age65_slider$`Total - 15 years and over` <- ifelse(is.na(age65_slider$`Total - 15 years and over`),0,age65_slider$`Total - 15 years and over`)
age65_slider$overall_percent_65 <- ifelse(age65_slider$`Total - 15 years and over` >0,age65_slider$`65 years and over`/age65_slider$`Total - 15 years and over`*100,0)
age65_slider$overall_percent_65 <- ifelse(age65_slider$overall_percent_65>100, 100, age65_slider$overall_percent_65)
age65_slider <-age65_slider %>% ungroup() %>% distinct(geography,health_region,industry,naics_sector_name,noc_code,overall_percent_65)
# # View(age65_slider)

sliders <- merge(female_slider,age65_slider,by=c('geography','health_region', 'industry','naics_sector_name', 'noc_code'),all=T)
sliders$overall_percent_female <- ifelse(is.na(sliders$overall_percent_female),0,sliders$overall_percent_female)
sliders$overall_percent_65 <- ifelse(is.na(sliders$overall_percent_65),0,sliders$overall_percent_65)
# View(sliders)

table3_datatool <- merge(table3_r_tool_f,sliders,by=c('geography','health_region', 'industry','naics_sector_name', 'noc_code'),all=T)
table3_datatool$overall_percent_female <- ifelse(is.na(table3_datatool$overall_percent_female),0,table3_datatool$overall_percent_female)
table3_datatool$overall_percent_65 <- ifelse(is.na(table3_datatool$overall_percent_65),0,table3_datatool$overall_percent_65)

###########################################
## MERGE WITH OCCUPATION MEASURES from ONET
############################################
# # View(onet)
table3_datatool <- merge(table3_datatool,onet, by=c("noc_code"),all.x=T)

# View(table3_datatool)
###########################################
## MERGE WITH INCOME
############################################
table3_median_income <- table3_median_income %>% select(-noc_code_class)
# # View(table3_median_income)
table3_median_income$noc_code <- as.numeric(table3_median_income$noc_code)
table3_median_income$noc_code<-formatC(table3_median_income$noc_code, width = 4, format = "d", flag = "0")
table3_median_income$noc_code<-as.character(table3_median_income$noc_code)

table3_datatool  <- merge(table3_datatool,table3_median_income , by=c("health_region","noc_code"),all.x=T)

table3_datatool  <- table3_datatool %>% dplyr::rename("median_income"="median_total1")
# View(table3_datatool)

##########################################
## MERGE WITH NOC_MERGE & SAVE
############################################
table3_sector <- merge(table3_datatool,NOC_MERGE,by="noc_code",all.x=T)
table3_sector <- table3_sector %>% 
  select(geography,health_region,industry,noc_broad,noc_broad_descript,naics_sector_name,noc_code,noc_code_class,sex,age,everything()) %>% 
  arrange(geography,health_region,industry,naics_sector_name,noc_code,sex,age) %>%
  mutate_if(is.numeric, round, 0)


table3_sector <- table3_sector%>%mutate(noc_code_class=substring(noc_code_class,6))
table3_sector<-table3_sector%>%mutate(noc_code_class=gsub("\\s*\\([^\\)]+\\)","",as.character(noc_code_class)))
table3_sector<-table3_sector%>%mutate(noc_code_class= gsub('[0-9]+', '', noc_code_class))

table3_sector <- table3_sector%>%mutate(health_region=substring(health_region,6))

##########################################
## Apply PHO Operational names for PHUs in Ontario
##########################################
table3_sector$health_region_ontario = factor(
  table3_sector$health_region,
  levels = c(
    'The District of Algoma Health Unit',
    'Brant County Health Unit',
    'Durham Regional Health Unit',
    'Grey Bruce Health Unit',
    'Haldimand-Norfolk Health Unit',
    'Haliburton, Kawartha, Pine Ridge District Health Unit',
    'Halton Regional Health Unit',
    'City of Hamilton Health Unit',
    'Hastings and Prince Edward Counties Health Unit',
    'Huron County Health Unit',
    'Chatham-Kent Health Unit',
    'Kingston, Frontenac and Lennox and Addington Health Unit',
    'Lambton Health Unit',
    'Leeds, Grenville and Lanark District Health Unit',
    'Middlesex-London Health Unit',
    'Niagara Regional Area Health Unit',
    'North Bay Parry Sound District Health Unit',
    'Northwestern Health Unit',
    'City of Ottawa Health Unit',
    'Peel Regional Health Unit',
    'Perth District Health Unit',
    'Peterborough County-City Health Unit',
    'Porcupine Health Unit',
    'Renfrew County and District Health Unit',
    'The Eastern Ontario Health Unit',
    'Simcoe Muskoka District Health Unit',
    'Sudbury and District Health Unit',
    'Thunder Bay District Health Unit',
    'Timiskaming Health Unit',
    'Waterloo Health Unit',
    'Wellington-Dufferin-Guelph Health Unit',
    'Windsor-Essex County Health Unit',
    'York Regional Health Unit',
    'Oxford Elgin St. Thomas Health Unit',
    'City of Toronto Health Unit'
    
  ),
  labels = c(
    
    'Algoma Public Health',
    'Brant County Health Unit',
    'Durham Region Health Department',
    'Grey Bruce Health Unit',
    'Haldimand-Norfolk Health Unit',
    'Haliburton, Kawartha, Pine Ridge District Health Unit',
    'Halton Region Public Health',
    'City of Hamilton Public Health Services',
    'Hastings Prince Edward Public Health',
    'Huron Public Health',
    'Chatham-Kent Public Health',
    'Kingston, Frontenac and Lennox & Addington Public Health',
    'Lambton Public Health',
    'Leeds, Grenville & Lanark District Health Unit',
    'Middlesex-London Health Unit',
    'Niagara Region Public Health',
    'North Bay Parry Sound District Health Unit',
    'Northwestern Health Unit',
    'Ottawa Public Health',
    'Peel Public Health',
    'Perth Public Health',
    'Peterborough Public Health',
    'Porcupine Health Unit',
    'Renfrew County and District Health Unit',
    'Eastern Ontario Health Unit',
    'Simcoe Muskoka District Health Unit',
    'Public Health Sudbury & Districts',
    'Thunder Bay District Health Unit',
    'Timiskaming Health Unit',
    'Region of Waterloo Public Health and Emergency Services',
    'Wellington-Dufferin-Guelph Public Health',
    'Windsor-Essex County Health Unit',
    'York Region Public Health',
    'Southwestern Public Health',
    'Toronto Public Health'
    
  )
)
table3_sector$health_region_ontario <- as.character(table3_sector$health_region_ontario)
table3_sector$health_region <- ifelse(table3_sector$geography %in% "Ontario", table3_sector$health_region_ontario,table3_sector$health_region)
table3_sector$health_region <- ifelse(is.na(table3_sector$health_region),"Peterborough Public Health",table3_sector$health_region)


table3_sector <- table3_sector %>% select(-health_region_ontario)

table3_sector <- table3_sector %>% filter(!(is.na(sum_total1)))


table3_datatool <- readRDS("table3_datatool.rds")
table3_datatool$naics_sector_name <- c("Total Sectors")

table3_final <- rbind(table3_sector,table3_datatool)

table3_final$age=factor(table3_final$age,
                        levels=c("15 - 24 years",
                                 "25 - 34 years",
                                 "35 - 44 years",
                                 "45 - 54 years",
                                 "55 - 64 years",
                                 "65 years and over",
                                 "Total - 15 years and over"),
                        labels=c("15 - 24",
                                 "25 - 34",
                                 "35 - 44",
                                 "45 - 54",
                                 "55 - 64",
                                 "65+",
                                 "Total"))
# View(table3_final)
table3_final$sex=factor(table3_final$sex,
                        levels=c("Female",
                                 "Male",
                                 "Total - Sex"),
                        labels=c("Female",
                                 "Male",
                                 "Total"))

table3_final <- table3_final %>% 
  select(geography,health_region,industry,naics_sector_name,noc_code,noc_code_class,everything()) %>%
  arrange(geography,health_region,industry,naics_sector_name,noc_code,noc_code_class) %>%
  filter(sum_total1 > 10)

table3_final_ontario <- table3_final %>% filter(geography=="Ontario")
saveRDS(table3_final, file = "table3_final.rds")


#### create input dataset with regions for sidebar selections in tool
regions_input <- readRDS("table3_final.rds")

regions_input <- regions_input %>% distinct(geography,health_region) %>% filter(!is.na(health_region))



### create the total dataset for tabs that use overall sex and age
table3_final <- readRDS("table3_final.rds")
table3_final$median_income_plot <-
  ifelse(table3_final$median_income > 150000,
         150000,
         table3_final$median_income)
table3_final$noc_broad_descript = factor(
  table3_final$noc_broad_descript,
  levels = c(
    "Management occupations",
    "Business, finance and administration occupations",
    "Natural and applied sciences and related occupations",
    "Health occupations",
    "Occupations in education, law and social, community and government services",
    "Occupations in art, culture, recreation and sport",
    "Sales and service occupations",
    "Trades, transport and equipment operators and related occupations",
    "Natural resources, agriculture and related production occupations",
    "Occupations in manufacturing and utilities"
  ),
  labels = c(
    "Management",
    "Business",
    "Sciences",
    "Health",
    "Community",
    "Culture",
    "Sales",
    "Trades",
    "Agriculture",
    "Utilities"
  )
)

table3_final_total <- table3_final %>% filter(sex == 'Total' & age == 'Total')

## Save datatool datasets
saveRDS(regions_input, file = "regions_input.rds")
saveRDS(table3_final_total, file = "table3_final_total.rds")
saveRDS(table3_final, file = "table3_final.rds")


