####################
## Occupational Risk Covid19 Code Repository

#######################
## Step4: Create Datatool Datasets for provinces (Table1 and Table2 Census)
#######################

library(dplyr)
library(scales)
library(tidyverse)

#####################################
#####################################
rm(list=ls())
load("onet_naics_noc.RData")
load("table1_r.RData")

##########################################
## Sum NOC codes within services
##########################################
table1_r_tool <- table1_r %>% 
  group_by(geography, essential, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum) %>%
  ungroup()

######################################################################
## create designations for industry variable for each service strategy
######################################################################
table1_r_tool$industry <- ifelse(table1_r_tool$essential==1, "Essential", "Non-essential") 
table1_r_tool <- table1_r_tool %>% 
  ungroup() %>% 
  select(-essential)

###################################################
## now create an "all occupations" dataset 
## which includes both essential and other services
###################################################
table1_r_tool_all <- table1_r_tool %>% 
  ungroup() %>% 
  group_by(geography, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)
table1_r_tool_all$industry <- c("Total")

############################
## append the datasets 
############################
table1_r_tool_f <- rbind(table1_r_tool, table1_r_tool_all)

#############################
## socio-dem characteristics
#############################
table1_r_tool_f$sum_white1 <- table1_r_tool_f$sum_notvismin1

table1_r_tool_f$percent_immig <- ifelse(table1_r_tool_f$sum_total1>0, table1_r_tool_f$sum_immig1 / table1_r_tool_f$sum_total1*100, 0)
table1_r_tool_f$percent_immig <- ifelse(table1_r_tool_f$percent_immig>100, 100, table1_r_tool_f$percent_immig)

table1_r_tool_f$percent_nonpermres <- ifelse(table1_r_tool_f$sum_total1>0, table1_r_tool_f$sum_nonpermres1 / table1_r_tool_f$sum_total1*100, 0)
table1_r_tool_f$percent_nonpermres <- ifelse(table1_r_tool_f$percent_nonpermres>100, 100, table1_r_tool_f$percent_nonpermres)

table1_r_tool_f$percent_vismin <- ifelse(table1_r_tool_f$sum_total1>0, table1_r_tool_f$sum_vismin1 / table1_r_tool_f$sum_total1*100, 0)
table1_r_tool_f$percent_vismin <- ifelse(table1_r_tool_f$percent_vismin>100, 100, table1_r_tool_f$percent_vismin)

################################################################################
## create overall female and over 65 percents (for SLIDER in population group)
################################################################################
female_slider <- table1_r_tool_f %>% 
  filter(age == "Total - 15 years and over") %>% 
  select(geography,industry,noc_code,sex,age,sum_total1) %>%
  spread(sex,sum_total1) 
female_slider$Female <- ifelse(is.na(female_slider$Female),0,female_slider$Female)
female_slider$`Total - Sex` <- ifelse(is.na(female_slider$`Total - Sex`),0,female_slider$`Total - Sex`)
female_slider$overall_percent_female <- ifelse(female_slider$`Total - Sex` >0,female_slider$Female/female_slider$`Total - Sex`*100,0)
female_slider$overall_percent_female <- ifelse(female_slider$overall_percent_female>100, 100, female_slider$overall_percent_female)
female_slider <- female_slider %>% ungroup() %>% distinct(geography,industry,noc_code,overall_percent_female)
# View(female_slider)                                                    

age65_slider <- table1_r_tool_f %>% 
  filter(sex == "Total - Sex") %>% 
  select(geography,industry,noc_code,sex,age,sum_total1) %>%
  spread(age,sum_total1) 
age65_slider$`65 years and over` <- ifelse(is.na(age65_slider$`65 years and over`),0,age65_slider$`65 years and over`)
age65_slider$`Total - 15 years and over` <- ifelse(is.na(age65_slider$`Total - 15 years and over`),0,age65_slider$`Total - 15 years and over`)
age65_slider$overall_percent_65 <- ifelse(age65_slider$`Total - 15 years and over` >0,age65_slider$`65 years and over`/age65_slider$`Total - 15 years and over`*100,0)
age65_slider$overall_percent_65 <- ifelse(age65_slider$overall_percent_65>100, 100, age65_slider$overall_percent_65)
age65_slider <-age65_slider %>% ungroup() %>% distinct(geography,industry,noc_code,overall_percent_65)
# View(age65_slider)

sliders <- merge(female_slider,age65_slider,by=c('geography','industry','noc_code'),all=T)
sliders$overall_percent_female <- ifelse(is.na(sliders$overall_percent_female),0,sliders$overall_percent_female)
sliders$overall_percent_65 <- ifelse(is.na(sliders$overall_percent_65),0,sliders$overall_percent_65)
# View(sliders)

table1_datatool <- merge(table1_r_tool_f,sliders,by=c('geography','industry','noc_code'),all=T)
table1_datatool$overall_percent_female <- ifelse(is.na(table1_datatool$overall_percent_female),0,table1_datatool$overall_percent_female)
table1_datatool$overall_percent_65 <- ifelse(is.na(table1_datatool$overall_percent_65),0,table1_datatool$overall_percent_65)
# View(table1_datatool)
###########################################
## MERGE WITH OCCUPATION MEASURES from ONET
############################################
table1_datatool <- merge(table1_datatool,onet, by=c("noc_code"),all.x=T) 
# View(table1_datatool)

###############################################################
## MERGE WITH INCOME & SAVE
###############################################################
load("table1_median_income.RData")

table1_median_income_filter <- table1_median_income %>% 
   select(-noc_code_class)

table1_datatool <- merge(table1_datatool,table1_median_income_filter, by=c("geography","noc_code"),all.x=T)
table1_datatool$median_income_plot <- ifelse(table1_datatool$median_income > 150000, 150000,table1_datatool$median_income) 

table1_datatool <- table1_datatool %>% 
  select(geography, industry,noc_code, sex, age, everything()) %>% 
  arrange(geography, industry,noc_code, sex, age) %>% 
  mutate_if(is.numeric, round, 0)




###### income quintile (Table 2)
##########################################
## Sum NOC codes within services
##########################################
load("table2_r.RData")
table2_r_tool <- table2_r %>% 
  group_by(geography, essential, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum) %>%ungroup()


######################################################################
## create designations for industry variable for each service strategy
######################################################################
table2_r_tool$industry <- ifelse(table2_r_tool$essential==1, "Essential", "Non-essential")
table2_r_tool <- table2_r_tool %>% 
  ungroup() %>% 
  select(-essential)

###################################################
## now create an "all occupations" dataset 
## which includes both essential and other services
###################################################
table2_r_tool_all <- table2_r_tool %>% 
  ungroup() %>% 
  group_by(geography, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)
table2_r_tool_all$industry <- c("Total")

############################
## append the datasets 
############################
table2_r_tool_f <- rbind(table2_r_tool, table2_r_tool_all)

###############################################################
## socio-demographic characteristics
###############################################################
table2_r_tool_f$percent_quintile1 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile1 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile1 <- ifelse(table2_r_tool_f$percent_quintile1>100, 100, table2_r_tool_f$percent_quintile1)

table2_r_tool_f$percent_quintile2 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile2 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile2 <- ifelse(table2_r_tool_f$percent_quintile2>100, 100, table2_r_tool_f$percent_quintile2)

table2_r_tool_f$percent_quintile3 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile3 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile3 <- ifelse(table2_r_tool_f$percent_quintile3>100, 100, table2_r_tool_f$percent_quintile3)

table2_r_tool_f$percent_quintile4 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile4 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile4 <- ifelse(table2_r_tool_f$percent_quintile4>100, 100, table2_r_tool_f$percent_quintile4)

table2_r_tool_f$percent_quintile5 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile5 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile5 <- ifelse(table2_r_tool_f$percent_quintile5>100, 100, table2_r_tool_f$percent_quintile5)

#########################################
#merge with income and finalize dataset
#########################################
table2_datatool<- table2_r_tool_f %>% 
  mutate_if(is.numeric, round, 0) %>% 
  distinct() %>% 
  select(-median_total1)

table2_datatool <- table2_datatool %>% 
  select(geography, industry,noc_code, sex, age, everything()) %>% 
  arrange(geography, industry, noc_code, sex, age)
# View(table2_datatool)

####### merge files together (Census table 1 and table2)


table2_datatool1 <-table2_datatool %>% 
  select(geography,industry,noc_code,sex,age,sum_quintile1,sum_quintile2,sum_quintile3,sum_quintile4,sum_quintile5,
         percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5)

table2_datatool1$age=factor(table2_datatool1$age,
                            levels=c("15 to 24 years",
                                     "25 to 34 years",
                                     "35 to 44 years",
                                     "45 to 54 years",
                                     "55 to 64 years",
                                     "65 years and over",
                                     "Total -  Population 15 years and over"
                            ),
                            labels=c("15 - 24 years",
                                     "25 - 34 years",
                                     "35 - 44 years",
                                     "45 - 54 years",
                                     "55 - 64 years",
                                     "65 years and over",
                                     "Total - 15 years and over"
                            ))
# View(table2_datatool1)

test<-merge(table1_datatool,table2_datatool1,by=c('geography','industry','noc_code','sex','age'),all.x=T)
test <- test %>% 
  select(geography, industry,noc_code, sex, age, everything()) %>% 
  arrange(geography, industry, noc_code, sex, age) %>% 
  mutate_if(is.numeric, round, 0)

test$sum_quintile1 <- ifelse(is.na(test$sum_quintile1),0,test$sum_quintile1)
test$sum_quintile2 <- ifelse(is.na(test$sum_quintile2),0,test$sum_quintile2)
test$sum_quintile3 <- ifelse(is.na(test$sum_quintile3),0,test$sum_quintile3)
test$sum_quintile4 <- ifelse(is.na(test$sum_quintile4),0,test$sum_quintile4)
test$sum_quintile5 <- ifelse(is.na(test$sum_quintile5),0,test$sum_quintile5)

test$percent_quintile1 <- ifelse(is.na(test$percent_quintile1),0,test$percent_quintile1)
test$percent_quintile2 <- ifelse(is.na(test$percent_quintile2),0,test$percent_quintile2)
test$percent_quintile3 <- ifelse(is.na(test$percent_quintile3),0,test$percent_quintile3)
test$percent_quintile4 <- ifelse(is.na(test$percent_quintile4),0,test$percent_quintile4)
test$percent_quintile5 <- ifelse(is.na(test$percent_quintile5),0,test$percent_quintile5)

table1_table2_datatool <- test

table1_table2_datatool <- merge(table1_table2_datatool,NOC_MERGE,by="noc_code",all.x=T)

table1_table2_datatool <- table1_table2_datatool %>% 
  select(geography, industry,noc_broad, noc_broad_descript,noc_code,noc_code_class, sex, age, everything()) 


table1_table2_datatool$age=factor(table1_table2_datatool$age,
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

table1_table2_datatool$sex=factor(table1_table2_datatool$sex,
                                  levels=c("Female",
                                           "Male",
                                           "Total - Sex"),
                                  labels=c("Female",
                                           "Male",
                                           "Total"))



table1_table2_datatool <- table1_table2_datatool%>%mutate(noc_code_class=substring(noc_code_class,6))
table1_table2_datatool<-table1_table2_datatool%>%mutate(noc_code_class=gsub("\\s*\\([^\\)]+\\)","",as.character(noc_code_class)))
table1_table2_datatool<-table1_table2_datatool%>%mutate(noc_code_class= gsub('[0-9]+', '', noc_code_class))

# View(table1_table2_datatool)

saveRDS(table1_table2_datatool, file = "table1_table2_datatool.rds")


library(dplyr)
library(scales)
library(tidyverse)

#####################################
## Specific Sectors (NAICS)
#####################################
rm(list=ls())
load("onet_naics_noc.RData")
load("table1_r.RData")

# View(table1_r)
##########################################
## Sum NOC codes within services
##########################################
table1_r_tool <- table1_r %>% 
  group_by(geography, essential, naics_sector_name, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum) %>%
  ungroup()

######################################################################
## create designations for industry variable for each service strategy
######################################################################
table1_r_tool$industry <- ifelse(table1_r_tool$essential==1, "Essential", "Non-essential") 
table1_r_tool <- table1_r_tool %>% 
  ungroup() %>% 
  select(-essential)

###################################################
## now create an "all occupations" dataset 
## which includes both essential and other services
###################################################
table1_r_tool_all <- table1_r_tool %>% 
  ungroup() %>% 
  group_by(geography,naics_sector_name, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)
table1_r_tool_all$industry <- c("Total")

############################
## append the datasets 
############################
table1_r_tool_f <- rbind(table1_r_tool, table1_r_tool_all)

#############################
## socio-dem characteristics
#############################
table1_r_tool_f$sum_white1 <- table1_r_tool_f$sum_notvismin1

table1_r_tool_f$percent_immig <- ifelse(table1_r_tool_f$sum_total1>0, table1_r_tool_f$sum_immig1 / table1_r_tool_f$sum_total1*100, 0)
table1_r_tool_f$percent_immig <- ifelse(table1_r_tool_f$percent_immig>100, 100, table1_r_tool_f$percent_immig)

table1_r_tool_f$percent_nonpermres <- ifelse(table1_r_tool_f$sum_total1>0, table1_r_tool_f$sum_nonpermres1 / table1_r_tool_f$sum_total1*100, 0)
table1_r_tool_f$percent_nonpermres <- ifelse(table1_r_tool_f$percent_nonpermres>100, 100, table1_r_tool_f$percent_nonpermres)

table1_r_tool_f$percent_vismin <- ifelse(table1_r_tool_f$sum_total1>0, table1_r_tool_f$sum_vismin1 / table1_r_tool_f$sum_total1*100, 0)
table1_r_tool_f$percent_vismin <- ifelse(table1_r_tool_f$percent_vismin>100, 100, table1_r_tool_f$percent_vismin)

################################################################################
## create overall female and over 65 percents (for SLIDER in population group)
################################################################################
female_slider <- table1_r_tool_f %>% 
  filter(age == "Total - 15 years and over") %>% 
  select(geography,industry,naics_sector_name,noc_code,sex,age,sum_total1) %>%
  spread(sex,sum_total1) 
female_slider$Female <- ifelse(is.na(female_slider$Female),0,female_slider$Female)
female_slider$`Total - Sex` <- ifelse(is.na(female_slider$`Total - Sex`),0,female_slider$`Total - Sex`)
female_slider$overall_percent_female <- ifelse(female_slider$`Total - Sex` >0,female_slider$Female/female_slider$`Total - Sex`*100,0)
female_slider$overall_percent_female <- ifelse(female_slider$overall_percent_female>100, 100, female_slider$overall_percent_female)
female_slider <- female_slider %>% ungroup() %>% distinct(geography,industry,naics_sector_name,noc_code,overall_percent_female)
# View(female_slider)                                                    

age65_slider <- table1_r_tool_f %>% 
  filter(sex == "Total - Sex") %>% 
  select(geography,industry,naics_sector_name,noc_code,sex,age,sum_total1) %>%
  spread(age,sum_total1) 
age65_slider$`65 years and over` <- ifelse(is.na(age65_slider$`65 years and over`),0,age65_slider$`65 years and over`)
age65_slider$`Total - 15 years and over` <- ifelse(is.na(age65_slider$`Total - 15 years and over`),0,age65_slider$`Total - 15 years and over`)
age65_slider$overall_percent_65 <- ifelse(age65_slider$`Total - 15 years and over` >0,age65_slider$`65 years and over`/age65_slider$`Total - 15 years and over`*100,0)
age65_slider$overall_percent_65 <- ifelse(age65_slider$overall_percent_65>100, 100, age65_slider$overall_percent_65)
age65_slider <-age65_slider %>% ungroup() %>% distinct(geography,industry,naics_sector_name,noc_code,overall_percent_65)
# View(age65_slider)

sliders <- merge(female_slider,age65_slider,by=c('geography','industry','naics_sector_name','noc_code'),all=T)
sliders$overall_percent_female <- ifelse(is.na(sliders$overall_percent_female),0,sliders$overall_percent_female)
sliders$overall_percent_65 <- ifelse(is.na(sliders$overall_percent_65),0,sliders$overall_percent_65)
# View(sliders)

table1_datatool <- merge(table1_r_tool_f,sliders,by=c('geography','industry','naics_sector_name','noc_code'),all=T)
table1_datatool$overall_percent_female <- ifelse(is.na(table1_datatool$overall_percent_female),0,table1_datatool$overall_percent_female)
table1_datatool$overall_percent_65 <- ifelse(is.na(table1_datatool$overall_percent_65),0,table1_datatool$overall_percent_65)
# View(table1_datatool)
###########################################
## MERGE WITH OCCUPATION MEASURES from ONET
############################################
table1_datatool <- merge(table1_datatool,onet, by=c("noc_code"),all.x=T) 
# View(table1_datatool)

###############################################################
## MERGE WITH INCOME & SAVE
###############################################################
load("table1_median_income.RData")


table1_median_income_filter <- table1_median_income %>% 
  #filter(noc_code_class %in% NOC_MERGE$noc_code_class) %>% 
  select(-noc_code_class)

table1_datatool <- merge(table1_datatool,table1_median_income_filter, by=c("geography","noc_code"),all.x=T)
table1_datatool$median_income_plot <- ifelse(table1_datatool$median_income > 150000, 150000,table1_datatool$median_income) 

table1_datatool <- table1_datatool %>% 
  select(geography, industry,naics_sector_name,noc_code, sex, age, everything()) %>% 
  arrange(geography, industry,naics_sector_name,noc_code, sex, age) %>% 
  mutate_if(is.numeric, round, 0)


###### income quintile (Table 2)
load("table2_r.RData")

##########################################
## Sum NOC codes within services
##########################################
table2_r_tool <- table2_r %>% 
  group_by(geography, essential, 
           naics_sector_name, noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum) %>%ungroup()


######################################################################
## create designations for industry variable for each service strategy
######################################################################
table2_r_tool$industry <- ifelse(table2_r_tool$essential==1, "Essential", "Non-essential")
table2_r_tool <- table2_r_tool %>% 
  ungroup() %>% 
  select(-essential)

###################################################
## now create an "all occupations" dataset 
## which includes both essential and other services
###################################################
table2_r_tool_all <- table2_r_tool %>% 
  ungroup() %>% 
  group_by(geography, 
           naics_sector_name,noc_code, sex, age) %>% 
  summarise_if(is.numeric, sum)
table2_r_tool_all$industry <- c("Total")

############################
## append the datasets 
############################
table2_r_tool_f <- rbind(table2_r_tool, table2_r_tool_all)

###############################################################
## socio-demographic characteristics
###############################################################
table2_r_tool_f$percent_quintile1 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile1 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile1 <- ifelse(table2_r_tool_f$percent_quintile1>100, 100, table2_r_tool_f$percent_quintile1)

table2_r_tool_f$percent_quintile2 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile2 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile2 <- ifelse(table2_r_tool_f$percent_quintile2>100, 100, table2_r_tool_f$percent_quintile2)

table2_r_tool_f$percent_quintile3 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile3 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile3 <- ifelse(table2_r_tool_f$percent_quintile3>100, 100, table2_r_tool_f$percent_quintile3)

table2_r_tool_f$percent_quintile4 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile4 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile4 <- ifelse(table2_r_tool_f$percent_quintile4>100, 100, table2_r_tool_f$percent_quintile4)

table2_r_tool_f$percent_quintile5 <- ifelse(table2_r_tool_f$sum_total1>0, table2_r_tool_f$sum_quintile5 / table2_r_tool_f$sum_total1*100, 0)
table2_r_tool_f$percent_quintile5 <- ifelse(table2_r_tool_f$percent_quintile5>100, 100, table2_r_tool_f$percent_quintile5)

#########################################
#merge with income and finalize dataset
#########################################
table2_datatool<- table2_r_tool_f %>% 
  mutate_if(is.numeric, round, 0) %>% 
  distinct() %>% 
  select(-median_total1)

table2_datatool <- table2_datatool %>% 
  select(geography, industry,
         naics_sector_name,noc_code, sex, age, everything()) %>% 
  arrange(geography, industry,
          naics_sector_name, noc_code, sex, age)
# View(table2_datatool)



####### merge files together (Census table 1 and table2)
table2_datatool1 <-table2_datatool %>% 
  select(geography,industry,
         naics_sector_name,noc_code,sex,age,sum_quintile1,sum_quintile2,sum_quintile3,sum_quintile4,sum_quintile5,
         percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5)

table2_datatool1$age=factor(table2_datatool1$age,
                            levels=c("15 to 24 years",
                                     "25 to 34 years",
                                     "35 to 44 years",
                                     "45 to 54 years",
                                     "55 to 64 years",
                                     "65 years and over",
                                     "Total -  Population 15 years and over"
                            ),
                            labels=c("15 - 24 years",
                                     "25 - 34 years",
                                     "35 - 44 years",
                                     "45 - 54 years",
                                     "55 - 64 years",
                                     "65 years and over",
                                     "Total - 15 years and over"
                            ))
# View(table2_datatool1)

test<-merge(table1_datatool,table2_datatool1,by=c('geography','industry',
                                                  'naics_sector_name','noc_code','sex','age'),all.x=T)
test <- test %>% 
  select(geography, industry,
         naics_sector_name,noc_code, sex, age, everything()) %>% 
  arrange(geography, industry, 
          naics_sector_name,noc_code, sex, age) %>% 
  mutate_if(is.numeric, round, 0)

test$sum_quintile1 <- ifelse(is.na(test$sum_quintile1),0,test$sum_quintile1)
test$sum_quintile2 <- ifelse(is.na(test$sum_quintile2),0,test$sum_quintile2)
test$sum_quintile3 <- ifelse(is.na(test$sum_quintile3),0,test$sum_quintile3)
test$sum_quintile4 <- ifelse(is.na(test$sum_quintile4),0,test$sum_quintile4)
test$sum_quintile5 <- ifelse(is.na(test$sum_quintile5),0,test$sum_quintile5)

test$percent_quintile1 <- ifelse(is.na(test$percent_quintile1),0,test$percent_quintile1)
test$percent_quintile2 <- ifelse(is.na(test$percent_quintile2),0,test$percent_quintile2)
test$percent_quintile3 <- ifelse(is.na(test$percent_quintile3),0,test$percent_quintile3)
test$percent_quintile4 <- ifelse(is.na(test$percent_quintile4),0,test$percent_quintile4)
test$percent_quintile5 <- ifelse(is.na(test$percent_quintile5),0,test$percent_quintile5)

table1_table2_sector <- test

table1_table2_sector <- merge(table1_table2_sector,NOC_MERGE,by="noc_code",all.x=T)

table1_table2_sector <- table1_table2_sector %>% 
  select(geography, industry,noc_broad, noc_broad_descript,
         naics_sector_name,noc_code,noc_code_class, sex, age, everything()) 


table1_table2_sector$age=factor(table1_table2_sector$age,
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

table1_table2_sector$sex=factor(table1_table2_sector$sex,
                                levels=c("Female",
                                         "Male",
                                         "Total - Sex"),
                                labels=c("Female",
                                         "Male",
                                         "Total"))


table1_table2_sector <- table1_table2_sector%>%mutate(noc_code_class=substring(noc_code_class,6))
table1_table2_sector<-table1_table2_sector%>%mutate(noc_code_class=gsub("\\s*\\([^\\)]+\\)","",as.character(noc_code_class)))
table1_table2_sector<-table1_table2_sector%>%mutate(noc_code_class= gsub('[0-9]+', '', noc_code_class))


table1_table2_datatool <- readRDS("table1_table2_datatool.rds")
table1_table2_datatool$naics_sector_name <- c("Total Sectors")

table1_table2_final <- rbind(table1_table2_sector,table1_table2_datatool)
table1_table2_final <- table1_table2_final %>% 
  select(geography,industry,naics_sector_name,noc_code,noc_code_class,everything()) %>%
  arrange(geography,industry,naics_sector_name,noc_code,noc_code_class) %>%
  filter(sum_total1 > 10)


saveRDS(table1_table2_final, file = "table1_table2_final.rds")


### create the total dataset for tabs that use overall sex and age
table1_table2_final <- readRDS("table1_table2_final.rds")
table1_table2_final$median_income_plot <-
  ifelse(table1_table2_final$median_income > 150000,
         150000,
         table1_table2_final$median_income)
table1_table2_final$noc_broad_descript = factor(
  table1_table2_final$noc_broad_descript,
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

table1_table2_final_total <- table1_table2_final %>% filter(sex == 'Total' & age == 'Total')

##Datatool datasets:
saveRDS(table1_table2_final_total, file = "table1_table2_final_total.rds")
saveRDS(table1_table2_final, file = "table1_table2_final.rds")
