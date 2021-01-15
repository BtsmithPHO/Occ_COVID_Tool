######################
## Occupational Risk Covid19 Code Repository
######################

###############
## STEP 1: ONET & BROOKFIELD CROSSWALK

library(dplyr)
library(scales)
library(tidyverse)
library(readxl)
rm(list=ls())


crosswalk <-read_excel("Brookfield_crosswalk.xlsx")
crosswalk <- crosswalk %>% rename('code'='onet',
                                  'noc_code'='noc')

# # View(crosswalk)
## 1) Import ONET indicators of interest
## Telework

load("//oto101pfile01v/Christine.Warren$/Research Projects/Covid19_Occupation_Project/onet_measures_all/onet_telework_dingel.RData")
onet_telework_dingel <- onet_telework_dingel %>% rename(code=onetsoccode,onet_title=title)

work_from_home <- merge(onet_telework_dingel,crosswalk, by="onet_title", all=TRUE, sort=TRUE)

# telework_na <- work_from_home %>% filter(is.na(teleworkable))

work_from_home <- work_from_home %>% filter(!is.na(noc_code))
work_from_home <- work_from_home[!names(work_from_home) %in% c("Occupation")] %>% filter(!(noc_code %in% "NA")) %>% filter(!(is.na(teleworkable)))

duplicated <- work_from_home %>% filter(duplicated(noc_code))
# write.csv(duplicated,"duplicated_wfh.csv")

work_from_home <- work_from_home %>% distinct(noc_code,.keep_all=T)

## using duplicated_wfh_final, attribute changed wfh status to codes
# duplicated_wfh <- read.csv("duplicated_wfh_final.csv")

# work_from_home$teleworkable<- ifelse(work_from_home$noc_code %in% duplicated_wfh$noc_code,duplicated_wfh$wfh_final,NA)
work_from_home$teleworkable <- ifelse(work_from_home$noc_code %in% c(1525,
                                                                     2121,
                                                                     2132,
                                                                     2224,
                                                                     3111,
                                                                     3144,
                                                                     3237,
                                                                     3414,
                                                                     4212,
                                                                     4423,
                                                                     5131,
                                                                     6533,
                                                                     6541,
                                                                     6562,
                                                                     6742,
                                                                     7441,
                                                                     1513,
                                                                     2272,
                                                                     8252,
                                                                     8255,
                                                                     2154,
                                                                     5232,
                                                                     5132,
                                                                     9471,
                                                                     9472,
                                                                     6532), 
                                      0,ifelse(work_from_home$noc_code %in% c(15,
                                                                              731,
                                                                              2148,
                                                                              4011,
                                                                              4021,
                                                                              4215,
                                                                              5132,
                                                                              6623
                                      ),1,work_from_home$teleworkable))



work_from_home$noc_code <- as.numeric(work_from_home$noc_code)
work_from_home$noc_code<-formatC(work_from_home$noc_code, width = 4, format = "d", flag = "0")
work_from_home$noc_code<-as.character(work_from_home$noc_code)

work_from_home <- work_from_home %>% rename(wfh_yes=teleworkable,
                                            noc_code_class=noc_title) 

# View(work_from_home)
work_from_home_check <- work_from_home %>% select(noc_code,noc_code_class,wfh_yes) %>% arrange(noc_code)
work_from_home_check$wfh_yes <- ifelse(work_from_home_check$wfh_yes == 1, "Yes","No")
# View(work_from_home_check)

write.csv(work_from_home_check,"//oto101pfile01v/Christine.Warren$/Research Projects/Covid19_Occupation_Project/Census Data/work_from_home_check.csv")
work_from_home <- work_from_home %>% select(noc_code,wfh_yes)

library(readxl)
work_context <- read_excel("//oto101pfile01v/Christine.Warren$/Research Projects/Covid19_Occupation_Project/onet_measures_all/Work Context.xlsx")
work_context2 <-work_context %>% filter(`Scale ID` %in% "CX" & 
                                          `Element Name` %in% 
                                          c("Contact With Others","Indoors, Environmentally Controlled",
                                            
                                            "Indoors, Not Environmentally Controlled","Physical Proximity",
                                            
                                            "Exposed to Disease or Infections",
                                            "Wear Common Protective or Safety Equipment such as Safety Shoes, Glasses, Gloves, Hearing Protection, Hard Hats, or Life Jackets",
                                            
                                            "Wear Specialized Protective or Safety Equipment such as Breathing Apparatus, Safety Harness, Full Protection Suits, or Radiation Protection"))

work_context2$onet_score <- (work_context2$`Data Value`-1)*25 
work_context2 <- work_context2 %>% 
  mutate_if(is.numeric,round,0) %>%
  select(`O*NET-SOC Code`,`Element Name`,"onet_score") %>%
  rename('code'=`O*NET-SOC Code`) %>%
  rename('measure'=`Element Name`)




## Contact w Others
Contact_with_Others <- work_context2 %>% 
  filter(measure== "Contact With Others") %>% 
  rename('Contact_with_Others'='onet_score')

# # # # # # # View(Contact_with_Others)

contact_others <- merge(Contact_with_Others,crosswalk, by="code", all=TRUE, sort=TRUE)

contact_others <- contact_others %>% filter(!is.na(noc_code))
contact_others$Contact_with_Others <- as.numeric(contact_others$Contact_with_Others)

contact_others <- contact_others[!names(contact_others) %in% c("Occupation")] %>% filter(!(noc_code %in% "NA"))
# # # # # # # View(contact_others)
### now take average by noc_code for each work factor
summary(contact_others)
contact_others <- contact_others %>% group_by(noc_code) %>% mutate_if(is.numeric,mean,na.rm=T) %>% mutate_if(is.numeric,round,0) %>% distinct(noc_code,.keep_all=T) 
## # # # # # # View(contact_others)

contact_others$noc_code <- as.numeric(contact_others$noc_code)
contact_others$noc_code<-formatC(contact_others$noc_code, width = 4, format = "d", flag = "0")
contact_others$noc_code<-as.character(contact_others$noc_code)
# # # # # # # View(contact_others)
contact_others <- contact_others %>% select(noc_code,Contact_with_Others)
summary(contact_others)
names(contact_others)


## Phys Prox
Physical_Proximity <- work_context2 %>% 
  filter(measure== "Physical Proximity") %>% 
  rename('Physical_Proximity'='onet_score')

phys_prox <- merge(Physical_Proximity,crosswalk, by="code", all=TRUE, sort=TRUE)

phys_prox <- phys_prox %>% filter(!is.na(noc_code))
phys_prox <- phys_prox[!names(phys_prox) %in% c("Occupation")] %>% filter(!(noc_code %in% "NA"))
# # # # # # # View(phys_prox)
### now take average by noc_code for each work factor
summary(phys_prox)
phys_prox$Physical_Proximity <- as.numeric(phys_prox$Physical_Proximity)

phys_prox <- phys_prox %>% group_by(noc_code) %>% mutate_if(is.numeric,mean,na.rm=T) %>% mutate_if(is.numeric,round,0) %>% distinct(noc_code,.keep_all=T) 


## # # # # # # View(phys_prox)

phys_prox$noc_code <- as.numeric(phys_prox$noc_code)
phys_prox$noc_code<-formatC(phys_prox$noc_code, width = 4, format = "d", flag = "0")
phys_prox$noc_code<-as.character(phys_prox$noc_code)
phys_prox <- phys_prox %>% select(noc_code,Physical_Proximity)
# # # # # # # View(phys_prox)
summary(phys_prox)
names(phys_prox)


## Exposure
Exposed_to_Disease_or_Infections <- work_context2 %>% 
  filter(measure== "Exposed to Disease or Infections") %>% 
  rename('Exposed_to_Disease_or_Infections'='onet_score')

exposed_disease <- merge(Exposed_to_Disease_or_Infections,crosswalk,by="code", all=TRUE, sort=TRUE)

exposed_disease <- exposed_disease %>% filter(!is.na(noc_code))
exposed_disease <- exposed_disease[!names(exposed_disease) %in% c("Occupation")] %>% filter(!(noc_code %in% "NA"))

exposed_disease$Exposed_to_Disease_or_Infections <- as.numeric(exposed_disease$Exposed_to_Disease_or_Infections)

exposed_disease <- exposed_disease %>% group_by(noc_code) %>% mutate_if(is.numeric,mean,na.rm=T) %>% mutate_if(is.numeric,round,0) %>% distinct(noc_code,.keep_all=T)

## # # # # # # View(exposed_disease)

exposed_disease$noc_code <- as.numeric(exposed_disease$noc_code)
exposed_disease$noc_code<-formatC(exposed_disease$noc_code, width = 4, format = "d", flag = "0")
exposed_disease$noc_code<-as.character(exposed_disease$noc_code)
# # # # # # # View(exposed_disease)
exposed_disease <- exposed_disease %>% select(noc_code,Exposed_to_Disease_or_Infections)
summary(exposed_disease)
names(exposed_disease)
exposed_disease$Exposed_to_Disease_or_Infections <- as.numeric(exposed_disease$Exposed_to_Disease_or_Infections)
# # # # # # # View(Exposed_to_Disease_or_Infections)

## Controlled indoor environment Indoors_Environmentally_Controlled
Indoors_Environmentally_Controlled <- work_context2 %>% 
  filter(measure== "Indoors, Environmentally Controlled") %>% 
  rename('Indoors_Environmentally_Controlled'='onet_score')

indoors_controlled <- merge(Indoors_Environmentally_Controlled,crosswalk,by="code", all=TRUE, sort=TRUE)


indoors_controlled <- indoors_controlled %>% filter(!is.na(noc_code))
indoors_controlled <- indoors_controlled[!names(indoors_controlled) %in% c("Occupation")] %>% filter(!(noc_code %in% "NA"))
indoors_controlled$Indoors_Environmentally_Controlled <- as.numeric(indoors_controlled$Indoors_Environmentally_Controlled)
indoors_controlled <- indoors_controlled %>% 
  group_by(noc_code) %>% 
  mutate_if(is.numeric,mean,na.rm=T) %>% 
  mutate_if(is.numeric,round,0) %>% 
  distinct(noc_code,.keep_all=T) 
## # # # # # # View(indoors_controlled)

indoors_controlled$noc_code <- as.numeric(indoors_controlled$noc_code)
indoors_controlled$noc_code<-formatC(indoors_controlled$noc_code, width = 4, format = "d", flag = "0")
indoors_controlled$noc_code<-as.character(indoors_controlled$noc_code)
# # # # # # # View(indoors_controlled)
indoors_controlled <- indoors_controlled %>% select(noc_code,Indoors_Environmentally_Controlled)
summary(indoors_controlled)
names(indoors_controlled)



## not controlled indoor environment Indoors_Not_Environmentally_Controlled
Indoors_Not_Environmentally_Controlled <- work_context2 %>% 
  filter(measure== "Indoors, Not Environmentally Controlled") %>% 
  rename('Indoors_Not_Environmentally_Controlled'='onet_score')

indoors_not_controlled <- merge(Indoors_Not_Environmentally_Controlled,crosswalk,by="code", all=TRUE, sort=TRUE)

indoors_not_controlled <- indoors_not_controlled %>% filter(!is.na(noc_code))
indoors_not_controlled <- indoors_not_controlled[!names(indoors_not_controlled) %in% c("Occupation")] %>% filter(!(noc_code %in% "NA"))
indoors_not_controlled$Indoors_Not_Environmentally_Controlled <- as.numeric(indoors_not_controlled$Indoors_Not_Environmentally_Controlled)
indoors_not_controlled <- indoors_not_controlled %>% group_by(noc_code) %>% mutate_if(is.numeric,mean,na.rm=T) %>% mutate_if(is.numeric,round,0) %>% distinct(noc_code,.keep_all=T) 

indoors_not_controlled$noc_code <- as.numeric(indoors_not_controlled$noc_code)
indoors_not_controlled$noc_code<-formatC(indoors_not_controlled$noc_code, width = 4, format = "d", flag = "0")
indoors_not_controlled$noc_code<-as.character(indoors_not_controlled$noc_code)
# # # # # # # View(indoors_not_controlled)
indoors_not_controlled <- indoors_not_controlled %>% select(noc_code,Indoors_Not_Environmentally_Controlled)
summary(indoors_not_controlled)
names(indoors_not_controlled)



## interacting w/ public
library(readxl)
Performing_for_or_Working_Directly_with_the_Public <-read_excel("//oto101pfile01v/Christine.Warren$/Research Projects/Covid19_Occupation_Project/onet_measures_all/Performing_for_or_Working_Directly_with_the_Public.xls")
# # # # # # # View(Performing_for_or_Working_Directly_with_the_Public)
Performing_for_or_Working_Directly_with_the_Public <- Performing_for_or_Working_Directly_with_the_Public %>% rename('Performing_for_or_Working_Directly_with_the_Public'='Browse by O*NET Data',
                                                                                                                    'code'='...3',
                                                                                                                    'onet_title'='...4') %>%
  filter(!(Performing_for_or_Working_Directly_with_the_Public %in% c('Work Activities - Performing for or Working Directly with the Public','Importance'))) %>%
  select(-`...2`)
# # # # # # # View(Performing_for_or_Working_Directly_with_the_Public)

public_importance <- merge(Performing_for_or_Working_Directly_with_the_Public,crosswalk, by="code", all=TRUE, sort=TRUE)

public_importance <- public_importance %>% filter(!is.na(noc_code))
public_importance <- public_importance[!names(public_importance) %in% c("Occupation")] %>% filter(!(noc_code %in% "NA"))
# # # # # # # View(public_importance)
### now take average by noc_code for each work factor
summary(public_importance)
public_importance$Performing_for_or_Working_Directly_with_the_Public <- as.numeric(public_importance$Performing_for_or_Working_Directly_with_the_Public)

public_importance <- public_importance %>% group_by(noc_code) %>% mutate_if(is.numeric,mean,na.rm=T) %>% mutate_if(is.numeric,round,0) %>% distinct(noc_code,.keep_all=T) %>% 
  select(-c(onet_title.x,onet_title.y,code,noc_title))
## # # # # # # View(public_importance)

public_importance$noc_code <- as.numeric(public_importance$noc_code)
public_importance$noc_code<-formatC(public_importance$noc_code, width = 4, format = "d", flag = "0")
public_importance$noc_code<-as.character(public_importance$noc_code)
# # # # # # # View(public_importance)

public_importance <- public_importance %>% select(noc_code,Performing_for_or_Working_Directly_with_the_Public)
summary(public_importance)
names(public_importance)



## merge all onet together
onet <- merge(work_from_home,phys_prox,by="noc_code",all=T)
onet <- merge(onet,exposed_disease,by="noc_code",all=T)
onet <- merge(onet,common_ppe,by="noc_code",all=T)
onet <- merge(onet,specialized_ppe,by="noc_code",all=T)
onet <- merge(onet,indoors_controlled,by="noc_code",all=T)
onet <- merge(onet,indoors_not_controlled,by="noc_code",all=T)
onet <- merge(onet,public_importance,by="noc_code",all=T)
onet <- merge(onet,contact_others,by="noc_code",all=T)

# View(onet)
names(onet)


onet <- onet %>% rename("exposure"="Exposed_to_Disease_or_Infections",
                        "phys_prox"="Physical_Proximity",
                        "public_importance"="Performing_for_or_Working_Directly_with_the_Public",
                        "indoors_controlled" = "Indoors_Environmentally_Controlled",
                        "indoors_not_controlled"="Indoors_Not_Environmentally_Controlled",
                        "contact_others"="Contact_with_Others"
)

### now import naics and noc code datasets
NOC_MERGE <- read_excel("NOC_MERGE.xlsx")
NOC_MERGE$noc_code <- as.numeric(NOC_MERGE$noc_code)
NOC_MERGE$noc_code<-formatC(NOC_MERGE$noc_code, width = 4, format = "d", flag = "0")
NOC_MERGE$noc_code<-as.character(NOC_MERGE$noc_code)
## View(NOC_MERGE)

NAICS_MERGE <- read_excel("NAICS_MERGE.xlsx")
NAICS_MERGE$naics_code <- as.numeric(NAICS_MERGE$naics_code)
NAICS_MERGE$naics_code<-formatC(NAICS_MERGE$naics_code, width = 4, format = "d", flag = "0")
NAICS_MERGE$naics_code<-as.character(NAICS_MERGE$naics_code)
## View(NAICS_MERGE)

save(onet,NOC_MERGE,NAICS_MERGE,file="onet_naics_noc.RData")
