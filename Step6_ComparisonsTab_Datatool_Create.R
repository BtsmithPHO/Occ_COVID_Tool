####################
## Occupational Risk Covid19 Code Repository

#######################
## Step6: Create Datatool Datasets for Comparisons tab
#######################



#### Occupation search input dataset
#####################################
#####################################
rm(list=ls())
load("onet_naics_noc.RData")
load("table1_r.RData")

# View(table1_r)
##########################################
## Sum NOC codes within services
##########################################

occup_search_tool <- table1_r %>% 
  group_by(geography, naics_sector_name, noc_code,sex, age) %>% 
  summarise_if(is.numeric, sum) %>%
  ungroup()

occup_search_tool$age=factor(occup_search_tool$age,
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

occup_search_tool$sex=factor(occup_search_tool$sex,
                             levels=c("Female",
                                      "Male",
                                      "Total - Sex"),
                             labels=c("Female",
                                      "Male",
                                      "Total"))

occup_search_tool <- occup_search_tool %>% filter(age=='Total' & sex=='Total')
names(occup_search_tool)
occup_search_tool <- occup_search_tool %>% select(geography,naics_sector_name,noc_code,sex,age,sum_total1)
###########################################
## MERGE WITH OCCUPATION MEASURES from ONET
############################################
occup_search_tool_onet <- merge(occup_search_tool,onet, by=c("noc_code"),all.x=T) 
# View(occup_search_tool_onet)

noc <- NOC_MERGE %>% select(noc_code,noc_code_class)
occup_search_tool_onet <- merge(occup_search_tool_onet,noc,by="noc_code",all.x=T)


occup_search_tool_onet <- occup_search_tool_onet %>% filter(!is.na(wfh_yes))
occup_search_tool_onet <- occup_search_tool_onet%>%mutate(noc_code_class=substring(noc_code_class,6))
occup_search_tool_onet<-occup_search_tool_onet%>%mutate(noc_code_class=gsub("\\s*\\([^\\)]+\\)","",as.character(noc_code_class)))
occup_search_tool_onet<-occup_search_tool_onet%>%mutate(noc_code_class= gsub('[0-9]+', '', noc_code_class))

## Save Datatool dataset:
saveRDS(occup_search_tool_onet, file = "occup_search_tool_onet.rds")


## Chart 3 in tool design Comparisons Tab; wfh high phys; high exp

library(dplyr)
library(scales)
library(tidyverse)

#####################################
#####################################
rm(list=ls())

load("onet_naics_noc.RData")
load("table1_r.RData")
load("table2_r.RData")


## organize by socio-dem; all are stratified by sex; geography; industry sector

#### 1) Visible Minority Status = Table1

vismin <- table1_r %>% filter(age=="Total - 15 years and over" & sex %in% c("Female","Male"))

vismin2 <- vismin %>%  select(geography,
                              naics_sector_name,
                              noc_code,sex,age,
                              sum_southasia1,
                              sum_easteasia1,
                              sum_black1,
                              sum_se_asia1,
                              sum_latin1,
                              sum_mideast1,
                              sum_multivismin1,
                              sum_nie1,
                              sum_notvismin1)

onet_merge <- onet %>% select(noc_code,wfh_yes,phys_prox,exposure)
onet_merge$phys_prox_high <- ifelse(onet_merge$phys_prox > 75, 1,0)
onet_merge$phys_prox_high <- ifelse(is.na(onet_merge$phys_prox), 0,onet_merge$phys_prox_high)

onet_merge$exposure_high <- ifelse(onet_merge$exposure >50, 1,0)
onet_merge$exposure_high <- ifelse(is.na(onet_merge$exposure),0,onet_merge$exposure_high)

onet_merge$wfh_yes <- ifelse(is.na(onet_merge$wfh_yes), 0,onet_merge$wfh_yes)

onet_merge <- onet_merge %>% select(noc_code,wfh_yes,phys_prox_high,exposure_high)

vismin3 <- merge(vismin2,onet_merge,by="noc_code",all.x=T)

vismin_all <- vismin3 %>% group_by(geography,naics_sector_name,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex)

vismin_allind <- vismin3 %>% group_by(geography,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,sex)
vismin_allind$naics_sector_name <- c("All Industry Sectors")

vismin_all <- rbind(vismin_all,vismin_allind)
# View(vismin_all)
# vismin_all$wfh_yes <- c('Total')
# vismin_all$phys_prox_high <- c('Total')
# vismin_all$exposure_high <- c('Total')

vismin_wfh <- vismin3 %>% group_by(geography,naics_sector_name,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         wfh_yes,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

vismin_wfh_allind <- vismin3 %>% group_by(geography,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         wfh_yes,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
vismin_wfh_allind$naics_sector_name <- c("All Industry Sectors")

vismin_wfh <- rbind(vismin_wfh,vismin_wfh_allind)

vismin_wfh_yes <- vismin_wfh %>% filter(wfh_yes==0) %>% select(-wfh_yes)

vismin_wfh_yes <- vismin_wfh_yes %>% rename(wfh_yes_sum_southasia1=sum_southasia1,
                                            wfh_yes_sum_easteasia1=sum_easteasia1,
                                            wfh_yes_sum_black1=sum_black1,
                                            wfh_yes_sum_se_asia1=sum_se_asia1,
                                            wfh_yes_sum_latin1=sum_latin1,
                                            wfh_yes_sum_mideast1=sum_mideast1,
                                            wfh_yes_sum_multivismin1=sum_multivismin1,
                                            wfh_yes_sum_nie1=sum_nie1,
                                            wfh_yes_sum_notvismin1=sum_notvismin1)


vismin_wfh_f <- merge(vismin_wfh_yes,vismin_all,by=c("geography","naics_sector_name","sex"),all=T)

vismin_wfh_f$percent_southasia1 <- ifelse(vismin_wfh_f$sum_southasia1>0, vismin_wfh_f$wfh_yes_sum_southasia1 / vismin_wfh_f$sum_southasia1*100, NA)
vismin_wfh_f$percent_easteasia1 <- ifelse(vismin_wfh_f$sum_easteasia1>0, vismin_wfh_f$wfh_yes_sum_easteasia1 / vismin_wfh_f$sum_easteasia1*100, NA)
vismin_wfh_f$percent_black1 <- ifelse(vismin_wfh_f$sum_black1>0, vismin_wfh_f$wfh_yes_sum_black1 / vismin_wfh_f$sum_black1*100, NA)
vismin_wfh_f$percent_se_asia1 <- ifelse(vismin_wfh_f$sum_se_asia1>0, vismin_wfh_f$wfh_yes_sum_se_asia1 / vismin_wfh_f$sum_se_asia1*100, NA)
vismin_wfh_f$percent_latin1 <- ifelse(vismin_wfh_f$sum_latin1>0, vismin_wfh_f$wfh_yes_sum_latin1 / vismin_wfh_f$sum_latin1*100, NA)
vismin_wfh_f$percent_mideast1 <- ifelse(vismin_wfh_f$sum_mideast1>0, vismin_wfh_f$wfh_yes_sum_mideast1 / vismin_wfh_f$sum_mideast1*100, NA)
vismin_wfh_f$percent_multivismin1 <- ifelse(vismin_wfh_f$sum_multivismin1>0, vismin_wfh_f$wfh_yes_sum_multivismin1 / vismin_wfh_f$sum_multivismin1*100, NA)
vismin_wfh_f$percent_nie1 <- ifelse(vismin_wfh_f$sum_nie1>0, vismin_wfh_f$wfh_yes_sum_nie1 / vismin_wfh_f$sum_nie1*100, NA)
vismin_wfh_f$percent_notvismin1 <- ifelse(vismin_wfh_f$sum_notvismin1>0, vismin_wfh_f$wfh_yes_sum_notvismin1 / vismin_wfh_f$sum_notvismin1*100, NA)


vismin_wfh_f <- vismin_wfh_f %>% 
  select(geography,naics_sector_name,sex,percent_southasia1,percent_easteasia1,percent_black1,percent_se_asia1,
         percent_latin1,percent_mideast1,percent_multivismin1,percent_nie1,percent_notvismin1) %>%
  mutate_if(is.numeric,round,0)

vismin_wfh_f <- vismin_wfh_f %>% gather(selected_char,percent,
                                        percent_southasia1,percent_easteasia1,percent_black1,percent_se_asia1,
                                        percent_latin1,percent_mideast1,percent_multivismin1,percent_nie1,percent_notvismin1)
# View(vismin_wfh_f)

vismin_phys_prox <- vismin3 %>% group_by(geography,naics_sector_name,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         phys_prox_high,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

vismin_phys_prox_allind <- vismin3 %>% group_by(geography,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         phys_prox_high,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
vismin_phys_prox_allind$naics_sector_name <- c("All Industry Sectors")

vismin_phys_prox <- rbind(vismin_phys_prox,vismin_phys_prox_allind)



vismin_phys_prox_high <- vismin_phys_prox %>% filter(phys_prox_high==1) %>% select(-phys_prox_high)

vismin_phys_prox_high <- vismin_phys_prox_high %>% rename(phys_prox_high_sum_southasia1=sum_southasia1,
                                                          phys_prox_high_sum_easteasia1=sum_easteasia1,
                                                          phys_prox_high_sum_black1=sum_black1,
                                                          phys_prox_high_sum_se_asia1=sum_se_asia1,
                                                          phys_prox_high_sum_latin1=sum_latin1,
                                                          phys_prox_high_sum_mideast1=sum_mideast1,
                                                          phys_prox_high_sum_multivismin1=sum_multivismin1,
                                                          phys_prox_high_sum_nie1=sum_nie1,
                                                          phys_prox_high_sum_notvismin1=sum_notvismin1)


vismin_phys_prox_f <- merge(vismin_phys_prox_high,vismin_all,by=c("geography","naics_sector_name","sex"),all=T)

vismin_phys_prox_f$percent_southasia1 <- ifelse(vismin_phys_prox_f$sum_southasia1>0, vismin_phys_prox_f$phys_prox_high_sum_southasia1 / vismin_phys_prox_f$sum_southasia1*100, NA)
vismin_phys_prox_f$percent_easteasia1 <- ifelse(vismin_phys_prox_f$sum_easteasia1>0, vismin_phys_prox_f$phys_prox_high_sum_easteasia1 / vismin_phys_prox_f$sum_easteasia1*100, NA)
vismin_phys_prox_f$percent_black1 <- ifelse(vismin_phys_prox_f$sum_black1>0, vismin_phys_prox_f$phys_prox_high_sum_black1 / vismin_phys_prox_f$sum_black1*100, NA)
vismin_phys_prox_f$percent_se_asia1 <- ifelse(vismin_phys_prox_f$sum_se_asia1>0, vismin_phys_prox_f$phys_prox_high_sum_se_asia1 / vismin_phys_prox_f$sum_se_asia1*100, NA)
vismin_phys_prox_f$percent_latin1 <- ifelse(vismin_phys_prox_f$sum_latin1>0, vismin_phys_prox_f$phys_prox_high_sum_latin1 / vismin_phys_prox_f$sum_latin1*100, NA)
vismin_phys_prox_f$percent_mideast1 <- ifelse(vismin_phys_prox_f$sum_mideast1>0, vismin_phys_prox_f$phys_prox_high_sum_mideast1 / vismin_phys_prox_f$sum_mideast1*100, NA)
vismin_phys_prox_f$percent_multivismin1 <- ifelse(vismin_phys_prox_f$sum_multivismin1>0, vismin_phys_prox_f$phys_prox_high_sum_multivismin1 / vismin_phys_prox_f$sum_multivismin1*100, NA)
vismin_phys_prox_f$percent_nie1 <- ifelse(vismin_phys_prox_f$sum_nie1>0, vismin_phys_prox_f$phys_prox_high_sum_nie1 / vismin_phys_prox_f$sum_nie1*100, NA)
vismin_phys_prox_f$percent_notvismin1 <- ifelse(vismin_phys_prox_f$sum_notvismin1>0, vismin_phys_prox_f$phys_prox_high_sum_notvismin1 / vismin_phys_prox_f$sum_notvismin1*100, NA)


vismin_phys_prox_f <- vismin_phys_prox_f %>% 
  select(geography,naics_sector_name,sex,percent_southasia1,percent_easteasia1,percent_black1,percent_se_asia1,
         percent_latin1,percent_mideast1,percent_multivismin1,percent_nie1,percent_notvismin1) %>%
  mutate_if(is.numeric,round,0)
# View(vismin_phys_prox_f)

vismin_phys_prox_f <- vismin_phys_prox_f %>% gather(selected_char,percent,
                                                    percent_southasia1,percent_easteasia1,percent_black1,percent_se_asia1,
                                                    percent_latin1,percent_mideast1,percent_multivismin1,percent_nie1,percent_notvismin1)


vismin_exposure <- vismin3 %>% group_by(geography,naics_sector_name,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         exposure_high,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

vismin_exposure_allind <- vismin3 %>% group_by(geography,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         exposure_high,
         sum_southasia1,
         sum_easteasia1,
         sum_black1,
         sum_se_asia1,
         sum_latin1,
         sum_mideast1,
         sum_multivismin1,
         sum_nie1,
         sum_notvismin1) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
vismin_exposure_allind$naics_sector_name <- c("All Industry Sectors")

vismin_exposure <- rbind(vismin_exposure,vismin_exposure_allind)


vismin_exposure_high <- vismin_exposure %>% filter(exposure_high==1) %>% select(-exposure_high)

vismin_exposure_high <- vismin_exposure_high %>% rename(exposure_high_sum_southasia1=sum_southasia1,
                                                        exposure_high_sum_easteasia1=sum_easteasia1,
                                                        exposure_high_sum_black1=sum_black1,
                                                        exposure_high_sum_se_asia1=sum_se_asia1,
                                                        exposure_high_sum_latin1=sum_latin1,
                                                        exposure_high_sum_mideast1=sum_mideast1,
                                                        exposure_high_sum_multivismin1=sum_multivismin1,
                                                        exposure_high_sum_nie1=sum_nie1,
                                                        exposure_high_sum_notvismin1=sum_notvismin1)

vismin_exposure_f <- merge(vismin_exposure_high,vismin_all,by=c("geography","naics_sector_name","sex"),all=T)

vismin_exposure_f$percent_southasia1 <- ifelse(vismin_exposure_f$sum_southasia1>0, vismin_exposure_f$exposure_high_sum_southasia1 / vismin_exposure_f$sum_southasia1*100, NA)
vismin_exposure_f$percent_easteasia1 <- ifelse(vismin_exposure_f$sum_easteasia1>0, vismin_exposure_f$exposure_high_sum_easteasia1 / vismin_exposure_f$sum_easteasia1*100, NA)
vismin_exposure_f$percent_black1 <- ifelse(vismin_exposure_f$sum_black1>0, vismin_exposure_f$exposure_high_sum_black1 / vismin_exposure_f$sum_black1*100, NA)
vismin_exposure_f$percent_se_asia1 <- ifelse(vismin_exposure_f$sum_se_asia1>0, vismin_exposure_f$exposure_high_sum_se_asia1 / vismin_exposure_f$sum_se_asia1*100, NA)
vismin_exposure_f$percent_latin1 <- ifelse(vismin_exposure_f$sum_latin1>0, vismin_exposure_f$exposure_high_sum_latin1 / vismin_exposure_f$sum_latin1*100, NA)
vismin_exposure_f$percent_mideast1 <- ifelse(vismin_exposure_f$sum_mideast1>0, vismin_exposure_f$exposure_high_sum_mideast1 / vismin_exposure_f$sum_mideast1*100, NA)
vismin_exposure_f$percent_multivismin1 <- ifelse(vismin_exposure_f$sum_multivismin1>0, vismin_exposure_f$exposure_high_sum_multivismin1 / vismin_exposure_f$sum_multivismin1*100, NA)
vismin_exposure_f$percent_nie1 <- ifelse(vismin_exposure_f$sum_nie1>0, vismin_exposure_f$exposure_high_sum_nie1 / vismin_exposure_f$sum_nie1*100, NA)
vismin_exposure_f$percent_notvismin1 <- ifelse(vismin_exposure_f$sum_notvismin1>0, vismin_exposure_f$exposure_high_sum_notvismin1 / vismin_exposure_f$sum_notvismin1*100, NA)


vismin_exposure_f <- vismin_exposure_f %>% 
  select(geography,naics_sector_name,sex,percent_southasia1,percent_easteasia1,percent_black1,percent_se_asia1,
         percent_latin1,percent_mideast1,percent_multivismin1,percent_nie1,percent_notvismin1) %>%
  mutate_if(is.numeric,round,0)


vismin_exposure_f <- vismin_exposure_f %>% gather(selected_char,percent,
                                                  percent_southasia1,percent_easteasia1,percent_black1,percent_se_asia1,
                                                  percent_latin1,percent_mideast1,percent_multivismin1,percent_nie1,percent_notvismin1)
# View(vismin_exposure_f)


save(vismin_wfh_f,vismin_phys_prox_f,vismin_exposure_f,file="vismin_chart.RData")



#### 2) Immigrant Status (Census Table 1)
#### 

immig <- table1_r %>% filter(age=="Total - 15 years and over" & sex %in% c("Female","Male"))

immig2 <- immig %>%  select(geography,
                            naics_sector_name,
                            noc_code,sex,age,
                            sum_immig1,
                            sum_nonpermres1,
                            sum_nonimmig1
)

onet_merge <- onet %>% select(noc_code,wfh_yes,phys_prox,exposure)
onet_merge$phys_prox_high <- ifelse(onet_merge$phys_prox > 75, 1,0)
onet_merge$phys_prox_high <- ifelse(is.na(onet_merge$phys_prox), 0,onet_merge$phys_prox_high)

onet_merge$exposure_high <- ifelse(onet_merge$exposure >50, 1,0)
onet_merge$exposure_high <- ifelse(is.na(onet_merge$exposure),0,onet_merge$exposure_high)

onet_merge$wfh_yes <- ifelse(is.na(onet_merge$wfh_yes), 0,onet_merge$wfh_yes)

onet_merge <- onet_merge %>% select(noc_code,wfh_yes,phys_prox_high,exposure_high)

immig3 <- merge(immig2,onet_merge,by="noc_code",all.x=T)

immig_all <- immig3 %>% group_by(geography,naics_sector_name,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex)

immig_allind <- immig3 %>% group_by(geography,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,sex)
immig_allind$naics_sector_name <- c("All Industry Sectors")

immig_all <- rbind(immig_all,immig_allind)
# View(immig_all)
# immig_all$wfh_yes <- c('Total')
# immig_all$phys_prox_high <- c('Total')
# immig_all$exposure_high <- c('Total')

immig_wfh <- immig3 %>% group_by(geography,naics_sector_name,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         wfh_yes,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

immig_wfh_allind <- immig3 %>% group_by(geography,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         wfh_yes,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
immig_wfh_allind$naics_sector_name <- c("All Industry Sectors")

immig_wfh <- rbind(immig_wfh,immig_wfh_allind)

immig_wfh_yes <- immig_wfh %>% filter(wfh_yes==0) %>% select(-wfh_yes)

immig_wfh_yes <- immig_wfh_yes %>% rename(wfh_yes_sum_immig1=sum_immig1,
                                          wfh_yes_sum_nonpermres1=sum_nonpermres1,
                                          wfh_yes_sum_nonimmig1=sum_nonimmig1)



immig_wfh_f <- merge(immig_wfh_yes,immig_all,by=c("geography","naics_sector_name","sex"),all=T)

immig_wfh_f$percent_immig1 <- ifelse(immig_wfh_f$sum_immig1>0, immig_wfh_f$wfh_yes_sum_immig1 / immig_wfh_f$sum_immig1*100, NA)
immig_wfh_f$percent_nonpermres1 <- ifelse(immig_wfh_f$sum_nonpermres1>0, immig_wfh_f$wfh_yes_sum_nonpermres1 / immig_wfh_f$sum_nonpermres1*100, NA)
immig_wfh_f$percent_nonimmig1 <- ifelse(immig_wfh_f$sum_nonimmig1>0, immig_wfh_f$wfh_yes_sum_nonimmig1 / immig_wfh_f$sum_nonimmig1*100, NA)



immig_wfh_f <- immig_wfh_f %>% 
  select(geography,naics_sector_name,sex,percent_immig1,percent_nonpermres1,percent_nonimmig1) %>%
  mutate_if(is.numeric,round,0)

immig_wfh_f <- immig_wfh_f %>% gather(selected_char,percent,
                                      percent_immig1,percent_nonpermres1,percent_nonimmig1)
# View(immig_wfh_f)

## physical prox
immig_phys_prox <- immig3 %>% group_by(geography,naics_sector_name,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         phys_prox_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

immig_phys_prox_allind <- immig3 %>% group_by(geography,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         phys_prox_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
immig_phys_prox_allind$naics_sector_name <- c("All Industry Sectors")

immig_phys_prox <- rbind(immig_phys_prox,immig_phys_prox_allind)

immig_phys_prox_high <- immig_phys_prox %>% filter(phys_prox_high==1) %>% select(-phys_prox_high)

immig_phys_prox_high <- immig_phys_prox_high %>% rename(phys_prox_high_sum_immig1=sum_immig1,
                                                        phys_prox_high_sum_nonpermres1=sum_nonpermres1,
                                                        phys_prox_high_sum_nonimmig1=sum_nonimmig1)



immig_phys_prox_f <- merge(immig_phys_prox_high,immig_all,by=c("geography","naics_sector_name","sex"),all=T)

immig_phys_prox_f$percent_immig1 <- ifelse(immig_phys_prox_f$sum_immig1>0, immig_phys_prox_f$phys_prox_high_sum_immig1 / immig_phys_prox_f$sum_immig1*100, NA)
immig_phys_prox_f$percent_nonpermres1 <- ifelse(immig_phys_prox_f$sum_nonpermres1>0, immig_phys_prox_f$phys_prox_high_sum_nonpermres1 / immig_phys_prox_f$sum_nonpermres1*100, NA)
immig_phys_prox_f$percent_nonimmig1 <- ifelse(immig_phys_prox_f$sum_nonimmig1>0, immig_phys_prox_f$phys_prox_high_sum_nonimmig1 / immig_phys_prox_f$sum_nonimmig1*100, NA)



immig_phys_prox_f <- immig_phys_prox_f %>% 
  select(geography,naics_sector_name,sex,percent_immig1,percent_nonpermres1,percent_nonimmig1) %>%
  mutate_if(is.numeric,round,0)

immig_phys_prox_f <- immig_phys_prox_f %>% gather(selected_char,percent,
                                                  percent_immig1,percent_nonpermres1,percent_nonimmig1)

# exposure
immig_exposure <- immig3 %>% group_by(geography,naics_sector_name,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         exposure_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

immig_exposure_allind <- immig3 %>% group_by(geography,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         exposure_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
immig_exposure_allind$naics_sector_name <- c("All Industry Sectors")

immig_exposure <- rbind(immig_exposure,immig_exposure_allind)

immig_exposure_high <- immig_exposure %>% filter(exposure_high==1) %>% select(-exposure_high)

immig_exposure_high <- immig_exposure_high %>% rename(exposure_high_sum_immig1=sum_immig1,
                                                      exposure_high_sum_nonpermres1=sum_nonpermres1,
                                                      exposure_high_sum_nonimmig1=sum_nonimmig1)



immig_exposure_f <- merge(immig_exposure_high,immig_all,by=c("geography","naics_sector_name","sex"),all=T)

immig_exposure_f$percent_immig1 <- ifelse(immig_exposure_f$sum_immig1>0, immig_exposure_f$exposure_high_sum_immig1 / immig_exposure_f$sum_immig1*100, NA)
immig_exposure_f$percent_nonpermres1 <- ifelse(immig_exposure_f$sum_nonpermres1>0, immig_exposure_f$exposure_high_sum_nonpermres1 / immig_exposure_f$sum_nonpermres1*100, NA)
immig_exposure_f$percent_nonimmig1 <- ifelse(immig_exposure_f$sum_nonimmig1>0, immig_exposure_f$exposure_high_sum_nonimmig1 / immig_exposure_f$sum_nonimmig1*100, NA)



immig_exposure_f <- immig_exposure_f %>% 
  select(geography,naics_sector_name,sex,percent_immig1,percent_nonpermres1,percent_nonimmig1) %>%
  mutate_if(is.numeric,round,0)

immig_exposure_f <- immig_exposure_f %>% gather(selected_char,percent,
                                                percent_immig1,percent_nonpermres1,percent_nonimmig1)

save(immig_wfh_f,immig_phys_prox_f,immig_exposure_f,file="immig_chart.RData")




#### Age (Census Table 1)
#### 

age_group <- table1_r %>% filter(sex %in% c("Female","Male"))

age_group2 <- age_group %>%  select(geography,
                                    naics_sector_name,
                                    noc_code,sex,age,sum_total1)

# View(age_group2)

onet_merge <- onet %>% select(noc_code,wfh_yes,phys_prox,exposure)
onet_merge$phys_prox_high <- ifelse(onet_merge$phys_prox > 75, 1,0)
onet_merge$phys_prox_high <- ifelse(is.na(onet_merge$phys_prox), 0,onet_merge$phys_prox_high)

onet_merge$exposure_high <- ifelse(onet_merge$exposure >50, 1,0)
onet_merge$exposure_high <- ifelse(is.na(onet_merge$exposure),0,onet_merge$exposure_high)

onet_merge$wfh_yes <- ifelse(is.na(onet_merge$wfh_yes), 0,onet_merge$wfh_yes)

onet_merge <- onet_merge %>% select(noc_code,wfh_yes,phys_prox_high,exposure_high)

age_group3 <- merge(age_group2,onet_merge,by="noc_code",all.x=T)

age_group_all <- age_group3 %>% group_by(geography,naics_sector_name,sex,age) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex,age)

age_group_allind <- age_group3 %>% group_by(geography,sex,age) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,sex,age)
age_group_allind$naics_sector_name <- c("All Industry Sectors")

age_group_all <- rbind(age_group_all,age_group_allind)
# View(age_group_all)
# age_group_all$wfh_yes <- c('Total')
# age_group_all$phys_prox_high <- c('Total')
# age_group_all$exposure_high <- c('Total')

age_group_wfh <- age_group3 %>% group_by(geography,naics_sector_name,sex,age,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         wfh_yes,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex,age) %>%
  ungroup()

age_group_wfh_allind <- age_group3 %>% group_by(geography,sex,age,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         wfh_yes,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,sex,age) %>%
  ungroup()
age_group_wfh_allind$naics_sector_name <- c("All Industry Sectors")

age_group_wfh <- rbind(age_group_wfh,age_group_wfh_allind)

age_group_wfh_yes <- age_group_wfh %>% filter(wfh_yes==0) %>% select(-wfh_yes)

age_group_wfh_yes <- age_group_wfh_yes %>% rename(wfh_yes_sum_total1=sum_total1
)



age_group_wfh_f <- merge(age_group_wfh_yes,age_group_all,by=c("geography","naics_sector_name","sex","age"),all=T)

age_group_wfh_f$percent_sum_total1 <- ifelse(age_group_wfh_f$sum_total1>0, age_group_wfh_f$wfh_yes_sum_total1 / age_group_wfh_f$sum_total1*100, NA)


age_group_wfh_f <- age_group_wfh_f %>% 
  select(geography,naics_sector_name,sex,age,percent_sum_total1) %>%
  mutate_if(is.numeric,round,0) %>%
  rename(selected_char=age,
         percent=percent_sum_total1)


# View(age_group_wfh_f)

## physical prox
age_group_phys_prox <- age_group3 %>% group_by(geography,naics_sector_name,sex,age,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         phys_prox_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex,age) %>%
  ungroup()

age_group_phys_prox_allind <- age_group3 %>% group_by(geography,sex,age,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         phys_prox_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,sex,age) %>%
  ungroup()
age_group_phys_prox_allind$naics_sector_name <- c("All Industry Sectors")

age_group_phys_prox <- rbind(age_group_phys_prox,age_group_phys_prox_allind)

age_group_phys_prox_high <- age_group_phys_prox %>% filter(phys_prox_high==1) %>% select(-phys_prox_high)

age_group_phys_prox_high <- age_group_phys_prox_high %>% rename(phys_prox_high_sum_total1=sum_total1
)



age_group_phys_prox_f <- merge(age_group_phys_prox_high,age_group_all,by=c("geography","naics_sector_name","sex","age"),all=T)

age_group_phys_prox_f$percent_sum_total1 <- ifelse(age_group_phys_prox_f$sum_total1>0, age_group_phys_prox_f$phys_prox_high_sum_total1 / age_group_phys_prox_f$sum_total1*100, NA)


age_group_phys_prox_f <- age_group_phys_prox_f %>% 
  select(geography,naics_sector_name,sex,age,percent_sum_total1) %>%
  mutate_if(is.numeric,round,0) %>%
  rename(selected_char=age,
         percent=percent_sum_total1)


# View(age_group_phys_prox_f)

#exposure
age_group_exposure <- age_group3 %>% group_by(geography,naics_sector_name,sex,age,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         exposure_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex,age) %>%
  ungroup()

age_group_exposure_allind <- age_group3 %>% group_by(geography,sex,age,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         exposure_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography,sex,age) %>%
  ungroup()
age_group_exposure_allind$naics_sector_name <- c("All Industry Sectors")

age_group_exposure <- rbind(age_group_exposure,age_group_exposure_allind)

age_group_exposure_high <- age_group_exposure %>% filter(exposure_high==1) %>% select(-exposure_high)

age_group_exposure_high <- age_group_exposure_high %>% rename(exposure_high_sum_total1=sum_total1
)



age_group_exposure_f <- merge(age_group_exposure_high,age_group_all,by=c("geography","naics_sector_name","sex","age"),all=T)

age_group_exposure_f$percent_sum_total1 <- ifelse(age_group_exposure_f$sum_total1>0, age_group_exposure_f$exposure_high_sum_total1 / age_group_exposure_f$sum_total1*100, NA)


age_group_exposure_f <- age_group_exposure_f %>% 
  select(geography,naics_sector_name,sex,age,percent_sum_total1) %>%
  mutate_if(is.numeric,round,0) %>%
  rename(selected_char=age,
         percent=percent_sum_total1)

save(age_group_wfh_f,age_group_phys_prox_f,age_group_exposure_f,file="age_group_chart.RData")


#### 2) Income Quintile (Census Table 2)
#### 

income <- table2_r %>% filter(age=="Total -  Population 15 years and over" & sex %in% c("Female","Male"))

income2 <- income %>%  select(geography,
                              naics_sector_name,
                              noc_code,sex,age,
                              sum_quintile1,
                              sum_quintile2,
                              sum_quintile3,
                              sum_quintile4,
                              sum_quintile5
)

onet_merge <- onet %>% select(noc_code,wfh_yes,phys_prox,exposure)
onet_merge$phys_prox_high <- ifelse(onet_merge$phys_prox > 75, 1,0)
onet_merge$phys_prox_high <- ifelse(is.na(onet_merge$phys_prox), 0,onet_merge$phys_prox_high)

onet_merge$exposure_high <- ifelse(onet_merge$exposure >50, 1,0)
onet_merge$exposure_high <- ifelse(is.na(onet_merge$exposure),0,onet_merge$exposure_high)

onet_merge$wfh_yes <- ifelse(is.na(onet_merge$wfh_yes), 0,onet_merge$wfh_yes)

onet_merge <- onet_merge %>% select(noc_code,wfh_yes,phys_prox_high,exposure_high)

income3 <- merge(income2,onet_merge,by="noc_code",all.x=T)

income_all <- income3 %>% group_by(geography,naics_sector_name,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex)

income_allind <- income3 %>% group_by(geography,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,sex)
income_allind$naics_sector_name <- c("All Industry Sectors")

income_all <- rbind(income_all,income_allind)
# View(income_all)
# income_all$wfh_yes <- c('Total')
# income_all$phys_prox_high <- c('Total')
# income_all$exposure_high <- c('Total')

income_wfh <- income3 %>% group_by(geography,naics_sector_name,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         wfh_yes,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

income_wfh_allind <- income3 %>% group_by(geography,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         wfh_yes,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
income_wfh_allind$naics_sector_name <- c("All Industry Sectors")

income_wfh <- rbind(income_wfh,income_wfh_allind)

income_wfh_yes <- income_wfh %>% filter(wfh_yes==0) %>% select(-wfh_yes)

income_wfh_yes <- income_wfh_yes %>% rename(wfh_yes_sum_quintile1=sum_quintile1,
                                            wfh_yes_sum_quintile2=sum_quintile2,
                                            wfh_yes_sum_quintile3=sum_quintile3,
                                            wfh_yes_sum_quintile4=sum_quintile4,
                                            wfh_yes_sum_quintile5=sum_quintile5)



income_wfh_f <- merge(income_wfh_yes,income_all,by=c("geography","naics_sector_name","sex"),all=T)

income_wfh_f$percent_quintile1 <- ifelse(income_wfh_f$sum_quintile1>0, income_wfh_f$wfh_yes_sum_quintile1 / income_wfh_f$sum_quintile1*100, NA)
income_wfh_f$percent_quintile2 <- ifelse(income_wfh_f$sum_quintile1>0, income_wfh_f$wfh_yes_sum_quintile2 / income_wfh_f$sum_quintile2*100, NA)
income_wfh_f$percent_quintile3 <- ifelse(income_wfh_f$sum_quintile1>0, income_wfh_f$wfh_yes_sum_quintile3 / income_wfh_f$sum_quintile3*100, NA)
income_wfh_f$percent_quintile4 <- ifelse(income_wfh_f$sum_quintile1>0, income_wfh_f$wfh_yes_sum_quintile4 / income_wfh_f$sum_quintile4*100, NA)
income_wfh_f$percent_quintile5 <- ifelse(income_wfh_f$sum_quintile1>0, income_wfh_f$wfh_yes_sum_quintile5 / income_wfh_f$sum_quintile5*100, NA)

income_wfh_f <- income_wfh_f %>% 
  select(geography,naics_sector_name,sex,percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5) %>%
  mutate_if(is.numeric,round,0)

income_wfh_f <- income_wfh_f %>% gather(selected_char,percent,
                                        percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5)
# View(income_wfh_f)

## physical prox
income_phys_prox <- income3 %>% group_by(geography,naics_sector_name,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         phys_prox_high,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

income_phys_prox_allind <- income3 %>% group_by(geography,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         phys_prox_high,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
income_phys_prox_allind$naics_sector_name <- c("All Industry Sectors")

income_phys_prox <- rbind(income_phys_prox,income_phys_prox_allind)

income_phys_prox_high <- income_phys_prox %>% filter(phys_prox_high==1) %>% select(-phys_prox_high)

income_phys_prox_high <- income_phys_prox_high %>% rename(phys_prox_high_sum_quintile1=sum_quintile1,
                                                          phys_prox_high_sum_quintile2=sum_quintile2,
                                                          phys_prox_high_sum_quintile3=sum_quintile3,
                                                          phys_prox_high_sum_quintile4=sum_quintile4,
                                                          phys_prox_high_sum_quintile5=sum_quintile5)



income_phys_prox_f <- merge(income_phys_prox_high,income_all,by=c("geography","naics_sector_name","sex"),all=T)

income_phys_prox_f$percent_quintile1 <- ifelse(income_phys_prox_f$sum_quintile1>0, income_phys_prox_f$phys_prox_high_sum_quintile1 / income_phys_prox_f$sum_quintile1*100, NA)
income_phys_prox_f$percent_quintile2 <- ifelse(income_phys_prox_f$sum_quintile1>0, income_phys_prox_f$phys_prox_high_sum_quintile2 / income_phys_prox_f$sum_quintile2*100, NA)
income_phys_prox_f$percent_quintile3 <- ifelse(income_phys_prox_f$sum_quintile1>0, income_phys_prox_f$phys_prox_high_sum_quintile3 / income_phys_prox_f$sum_quintile3*100, NA)
income_phys_prox_f$percent_quintile4 <- ifelse(income_phys_prox_f$sum_quintile1>0, income_phys_prox_f$phys_prox_high_sum_quintile4 / income_phys_prox_f$sum_quintile4*100, NA)
income_phys_prox_f$percent_quintile5 <- ifelse(income_phys_prox_f$sum_quintile1>0, income_phys_prox_f$phys_prox_high_sum_quintile5 / income_phys_prox_f$sum_quintile5*100, NA)

income_phys_prox_f <- income_phys_prox_f %>% 
  select(geography,naics_sector_name,sex,percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5) %>%
  mutate_if(is.numeric,round,0)

income_phys_prox_f <- income_phys_prox_f %>% gather(selected_char,percent,
                                                    percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5)
# View(income_phys_prox_f)

#exposure
income_exposure <- income3 %>% group_by(geography,naics_sector_name,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         naics_sector_name,
         sex,
         exposure_high,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,naics_sector_name,sex) %>%
  ungroup()

income_exposure_allind <- income3 %>% group_by(geography,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,
         sex,
         exposure_high,
         sum_quintile1,
         sum_quintile2,
         sum_quintile3,
         sum_quintile4,
         sum_quintile5) %>%
  distinct() %>% 
  arrange(geography,sex) %>%
  ungroup()
income_exposure_allind$naics_sector_name <- c("All Industry Sectors")

income_exposure <- rbind(income_exposure,income_exposure_allind)

income_exposure_high <- income_exposure %>% filter(exposure_high==1) %>% select(-exposure_high)

income_exposure_high <- income_exposure_high %>% rename(exposure_high_sum_quintile1=sum_quintile1,
                                                        exposure_high_sum_quintile2=sum_quintile2,
                                                        exposure_high_sum_quintile3=sum_quintile3,
                                                        exposure_high_sum_quintile4=sum_quintile4,
                                                        exposure_high_sum_quintile5=sum_quintile5)



income_exposure_f <- merge(income_exposure_high,income_all,by=c("geography","naics_sector_name","sex"),all=T)

income_exposure_f$percent_quintile1 <- ifelse(income_exposure_f$sum_quintile1>0, income_exposure_f$exposure_high_sum_quintile1 / income_exposure_f$sum_quintile1*100, NA)
income_exposure_f$percent_quintile2 <- ifelse(income_exposure_f$sum_quintile1>0, income_exposure_f$exposure_high_sum_quintile2 / income_exposure_f$sum_quintile2*100, NA)
income_exposure_f$percent_quintile3 <- ifelse(income_exposure_f$sum_quintile1>0, income_exposure_f$exposure_high_sum_quintile3 / income_exposure_f$sum_quintile3*100, NA)
income_exposure_f$percent_quintile4 <- ifelse(income_exposure_f$sum_quintile1>0, income_exposure_f$exposure_high_sum_quintile4 / income_exposure_f$sum_quintile4*100, NA)
income_exposure_f$percent_quintile5 <- ifelse(income_exposure_f$sum_quintile1>0, income_exposure_f$exposure_high_sum_quintile5 / income_exposure_f$sum_quintile5*100, NA)

income_exposure_f <- income_exposure_f %>% 
  select(geography,naics_sector_name,sex,percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5) %>%
  mutate_if(is.numeric,round,0)

income_exposure_f <- income_exposure_f %>% gather(selected_char,percent,
                                                  percent_quintile1,percent_quintile2,percent_quintile3,percent_quintile4,percent_quintile5)
# View(income_exposure_f)

save(income_wfh_f,income_phys_prox_f,income_exposure_f,file="income_chart.RData")



## regions
load("table3_r.RData")

table3_r <- table3_r%>%mutate(health_region=substring(health_region,6))


##########################################
## Apply PHO Operational names for PHUs in Ontario
##########################################

table3_r$health_region_ontario = factor(
  table3_r$health_region,
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



table3_r$health_region <- ifelse(is.na(table3_r$health_region),"Peterborough Public Health",table3_r$health_region)

table3_r$health_region_ontario <- as.character(table3_r$health_region_ontario)
table3_r$health_region <- ifelse(table3_r$geography %in% "Ontario", table3_r$health_region_ontario,table3_r$health_region)

table3_r <- table3_r %>% select(-health_region_ontario)

#### 1) Visible Minority Status = Table3

vismin <- table3_r %>% filter(age=="Total - 15 years and over" & sex %in% c("Female","Male"))

vismin2 <- vismin %>%  select(geography,health_region,
                              naics_sector_name,
                              noc_code,sex,age,
                              sum_total1,
                              sum_vismin1,
                              sum_aboriginal1)

vismin2$sum_notvismin1 <- ifelse(vismin2$sum_total1 >=(vismin2$sum_vismin1 + vismin2$sum_aboriginal1),vismin2$sum_total1 -(vismin2$sum_vismin1 + vismin2$sum_aboriginal1),0)

vismin2 <- vismin2 %>%  select(geography,health_region,
                               naics_sector_name,
                               noc_code,sex,age,
                               sum_notvismin1,
                               sum_vismin1
)

onet_merge <- onet %>% select(noc_code,wfh_yes,phys_prox,exposure)
onet_merge$phys_prox_high <- ifelse(onet_merge$phys_prox > 75, 1,0)
onet_merge$phys_prox_high <- ifelse(is.na(onet_merge$phys_prox), 0,onet_merge$phys_prox_high)

onet_merge$exposure_high <- ifelse(onet_merge$exposure >50, 1,0)
onet_merge$exposure_high <- ifelse(is.na(onet_merge$exposure),0,onet_merge$exposure_high)

onet_merge$wfh_yes <- ifelse(is.na(onet_merge$wfh_yes), 0,onet_merge$wfh_yes)

onet_merge <- onet_merge %>% select(noc_code,wfh_yes,phys_prox_high,exposure_high)

vismin3 <- merge(vismin2,onet_merge,by="noc_code",all.x=T)

vismin_all <- vismin3 %>% group_by(geography,health_region,naics_sector_name,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         naics_sector_name,
         sex,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,naics_sector_name,sex)

vismin_allind <- vismin3 %>% group_by(geography,health_region,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         sex,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,sex)
vismin_allind$naics_sector_name <- c("All Industry Sectors")

vismin_all <- rbind(vismin_all,vismin_allind)


vismin_wfh <- vismin3 %>% group_by(geography,health_region,naics_sector_name,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         naics_sector_name,
         sex,
         wfh_yes,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,naics_sector_name,sex) %>%
  ungroup()

vismin_wfh_allind <- vismin3 %>% group_by(geography,health_region,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         sex,
         wfh_yes,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,sex) %>%
  ungroup()
vismin_wfh_allind$naics_sector_name <- c("All Industry Sectors")

vismin_wfh <- rbind(vismin_wfh,vismin_wfh_allind)

vismin_wfh_yes <- vismin_wfh %>% filter(wfh_yes==0) %>% select(-wfh_yes)

vismin_wfh_yes <- vismin_wfh_yes %>% rename(wfh_yes_sum_notvismin1=sum_notvismin1,
                                            wfh_yes_sum_vismin1=sum_vismin1
)


vismin_wfh_f <- merge(vismin_wfh_yes,vismin_all,by=c("geography","health_region", "naics_sector_name","sex"),all=T)

vismin_wfh_f$percent_notvismin1 <- ifelse(vismin_wfh_f$sum_notvismin1>0, vismin_wfh_f$wfh_yes_sum_notvismin1 / vismin_wfh_f$sum_notvismin1*100, NA)
vismin_wfh_f$percent_vismin1 <- ifelse(vismin_wfh_f$sum_vismin1>0, vismin_wfh_f$wfh_yes_sum_vismin1 / vismin_wfh_f$sum_vismin1*100, NA)


vismin_wfh_f <- vismin_wfh_f %>% 
  select(geography,health_region,naics_sector_name,sex,percent_notvismin1,percent_vismin1) %>%
  mutate_if(is.numeric,round,0)

hr_vismin_wfh_f <- vismin_wfh_f %>% gather(selected_char,percent,
                                           percent_notvismin1,percent_vismin1)
# View(hr_vismin_wfh_f)


## phys prox
vismin_phys_prox <- vismin3 %>% group_by(geography,health_region,naics_sector_name,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         naics_sector_name,
         sex,
         phys_prox_high,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,naics_sector_name,sex) %>%
  ungroup()

vismin_phys_prox_allind <- vismin3 %>% group_by(geography,health_region,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         sex,
         phys_prox_high,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,sex) %>%
  ungroup()
vismin_phys_prox_allind$naics_sector_name <- c("All Industry Sectors")

vismin_phys_prox <- rbind(vismin_phys_prox,vismin_phys_prox_allind)

vismin_phys_prox_high <- vismin_phys_prox %>% filter(phys_prox_high==1) %>% select(-phys_prox_high)

vismin_phys_prox_high <- vismin_phys_prox_high %>% rename(phys_prox_high_sum_notvismin1=sum_notvismin1,
                                                          phys_prox_high_sum_vismin1=sum_vismin1
)


vismin_phys_prox_f <- merge(vismin_phys_prox_high,vismin_all,by=c("geography","health_region", "naics_sector_name","sex"),all=T)

vismin_phys_prox_f$percent_notvismin1 <- ifelse(vismin_phys_prox_f$sum_notvismin1>0, vismin_phys_prox_f$phys_prox_high_sum_notvismin1 / vismin_phys_prox_f$sum_notvismin1*100, NA)
vismin_phys_prox_f$percent_vismin1 <- ifelse(vismin_phys_prox_f$sum_vismin1>0, vismin_phys_prox_f$phys_prox_high_sum_vismin1 / vismin_phys_prox_f$sum_vismin1*100, NA)

vismin_phys_prox_f <- vismin_phys_prox_f %>% 
  select(geography,health_region,naics_sector_name,sex,percent_notvismin1,percent_vismin1) %>%
  mutate_if(is.numeric,round,0)

hr_vismin_phys_prox_f <- vismin_phys_prox_f %>% gather(selected_char,percent,
                                                       percent_notvismin1,percent_vismin1)

## exposure
vismin_exposure <- vismin3 %>% group_by(geography,health_region,naics_sector_name,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         naics_sector_name,
         sex,
         exposure_high,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,naics_sector_name,sex) %>%
  ungroup()

vismin_exposure_allind <- vismin3 %>% group_by(geography,health_region,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography,health_region,
         sex,
         exposure_high,
         sum_notvismin1,
         sum_vismin1) %>%
  distinct() %>% 
  arrange(geography,health_region,sex) %>%
  ungroup()
vismin_exposure_allind$naics_sector_name <- c("All Industry Sectors")

vismin_exposure <- rbind(vismin_exposure,vismin_exposure_allind)

vismin_exposure_high <- vismin_exposure %>% filter(exposure_high==1) %>% select(-exposure_high)

vismin_exposure_high <- vismin_exposure_high %>% rename(exposure_high_sum_notvismin1=sum_notvismin1,
                                                        exposure_high_sum_vismin1=sum_vismin1
)


vismin_exposure_f <- merge(vismin_exposure_high,vismin_all,by=c("geography","health_region", "naics_sector_name","sex"),all=T)

vismin_exposure_f$percent_notvismin1 <- ifelse(vismin_exposure_f$sum_notvismin1>0, vismin_exposure_f$exposure_high_sum_notvismin1 / vismin_exposure_f$sum_notvismin1*100, NA)
vismin_exposure_f$percent_vismin1 <- ifelse(vismin_exposure_f$sum_vismin1>0, vismin_exposure_f$exposure_high_sum_vismin1 / vismin_exposure_f$sum_vismin1*100, NA)

vismin_exposure_f <- vismin_exposure_f %>% 
  select(geography,health_region,naics_sector_name,sex,percent_notvismin1,percent_vismin1) %>%
  mutate_if(is.numeric,round,0)

hr_vismin_exposure_f <- vismin_exposure_f %>% gather(selected_char,percent,
                                                     percent_notvismin1,percent_vismin1)

save(hr_vismin_wfh_f,hr_vismin_phys_prox_f,hr_vismin_exposure_f,file="hr_vismin_chart.RData")


#### 2) Immigrant Status (Census Table 3)
#### 

immig <- table3_r %>% filter(age=="Total - 15 years and over" & sex %in% c("Female","Male"))

immig2 <- immig %>%  select(geography, health_region,
                            naics_sector_name,
                            noc_code,sex,age,
                            sum_total1,
                            sum_immig1,
                            sum_nonpermres1
)

immig2$sum_nonimmig1 <- ifelse(immig2$sum_total1 >= (immig2$sum_immig1+immig2$sum_nonpermres1), (immig2$sum_total1 -(immig2$sum_immig1+immig2$sum_nonpermres1)), 0)
immig2 <- immig2 %>%  select(geography, health_region,
                             naics_sector_name,
                             noc_code,sex,age,
                             sum_nonimmig1,
                             sum_immig1,
                             sum_nonpermres1
)
onet_merge <- onet %>% select(noc_code,wfh_yes,phys_prox,exposure)
onet_merge$phys_prox_high <- ifelse(onet_merge$phys_prox > 75, 1,0)
onet_merge$phys_prox_high <- ifelse(is.na(onet_merge$phys_prox), 0,onet_merge$phys_prox_high)

onet_merge$exposure_high <- ifelse(onet_merge$exposure >50, 1,0)
onet_merge$exposure_high <- ifelse(is.na(onet_merge$exposure),0,onet_merge$exposure_high)

onet_merge$wfh_yes <- ifelse(is.na(onet_merge$wfh_yes), 0,onet_merge$wfh_yes)

onet_merge <- onet_merge %>% select(noc_code,wfh_yes,phys_prox_high,exposure_high)

immig3 <- merge(immig2,onet_merge,by="noc_code",all.x=T)

immig_all <- immig3 %>% group_by(geography, health_region,naics_sector_name,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex)

immig_allind <- immig3 %>% group_by(geography, health_region,sex) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex)
immig_allind$naics_sector_name <- c("All Industry Sectors")

immig_all <- rbind(immig_all,immig_allind)
# View(immig_all)
# immig_all$wfh_yes <- c('Total')
# immig_all$phys_prox_high <- c('Total')
# immig_all$exposure_high <- c('Total')

immig_wfh <- immig3 %>% group_by(geography, health_region,naics_sector_name,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         wfh_yes,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex) %>%
  ungroup()

immig_wfh_allind <- immig3 %>% group_by(geography, health_region,sex,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         wfh_yes,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex) %>%
  ungroup()
immig_wfh_allind$naics_sector_name <- c("All Industry Sectors")

immig_wfh <- rbind(immig_wfh,immig_wfh_allind)

immig_wfh_yes <- immig_wfh %>% filter(wfh_yes==0) %>% select(-wfh_yes)

immig_wfh_yes <- immig_wfh_yes %>% rename(wfh_yes_sum_immig1=sum_immig1,
                                          wfh_yes_sum_nonpermres1=sum_nonpermres1,
                                          wfh_yes_sum_nonimmig1=sum_nonimmig1)



immig_wfh_f <- merge(immig_wfh_yes,immig_all,by=c("geography", "health_region","naics_sector_name","sex"),all=T)

immig_wfh_f$percent_immig1 <- ifelse(immig_wfh_f$sum_immig1>0, immig_wfh_f$wfh_yes_sum_immig1 / immig_wfh_f$sum_immig1*100, NA)
immig_wfh_f$percent_nonpermres1 <- ifelse(immig_wfh_f$sum_nonpermres1>0, immig_wfh_f$wfh_yes_sum_nonpermres1 / immig_wfh_f$sum_nonpermres1*100, NA)
immig_wfh_f$percent_nonimmig1 <- ifelse(immig_wfh_f$sum_nonimmig1>0, immig_wfh_f$wfh_yes_sum_nonimmig1 / immig_wfh_f$sum_nonimmig1*100, NA)



immig_wfh_f <- immig_wfh_f %>% 
  select(geography, health_region,naics_sector_name,sex,percent_immig1,percent_nonpermres1,percent_nonimmig1) %>%
  mutate_if(is.numeric,round,0)

hr_immig_wfh_f <- immig_wfh_f %>% gather(selected_char,percent,
                                         percent_immig1,percent_nonpermres1,percent_nonimmig1)
# View(immig_wfh_f)

## physical prox
immig_phys_prox <- immig3 %>% group_by(geography, health_region,naics_sector_name,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         phys_prox_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex) %>%
  ungroup()

immig_phys_prox_allind <- immig3 %>% group_by(geography, health_region,sex,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         phys_prox_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex) %>%
  ungroup()
immig_phys_prox_allind$naics_sector_name <- c("All Industry Sectors")

immig_phys_prox <- rbind(immig_phys_prox,immig_phys_prox_allind)

immig_phys_prox_high <- immig_phys_prox %>% filter(phys_prox_high==1) %>% select(-phys_prox_high)

immig_phys_prox_high <- immig_phys_prox_high %>% rename(phys_prox_high_sum_immig1=sum_immig1,
                                                        phys_prox_high_sum_nonpermres1=sum_nonpermres1,
                                                        phys_prox_high_sum_nonimmig1=sum_nonimmig1)



immig_phys_prox_f <- merge(immig_phys_prox_high,immig_all,by=c("geography", "health_region","naics_sector_name","sex"),all=T)

immig_phys_prox_f$percent_immig1 <- ifelse(immig_phys_prox_f$sum_immig1>0, immig_phys_prox_f$phys_prox_high_sum_immig1 / immig_phys_prox_f$sum_immig1*100, NA)
immig_phys_prox_f$percent_nonpermres1 <- ifelse(immig_phys_prox_f$sum_nonpermres1>0, immig_phys_prox_f$phys_prox_high_sum_nonpermres1 / immig_phys_prox_f$sum_nonpermres1*100, NA)
immig_phys_prox_f$percent_nonimmig1 <- ifelse(immig_phys_prox_f$sum_nonimmig1>0, immig_phys_prox_f$phys_prox_high_sum_nonimmig1 / immig_phys_prox_f$sum_nonimmig1*100, NA)



immig_phys_prox_f <- immig_phys_prox_f %>% 
  select(geography, health_region,naics_sector_name,sex,percent_immig1,percent_nonpermres1,percent_nonimmig1) %>%
  mutate_if(is.numeric,round,0)

hr_immig_phys_prox_f <- immig_phys_prox_f %>% gather(selected_char,percent,
                                                     percent_immig1,percent_nonpermres1,percent_nonimmig1)

# exposure
immig_exposure <- immig3 %>% group_by(geography, health_region,naics_sector_name,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         exposure_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex) %>%
  ungroup()

immig_exposure_allind <- immig3 %>% group_by(geography, health_region,sex,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         exposure_high,
         sum_immig1,
         sum_nonpermres1,
         sum_nonimmig1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex) %>%
  ungroup()
immig_exposure_allind$naics_sector_name <- c("All Industry Sectors")

immig_exposure <- rbind(immig_exposure,immig_exposure_allind)

immig_exposure_high <- immig_exposure %>% filter(exposure_high==1) %>% select(-exposure_high)

immig_exposure_high <- immig_exposure_high %>% rename(exposure_high_sum_immig1=sum_immig1,
                                                      exposure_high_sum_nonpermres1=sum_nonpermres1,
                                                      exposure_high_sum_nonimmig1=sum_nonimmig1)



immig_exposure_f <- merge(immig_exposure_high,immig_all,by=c("geography", "health_region","naics_sector_name","sex"),all=T)

immig_exposure_f$percent_immig1 <- ifelse(immig_exposure_f$sum_immig1>0, immig_exposure_f$exposure_high_sum_immig1 / immig_exposure_f$sum_immig1*100, NA)
immig_exposure_f$percent_nonpermres1 <- ifelse(immig_exposure_f$sum_nonpermres1>0, immig_exposure_f$exposure_high_sum_nonpermres1 / immig_exposure_f$sum_nonpermres1*100, NA)
immig_exposure_f$percent_nonimmig1 <- ifelse(immig_exposure_f$sum_nonimmig1>0, immig_exposure_f$exposure_high_sum_nonimmig1 / immig_exposure_f$sum_nonimmig1*100, NA)



immig_exposure_f <- immig_exposure_f %>% 
  select(geography, health_region,naics_sector_name,sex,percent_immig1,percent_nonpermres1,percent_nonimmig1) %>%
  mutate_if(is.numeric,round,0)

hr_immig_exposure_f <- immig_exposure_f %>% gather(selected_char,percent,
                                                   percent_immig1,percent_nonpermres1,percent_nonimmig1)

save(hr_immig_wfh_f,hr_immig_phys_prox_f,hr_immig_exposure_f,file="hr_immig_chart.RData")




#### Age (Census Table 3)
#### 

age_group <- table3_r %>% filter(sex %in% c("Female","Male"))

age_group2 <- age_group %>%  select(geography, health_region,
                                    naics_sector_name,
                                    noc_code,sex,age,sum_total1)

# View(age_group2)

onet_merge <- onet %>% select(noc_code,wfh_yes,phys_prox,exposure)
onet_merge$phys_prox_high <- ifelse(onet_merge$phys_prox > 75, 1,0)
onet_merge$phys_prox_high <- ifelse(is.na(onet_merge$phys_prox), 0,onet_merge$phys_prox_high)

onet_merge$exposure_high <- ifelse(onet_merge$exposure >50, 1,0)
onet_merge$exposure_high <- ifelse(is.na(onet_merge$exposure),0,onet_merge$exposure_high)

onet_merge$wfh_yes <- ifelse(is.na(onet_merge$wfh_yes), 0,onet_merge$wfh_yes)

onet_merge <- onet_merge %>% select(noc_code,wfh_yes,phys_prox_high,exposure_high)

age_group3 <- merge(age_group2,onet_merge,by="noc_code",all.x=T)

age_group_all <- age_group3 %>% group_by(geography, health_region,naics_sector_name,sex,age) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex,age)

age_group_allind <- age_group3 %>% group_by(geography, health_region,sex,age) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex,age)
age_group_allind$naics_sector_name <- c("All Industry Sectors")

age_group_all <- rbind(age_group_all,age_group_allind)

age_group_wfh <- age_group3 %>% group_by(geography, health_region,naics_sector_name,sex,age,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         wfh_yes,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex,age) %>%
  ungroup()

age_group_wfh_allind <- age_group3 %>% group_by(geography, health_region,sex,age,wfh_yes) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         wfh_yes,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex,age) %>%
  ungroup()
age_group_wfh_allind$naics_sector_name <- c("All Industry Sectors")

age_group_wfh <- rbind(age_group_wfh,age_group_wfh_allind)

age_group_wfh_yes <- age_group_wfh %>% filter(wfh_yes==0) %>% select(-wfh_yes)

age_group_wfh_yes <- age_group_wfh_yes %>% rename(wfh_yes_sum_total1=sum_total1
)



age_group_wfh_f <- merge(age_group_wfh_yes,age_group_all,by=c("geography", "health_region","naics_sector_name","sex","age"),all=T)

age_group_wfh_f$percent_sum_total1 <- ifelse(age_group_wfh_f$sum_total1>0, age_group_wfh_f$wfh_yes_sum_total1 / age_group_wfh_f$sum_total1*100, NA)


hr_age_group_wfh_f <- age_group_wfh_f %>% 
  select(geography, health_region,naics_sector_name,sex,age,percent_sum_total1) %>%
  mutate_if(is.numeric,round,0) %>%
  rename(selected_char=age,
         percent=percent_sum_total1)


# View(age_group_wfh_f)

## physical prox
age_group_phys_prox <- age_group3 %>% group_by(geography, health_region,naics_sector_name,sex,age,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         phys_prox_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex,age) %>%
  ungroup()

age_group_phys_prox_allind <- age_group3 %>% group_by(geography, health_region,sex,age,phys_prox_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         phys_prox_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex,age) %>%
  ungroup()
age_group_phys_prox_allind$naics_sector_name <- c("All Industry Sectors")

age_group_phys_prox <- rbind(age_group_phys_prox,age_group_phys_prox_allind)

age_group_phys_prox_high <- age_group_phys_prox %>% filter(phys_prox_high==1) %>% select(-phys_prox_high)

age_group_phys_prox_high <- age_group_phys_prox_high %>% rename(phys_prox_high_sum_total1=sum_total1
)



age_group_phys_prox_f <- merge(age_group_phys_prox_high,age_group_all,by=c("geography", "health_region","naics_sector_name","sex","age"),all=T)

age_group_phys_prox_f$percent_sum_total1 <- ifelse(age_group_phys_prox_f$sum_total1>0, age_group_phys_prox_f$phys_prox_high_sum_total1 / age_group_phys_prox_f$sum_total1*100, NA)


hr_age_group_phys_prox_f <- age_group_phys_prox_f %>% 
  select(geography, health_region,naics_sector_name,sex,age,percent_sum_total1) %>%
  mutate_if(is.numeric,round,0) %>%
  rename(selected_char=age,
         percent=percent_sum_total1)


# View(age_group_phys_prox_f)

#exposure
age_group_exposure <- age_group3 %>% group_by(geography, health_region,naics_sector_name,sex,age,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         naics_sector_name,
         sex,
         exposure_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,naics_sector_name,sex,age) %>%
  ungroup()

age_group_exposure_allind <- age_group3 %>% group_by(geography, health_region,sex,age,exposure_high) %>%
  mutate_if(is.numeric,sum) %>%
  select(geography, health_region,
         sex,
         exposure_high,
         age,
         sum_total1) %>%
  distinct() %>% 
  arrange(geography, health_region,sex,age) %>%
  ungroup()
age_group_exposure_allind$naics_sector_name <- c("All Industry Sectors")

age_group_exposure <- rbind(age_group_exposure,age_group_exposure_allind)

age_group_exposure_high <- age_group_exposure %>% filter(exposure_high==1) %>% select(-exposure_high)

age_group_exposure_high <- age_group_exposure_high %>% rename(exposure_high_sum_total1=sum_total1
)



age_group_exposure_f <- merge(age_group_exposure_high,age_group_all,by=c("geography", "health_region","naics_sector_name","sex","age"),all=T)

age_group_exposure_f$percent_sum_total1 <- ifelse(age_group_exposure_f$sum_total1>0, age_group_exposure_f$exposure_high_sum_total1 / age_group_exposure_f$sum_total1*100, NA)


hr_age_group_exposure_f <- age_group_exposure_f %>% 
  select(geography, health_region,naics_sector_name,sex,age,percent_sum_total1) %>%
  mutate_if(is.numeric,round,0) %>%
  rename(selected_char=age,
         percent=percent_sum_total1)

save(hr_age_group_wfh_f,hr_age_group_phys_prox_f,hr_age_group_exposure_f,file="hr_age_group_chart.RData")





