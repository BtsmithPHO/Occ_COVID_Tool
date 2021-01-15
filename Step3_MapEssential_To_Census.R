####################
## Occupational Risk Covid19 Code Repository


#######################################
## STEP3 - merge essential services and append
#######################################

## load R datasets
load("alberta_r.RData")
load("ontario_r.RData")
load("britishcolumbia_r.RData")
load("quebec_r.RData")
load("novascotia_r.RData")
load("newfoundland_r.RData")
load("northwest_r.RData")
load("yukon_r.RData")
load("nunavut_r.RData")
load("saskatchewan_r.RData")
load("manitoba_r.RData")
load("newbrunswick_r.RData")
load("pei_r.RData")

load("essential_industries.RData")
load("nonessential_occupations.RData")
load("onet_naics_noc.RData")

alberta_r$geography <- c("Alberta")
ontario_r$geography <- c("Ontario")
britishcolumbia_r$geography <- c("British Columbia")
quebec_r$geography <- c("Quebec")
novascotia_r$geography <- c("Nova Scotia")
newfoundland_r$geography <- c("Newfoundland and Labrador")
northwest_r$geography <- c("Northwest Territories")
yukon_r$geography <- c("Yukon")
nunavut_r$geography <- c("Nunavut")
saskatchewan_r$geography <- c("Saskatchewan")
manitoba_r$geography <- c("Manitoba")
newbrunswick_r$geography <- c("New Brunswick")
pei_r$geography <- c("Prince Edward Island")


# Call in libraries 
library(dplyr)
library(scales)
library(tidyverse)
##############################
## alberta essential services
###############################
alberta_r$essential_ind <- ifelse (alberta_r$naics_code %in% alberta_ess_ind, 1,0)
alberta_r$essential_occ <- ifelse(alberta_r$noc_code %in% alberta_noness_occ,0,1)
alberta_r$essential <- ifelse(alberta_r$essential_occ ==0, 0,alberta_r$essential_ind)

alberta_r<- alberta_r[!names(alberta_r) %in% c("essential_ind","essential_occ")]

##################################
## ontario essential services
###################################
ontario_r$essential_ind <- ifelse (ontario_r$naics_code %in% ontario_ess_ind, 1,0)	
ontario_r$essential_occ <- ifelse(ontario_r$noc_code %in% ontario_noness_occ,0,1)
ontario_r$essential <- ifelse(ontario_r$essential_occ ==0, 0,ontario_r$essential_ind)

ontario_r<- ontario_r[!names(ontario_r) %in% c("essential_ind","essential_occ")]

########################
## bc essential services
########################
britishcolumbia_r$essential_ind <- ifelse (britishcolumbia_r$naics_code %in% britishcolumbia_ess_ind,1,0)
britishcolumbia_r$essential_occ <- ifelse(britishcolumbia_r$noc_code %in% britishcolumbia_noness_occ,0,1)
britishcolumbia_r$essential <- ifelse(britishcolumbia_r$essential_occ ==0, 0,britishcolumbia_r$essential_ind)

britishcolumbia_r<- britishcolumbia_r[!names(britishcolumbia_r) %in% c("essential_ind","essential_occ")]

##############################
## quebec essential services
##############################
quebec_r$essential_ind <- ifelse (quebec_r$naics_code %in% quebec_ess_ind,1,0)
quebec_r$essential_occ <- ifelse(quebec_r$noc_code %in% quebec_noness_occ,0,1)
quebec_r$essential <- ifelse(quebec_r$essential_occ ==0, 0,quebec_r$essential_ind)

quebec_r<- quebec_r[!names(quebec_r) %in% c("essential_ind","essential_occ")]

##############################
## novascotia essential services
##############################
novascotia_r$essential_ind <- ifelse (novascotia_r$naics_code %in% novascotia_ess_ind,1,0)
novascotia_r$essential_occ <- ifelse (novascotia_r$noc_code %in% novascotia_noness_occ,0,1)
novascotia_r$essential <- ifelse(novascotia_r$essential_occ ==0, 0,novascotia_r$essential_ind)

novascotia_r<- novascotia_r[!names(novascotia_r) %in% c("essential_ind","essential_occ")]

##############################
## newfoundland essential services
##############################
newfoundland_r$essential_ind <- ifelse (newfoundland_r$naics_code %in% newfoundland_ess_ind,1,0)
newfoundland_r$essential_occ <- ifelse (newfoundland_r$noc_code %in% newfoundland_noness_occ,0,1)
newfoundland_r$essential <- ifelse(newfoundland_r$essential_occ ==0, 0,newfoundland_r$essential_ind)

newfoundland_r<- newfoundland_r[!names(newfoundland_r) %in% c("essential_ind","essential_occ")]

##############################
## northwest essential services
##############################
northwest_r$essential_ind <- ifelse (northwest_r$naics_code %in% northwest_ess_ind,1,0)
northwest_r$essential_occ <- ifelse (northwest_r$noc_code %in% northwest_noness_occ,0,1)
northwest_r$essential <- ifelse(northwest_r$essential_occ ==0, 0,northwest_r$essential_ind)

northwest_r<- northwest_r[!names(northwest_r) %in% c("essential_ind","essential_occ")]

##############################
## yukon essential services
##############################
yukon_r$essential_ind <- ifelse (yukon_r$naics_code %in% yukon_ess_ind,1,0)
yukon_r$essential_occ <- ifelse (yukon_r$noc_code %in% yukon_noness_occ,0,1)
yukon_r$essential <- ifelse(yukon_r$essential_occ ==0, 0,yukon_r$essential_ind)

yukon_r<- yukon_r[!names(yukon_r) %in% c("essential_ind","essential_occ")]

##############################
## nunavut essential services
##############################
nunavut_r$essential_ind <- ifelse (nunavut_r$naics_code %in% nunavut_ess_ind,1,0)
nunavut_r$essential_occ <- ifelse (nunavut_r$noc_code %in% nunavut_noness_occ,0,1)
nunavut_r$essential <- ifelse(nunavut_r$essential_occ ==0, 0,nunavut_r$essential_ind)

nunavut_r<- nunavut_r[!names(nunavut_r) %in% c("essential_ind","essential_occ")]

##############################
## manitoba essential services
##############################
manitoba_r$essential_ind <- ifelse (manitoba_r$naics_code %in% manitoba_ess_ind,1,0)
manitoba_r$essential_occ <- ifelse (manitoba_r$noc_code %in% manitoba_noness_occ,0,1)
manitoba_r$essential <- ifelse(manitoba_r$essential_occ ==0, 0,manitoba_r$essential_ind)

manitoba_r<- manitoba_r[!names(manitoba_r) %in% c("essential_ind","essential_occ")]

##############################
## saskatchewan essential services
##############################
saskatchewan_r$essential_ind <- ifelse (saskatchewan_r$naics_code %in% saskatchewan_ess_ind,1,0)
saskatchewan_r$essential_occ <- ifelse (saskatchewan_r$noc_code %in% saskatchewan_noness_occ, 0,1)
saskatchewan_r$essential <- ifelse(saskatchewan_r$essential_occ ==0, 0,saskatchewan_r$essential_ind)

saskatchewan_r<- saskatchewan_r[!names(saskatchewan_r) %in% c("essential_ind","essential_occ")]

##############################
## newbrunswick essential services
##############################
newbrunswick_r$essential_ind <- ifelse (newbrunswick_r$naics_code %in% newbrunswick_ess_ind,1,0)
newbrunswick_r$essential_occ <- ifelse (newbrunswick_r$noc_code %in% newbrunswick_noness_occ,0,1)
newbrunswick_r$essential <- ifelse(newbrunswick_r$essential_occ ==0, 0,newbrunswick_r$essential_ind)

newbrunswick_r<- newbrunswick_r[!names(newbrunswick_r) %in% c("essential_ind","essential_occ")]

##############################
## pei essential services
##############################
pei_r$essential_ind <- ifelse (pei_r$naics_code %in% pei_ess_ind,1,0)
pei_r$essential_occ <- ifelse (pei_r$noc_code %in% pei_noness_occ,0,1)
pei_r$essential <- ifelse(pei_r$essential_occ ==0, 0,pei_r$essential_ind)

pei_r<- pei_r[!names(pei_r) %in% c("essential_ind","essential_occ")]


###############################
## append all the provinces
################################
alberta_r <- alberta_r[,!grepl("^median",names(alberta_r))]
ontario_r <- ontario_r[,!grepl("^median",names(ontario_r))]
britishcolumbia_r <- britishcolumbia_r[,!grepl("^median",names(britishcolumbia_r))]
quebec_r <- quebec_r[,!grepl("^median",names(quebec_r))]
novascotia_r <- novascotia_r[,!grepl("^median",names(novascotia_r))]
newfoundland_r <- newfoundland_r[,!grepl("^median",names(newfoundland_r))]
northwest_r <- northwest_r[,!grepl("^median",names(northwest_r))]
yukon_r <- yukon_r[,!grepl("^median",names(yukon_r))]
nunavut_r<- nunavut_r[,!grepl("^median",names(nunavut_r))]
manitoba_r <- manitoba_r[,!grepl("^median",names(manitoba_r))]
saskatchewan_r <- saskatchewan_r[,!grepl("^median",names(saskatchewan_r))]
newbrunswick_r <- newbrunswick_r[,!grepl("^median",names(newbrunswick_r))]
pei_r <- pei_r[,!grepl("^median",names(pei_r))]

table1_r <- rbind(alberta_r,ontario_r,britishcolumbia_r,quebec_r,novascotia_r,newfoundland_r,northwest_r,yukon_r,nunavut_r,manitoba_r,saskatchewan_r,newbrunswick_r,pei_r)
rm(alberta_r,ontario_r,britishcolumbia_r,quebec_r,novascotia_r,newfoundland_r,northwest_r,yukon_r,nunavut_r,manitoba_r,saskatchewan_r,newbrunswick_r,pei_r)


NAICS_MERGE_SECTOR <- NAICS_MERGE %>% select(naics_sector,naics_sector_name) %>%distinct()
table1_r <- table1_r %>% select(-naics_sector)
table1_r <- merge(table1_r,NAICS_MERGE_SECTOR,by="naics_sector_name") %>% distinct()
# View(table1_r)

table1_r$noc_code <- as.character(table1_r$noc_code)
table1_r$naics_code <- as.character(table1_r$naics_code)
table1_r$noc_broad <- as.character(table1_r$noc_broad)
table1_r$naics_sector <- as.character(table1_r$naics_sector)
table1_r$noc_code_class <- as.character(table1_r$noc_code_class)

table1_r$noc_code <- as.numeric(table1_r$noc_code)
table1_r$noc_code<-formatC(table1_r$noc_code, width = 4, format = "d", flag = "0")
table1_r$noc_code<-as.character(table1_r$noc_code)


save(table1_r,file="table1_r.RData")
save(table1_r,file="table1_r.RData")

table1_r_ontario <- table1_r %>% filter(geography=="Ontario")
save(table1_r_ontario,file="table1_r_ontario.RData")

View(table1_r)

##### Census table 2


rm(list=ls())
# Call in libraries 
library(dplyr)
library(scales)
library(tidyverse)
library(readxl)
load("table2_r.RData")
load("table2_medinc_final_r.RData")
load("essential_industries.RData")
load("nonessential_occupations.RData")
load("onet_naics_noc.RData")

##############################
## alberta essential services
###############################
if(table2_r$geography=="Alberta"){
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% alberta_ess_ind, 1,0)
  table2_r$essential_occ <- ifelse(table2_r$noc_code %in% alberta_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Ontario"){
  ##################################
  ## ontario essential services
  ###################################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% ontario_ess_ind, 1,0)	
  table2_r$essential_occ <- ifelse(table2_r$noc_code %in% ontario_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="British Columbia"){
  ########################
  ## bc essential services
  ########################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% britishcolumbia_ess_ind,1,0)
  table2_r$essential_occ <- ifelse(table2_r$noc_code %in% britishcolumbia_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Quebec"){
  ##############################
  ## quebec essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% quebec_ess_ind,1,0)
  table2_r$essential_occ <- ifelse(table2_r$noc_code %in% quebec_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Manitoba"){
  
  ##############################
  ## manitoba essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% manitoba_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% manitoba_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Saskatchewan"){
  ##############################
  ## saskatchewan essential services
  ##############################
  
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% saskatchewan_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% saskatchewan_noness_occ, 0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="New Brunswick"){
  ##############################
  ## newbrunswick essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% newbrunswick_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% newbrunswick_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Prince Edward Island"){
  ##############################
  ## pei essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% pei_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% pei_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Nova Scotia"){
  
  ##############################
  ## novascotia essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% novascotia_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% novascotia_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Newfoundland and Labrador"){
  ##############################
  ## newfoundland essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% newfoundland_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% newfoundland_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Northwest Territories"){
  ##############################
  ## northwest essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% northwest_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% northwest_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Yukon"){
  ##############################
  ## yukon essential services
  ##############################
  
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% yukon_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% yukon_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
} else if(table2_r$geography=="Nunavut"){
  ##############################
  ## nunavut essential services
  ##############################
  table2_r$essential_ind <- ifelse (table2_r$naics_code %in% nunavut_ess_ind,1,0)
  table2_r$essential_occ <- ifelse (table2_r$noc_code %in% nunavut_noness_occ,0,1)
  table2_r$essential <- ifelse(table2_r$essential_occ ==0, 0,table2_r$essential_ind)
  
  table2_r<- table2_r[!names(table2_r) %in% c("essential_ind","essential_occ")]
}

NAICS_MERGE_SECTOR <- NAICS_MERGE %>% select(naics_sector,naics_sector_name) %>%distinct()
table2_r <- table2_r %>% select(-naics_sector)
table2_r <- merge(table2_r,NAICS_MERGE_SECTOR,by="naics_sector_name") %>% distinct()
# View(table2_r)

table2_r$noc_code <- as.numeric(table2_r$noc_code)
table2_r$noc_code<-formatC(table2_r$noc_code, width = 4, format = "d", flag = "0")
table2_r$noc_code<-as.character(table2_r$noc_code)

table2_r_ontario <- table2_r %>% filter(geography=='Ontario')
save(table2_r_ontario,file="table2_r_ontario.RData")
save(table2_r, file="table2_r.RData")
save(table2_r,file="table2_r.RData")



#############################
## Census table 3 Health Regions
###############################
##############################
rm(list=ls())

load("table3_on_part1_r.RData")
load("table3_on_part2_r.RData")
load("table3_on_part3_r.RData")
load("table3_on_part4_r.RData")
load("table3_on_part5_r.RData")
load("table3_on_part6_r.RData")
load("table3_on_part7_r.RData")
load("table3_on_part8_r.RData")
load("table3_on_part9_r.RData")
ontario_healthregions <- rbind(table3_on_part1_r,table3_on_part2_r,table3_on_part3_r,table3_on_part4_r,table3_on_part5_r,
                               table3_on_part6_r,table3_on_part7_r,table3_on_part8_r,table3_on_part9_r)
save(ontario_healthregions,file="ontario_healthregions.RData")

load("table3_bc_part1_r.RData")
load("table3_bc_part2_r.RData")
load("table3_bc_part3_r.RData")
load("table3_bc_part4_r.RData")
load("table3_bc_part5_r.RData")
britishcolumbia_healthregions <- rbind(table3_bc_part1_r,table3_bc_part2_r,table3_bc_part3_r,table3_bc_part4_r,table3_bc_part5_r)
save(britishcolumbia_healthregions,file="britishcolumbia_healthregions.RData")

load("table3_ab_part1_r.RData")
load("table3_ab_part2_r.RData")
alberta_healthregions <- rbind(table3_ab_part1_r,table3_ab_part2_r)
save(alberta_healthregions,file="alberta_healthregions.RData")


load("table3_qb_part1_r.RData")
load("table3_qb_part2_r.RData")
load("table3_qb_part3_r.RData")
load("table3_qb_part4_r.RData")
load("table3_qb_part5_r.RData")
quebec_healthregions <- rbind(table3_qb_part1_r,table3_qb_part2_r,table3_qb_part3_r,table3_qb_part4_r,table3_qb_part5_r)
save(quebec_healthregions,file="quebec_healthregions.RData")

load("table3_sk_part1_r.RData")
load("table3_sk_part2_r.RData")
load("table3_sk_part3_r.RData")
load("table3_sk_part4_r.RData")
saskatchewan_healthregions <- rbind(table3_sk_part1_r,table3_sk_part2_r,table3_sk_part3_r,table3_sk_part4_r)
save(saskatchewan_healthregions,file="saskatchewan_healthregions.RData")

load("table3_mb_part1_r.RData")
load("table3_mb_part2_r.RData")
manitoba_healthregions <- rbind(table3_mb_part1_r,table3_mb_part2_r)
save(manitoba_healthregions,file="manitoba_healthregions.RData")

load("table3_nb_part1_r.RData")
load("table3_nb_part2_r.RData")
newbrunswick_healthregions <- rbind(table3_nb_part1_r,table3_nb_part2_r)
save(newbrunswick_healthregions,file="manitoba_healthregions.RData")

load("table3_ns_part1_r.RData")
load("table3_ns_part2_r.RData")
novascotia_healthregions <- rbind(table3_ns_part1_r,table3_ns_part2_r)
save(novascotia_healthregions,file="novascotia_healthregions.RData")

load("table3_nl_part1_r.RData")
load("table3_nl_part2_r.RData")
newfoundland_healthregions <- rbind(table3_nl_part1_r,table3_nl_part2_r)
save(newfoundland_healthregions,file="newfoundland_healthregions.RData")

load("table3_pei_part1_r.RData")
pei_healthregions <- table3_pei_part1_r
save(pei_healthregions,file="pei_healthregions.RData")

load("table3_yt_part1_r.RData")
yukon_healthregions <- table3_yt_part1_r
save(yukon_healthregions,file="yukon_healthregions.RData")

load("table3_nt_part1_r.RData")
northwest_healthregions <- table3_nt_part1_r
save(northwest_healthregions,file="northwest_healthregions.RData")

load("table3_nu_part1_r.RData")
nunavut_healthregions <- table3_nu_part1_r
save(nunavut_healthregions,file="nunavut_healthregions.RData")

rm(list=ls())
# Call in libraries 
library(dplyr)
library(scales)
library(tidyverse)

load("ontario_healthregions.RData")
load("alberta_healthregions.RData")
load("britishcolumbia_healthregions.RData")
load("quebec_healthregions.RData")
load("newfoundland_healthregions.RData")
load("newbrunswick_healthregions.RData")
load("novascotia_healthregions.RData")
load("pei_healthregions.RData")
load("saskatchewan_healthregions.RData")
load("manitoba_healthregions.RData")
load("nunavut_healthregions.RData")
load("northwest_healthregions.RData")
load("yukon_healthregions.RData")

ontario_healthregions$geography <- c("Ontario")
alberta_healthregions$geography <- c("Alberta")
britishcolumbia_healthregions$geography <- c("British Columbia")
quebec_healthregions$geography <- c("Quebec")
newfoundland_healthregions$geography <- c("Newfoundland and Labrador")
newbrunswick_healthregions$geography <- c("New Brunswick")
novascotia_healthregions$geography <- c("Nova Scotia")
pei_healthregions$geography <- c("Prince Edward Island")
saskatchewan_healthregions$geography <- c("Saskatchewan")
manitoba_healthregions$geography <- c("Manitoba")
nunavut_healthregions$geography <- c("Nunavut")
northwest_healthregions$geography <- c("Northwest Territories")
yukon_healthregions$geography <- c("Yukon")


table3_r <- rbind(ontario_healthregions, alberta_healthregions,britishcolumbia_healthregions,quebec_healthregions,
                  newfoundland_healthregions,newbrunswick_healthregions,novascotia_healthregions,pei_healthregions,
                  saskatchewan_healthregions,manitoba_healthregions,nunavut_healthregions,northwest_healthregions,
                  yukon_healthregions)

# table3_r_regions<- table3_r %>% distinct(health_region)
# library(rio)
# export(table3_r_regions,"table3_r_regions2.xlsx")
# View(table3_r_regions)
library(readxl)
health_regions_match <- read_excel("//oto101pfile01v/Christine.Warren$/Research Projects/Covid19_Occupation_Project/Census Data/health_regions_match.xlsx")
# summary(health_regions_match)
# summary(table3_r)

table3_r_test <- merge(table3_r,health_regions_match,by="health_region",all.x=TRUE) 

table3_r <- table3_r_test%>% filter(!is.na(table3_r_test$health_regions_new)) %>% select(-health_region,-naics_sector) %>% rename("health_region"="health_regions_new")
# View(table3_r)


load("essential_industries.RData")
load("nonessential_occupations.RData")
load("onet_naics_noc.RData")

##################################
## essential services
###################################
if (table3_r$geography == "Ontario"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% ontario_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% ontario_noness_occ,0,1)
} else if (table3_r$geography == "Alberta"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% alberta_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% alberta_noness_occ,0,1)
}else if (table3_r$geography == "British Columbia"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% britishcolumbia_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% britishcolumbia_noness_occ,0,1)
}else if (table3_r$geography == "Quebec"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% quebec_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% quebec_noness_occ,0,1)
}else if (table3_r$geography == "Newfoundland and Labrador"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% newfoundland_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% newfoundland_noness_occ,0,1)
}else if (table3_r$geography == "New Brunswick"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% newbrunswick_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% newbrunswick_noness_occ,0,1)
}else if (table3_r$geography == "Nova Scotia"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% novascotia_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% novascotia_noness_occ,0,1)
}else if (table3_r$geography == "Prince Edward Island"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% pei_ess_ind, 1,0)	
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% pei_noness_occ,0,1)
}else if (table3_r$geography == "Saskatchewan"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% saskatchewan_ess_ind, 1,0)
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% saskatchewan_noness_occ,0,1)
}else if (table3_r$geography == "Manitoba"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% manitoba_ess_ind, 1,0)
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% manitoba_noness_occ,0,1)
}else if (table3_r$geography == "Nunavut"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% nunavut_ess_ind, 1,0)
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% nunavut_noness_occ,0,1)
}else if (table3_r$geography == "Northwest Territories"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% northwest_ess_ind, 1,0)
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% northwest_noness_occ,0,1)
}else if (table3_r$geography == "Yukon"){
  table3_r$essential_ind <- ifelse (table3_r$naics_code %in% yukon_ess_ind, 1,0)
  table3_r$essential_occ <- ifelse(table3_r$noc_code %in% yukon_noness_occ,0,1)
}
# we ignore the 1 for essential occupation, we only care about non-ess
# View(table3_r)
table3_r$essential <- ifelse(table3_r$essential_occ ==0, 0,table3_r$essential_ind)
table3_r<- table3_r[!names(table3_r) %in% c("essential_ind","essential_occ")]

table3_r <- table3_r[,!grepl("^median",names(table3_r))]


NAICS_MERGE_SECTOR <- NAICS_MERGE %>% select(naics_sector,naics_sector_name) %>%distinct()
table3_r <- table3_r %>% select(-naics_sector)
table3_r <- merge(table3_r,NAICS_MERGE_SECTOR,by="naics_sector_name") %>% distinct()
# View(table3_r)

table3_r$noc_code <- as.character(table3_r$noc_code)
table3_r$naics_code <- as.character(table3_r$naics_code)
table3_r$noc_broad <- as.character(table3_r$noc_broad)
table3_r$naics_sector <- as.character(table3_r$naics_sector)

table3_r$noc_code <- as.numeric(table3_r$noc_code)
table3_r$noc_code<-formatC(table3_r$noc_code, width = 4, format = "d", flag = "0")
table3_r$noc_code<-as.character(table3_r$noc_code)

save(table3_r,file="table3_r.RData")
save(table3_r,file="table3_r.RData")

