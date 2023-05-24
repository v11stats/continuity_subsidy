# Rscript to read in excel files and create family spells
library(readxl); library(tidyverse); library(data.table); library(survival)
library(broom)
################################################################################
#
# Add any additional year of data to the below section. Just link the excel file
# and use the naming convention x + year to name the file. The code will loop 
# through the datasets and produce the report: 'biannual_spells.rDATA' for each
# available year, which can then be added to the shiny app.
#
################################################################################
setwd("C:/Users/mwohn/Box/OSUdata5yrs/From Robi Feb 22, 2022")
#step1 import dataset
x2015 <- read_excel('2015_OSU_Update_202202.xlsx')
x2016 <- read_excel('2016_OSU_Update_202202.xlsx')
x2017 <- read_excel('2017_OSU_Update_202202.xlsx')
x2018 <- read_excel('2018_OSU_Update_202202.xlsx')
x2019 <- read_excel('2019_OSU_Update_202202.xlsx')
# collect all the datasets and only get the columns I want
f_names <- ls(pattern = "x20[0-9][0-9]+")# names of datasets
f_list <- mget(f_names)
for(i in seq_along(f_list)) {
    names(f_list[[i]]) =
        tolower(names(f_list[[i]]))
}
for(i in seq_along(f_list)) {
    f_list[[i]] <- f_list[[i]] %>% select(adultid_secure, benemonth, fy) %>%
        distinct()
        }
rm(list=ls(pattern = "x20[0-9][0-9]+"))
gc()
f_names
setwd("~/git/continuity_subsidy")
# needed
# this file is required to run this script, it contains a bespoke function
# for calculating arrangements
source("~/git/continuity_subsidy/spell_arrangement_function.R")
# get all possible months for each year all_months
# and all providers in each year, all_providers
# make a larger dataset of all possible provider month combinations, all_possible
# any month that a family has no billing will be NA
for(i in seq_along(f_list)) {
    all_months <- f_list[[i]] %>% select(benemonth) %>%
        distinct() %>% arrange(benemonth)
    all_providers <- f_list[[i]] %>% select(adultid_secure) %>%
         distinct() %>% arrange(adultid_secure)
    all_possible <- cross_join(all_months,all_providers)
    f_list[[i]] <- left_join(all_possible,f_list[[i]])
}
# data pairings. groups data into two consecutive years:
biannual_spells <- list()
yrs <- str_sub(f_names,2,5)
for(i in 1:(length(yrs)-1)){
    biannual_spells[[i]] <- bind_rows(f_list[[i]],f_list[[i+1]] )
}


durations <- list()
#get durations
for(i in seq_along(biannual_spells)) {
    durations[[i]] <- get_spell_length(i,'biannual_spells')
   
}
# build report
report <- tibble(Year=NA,Median=NA,LCL=NA,UCL=NA, N_of_families=NA, Events=NA)
for(i in seq_along(durations)) {
    temp <- durations[[i]] 
    report[i,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
    #create surv object
    tempSurv <- survfit(Surv(temp$sp_length,temp$rcensor)~1)
    tempQuants <- quantile(tempSurv,probs = .5)
    report[i,2] <- tempQuants$quantile
    report[i,3] <- tempQuants$lower
    report[i,4] <- tempQuants$upper
    report[i,5] <- tempSurv$n
    report[i,6] <- sum(tempSurv$n.event)
}
report
save(report, file = 'biannual_spells.rDATA')
# risk_tables
# if we want to save the entire risk table, run this:
for(i in seq_along(durations)) {
    temp <- durations[[i]] 
    tempSurv <- tidy(survfit(Surv(temp$sp_length,temp$rcensor)~1))
    temp2<-  paste0("risk_tables/familyRiskTable",yrs[i],"_", yrs[i+1],".rDATA")
    save(tempSurv, file = temp2)
}
#
# we can also save the entire survival object
for(i in seq_along(durations)) {
    temp <- durations[[i]] 
    tempSurv <- survfit(Surv(temp$sp_length,temp$rcensor)~1)
    temp2<-  paste0("survival_objects/familyRisk_obj",yrs[i],"_", yrs[i+1],".rDATA")
    save(tempSurv, file = temp2)
    
}