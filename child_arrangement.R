# R-script to read in excel files and create child arrangements
# R version 4.3.0 
# Using tidyverse 2.0.0, datatable 1.14.8, readxl 1.4.2
library(readxl); library(tidyverse); library(data.table); library(broom)
library(survival)
################################################################################
#
# Add any additional year of data to the below section. Just link the excel file
# and use the naming convention x + year to name the file. The code will loop 
# through the datasets and produce the c_report: 'biannual_spells.rDATA' for each
# available year, which can then be added to the shiny app.
#
################################################################################

# **** Important **** this script will only work if the below files contain the 
# column names in line 37, i.e. childid_secure, benemonth, providerdhsnum, 
# hours, payment, fy. They can 
# be upper or lowercase.

setwd("C:/Users/mwohn/Box/OSUdata5yrs/From Robi Feb 22, 2022")
#step1 import dataset
x2015 <- read_excel('2015_OSU_Update_202202.xlsx')
x2016 <- read_excel('2016_OSU_Update_202202.xlsx')
x2017 <- read_excel('2017_OSU_Update_202202.xlsx')
x2018 <- read_excel('2018_OSU_Update_202202.xlsx')
x2019 <- read_excel('2019_OSU_Update_202202.xlsx')
# collect all the datasets and only get the columns I want
d_names <- ls(pattern = "x20[0-9][0-9]+")# names of datasets
d_list <- mget(d_names)
for(i in seq_along(d_list)) {
    names(d_list[[i]]) =
        tolower(names(d_list[[i]]))
}
# reduce dataset to only the needed columns
for(i in seq_along(d_list)) {
    d_list[[i]] <- d_list[[i]] %>% 
        select(childid_secure, benemonth, providerdhsnum, 
               hours, payment, fy) %>%
        distinct()
}
rm(list=ls(pattern = "x20[0-9][0-9]+"))
gc()
# data pairings. groups data into two consecutive years:
# used to get a larger db of 2 year observations
biannual_arrangements <- list()
yrs <- str_sub(d_names,2,5)
for(i in 1:(length(yrs)-1)){
    biannual_arrangements[[i]] <- bind_rows(d_list[[i]],d_list[[i+1]] )
}
setwd("~/git/continuity_subsidy")
# this file is required to run this script, it contains a bespoke function
# for calculating arrangements
source("~/git/continuity_subsidy/spell_arrangement_function.R")

c_durations <- list()
#get c_durations
for(i in seq_along(biannual_arrangements)) {
    c_durations[[i]] <- get_arrangement(i,'biannual_arrangements')
}
# build c_report
# build c_report
c_report <- tibble(Year=NA,Median=NA,LCL=NA,UCL=NA, N_of_children=NA, Events=NA)
for(i in seq_along(c_durations)) {
    temp <- c_durations[[i]] 
    c_report[i,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
    #create surv object
    tempSurv <- survfit(Surv(temp$arrange_length,temp$rcensor)~1)
    tempQuants <- quantile(tempSurv,probs = .5)
    c_report[i,2] <- tempQuants$quantile
    c_report[i,3] <- tempQuants$lower
    c_report[i,4] <- tempQuants$upper
    c_report[i,5] <- tempSurv$n
    c_report[i,6] <- sum(tempSurv$n.event)
}
c_report
save(c_report, file = 'biannual_arrangements.rDATA')

# if we want to save the entire risk table, we can do this:
for(i in seq_along(c_durations)) {
    temp <- c_durations[[i]] 
    tempSurv <- tidy(survfit(Surv(temp$arrange_length,temp$rcensor)~1))
    temp2<-  paste0("risk_tables/childRiskTable",yrs[i],"_", yrs[i+1],".rDATA")
    save(tempSurv, file = temp2)
}

# we can also save the entire survival object
for(i in seq_along(c_durations)) {
    temp <- c_durations[[i]] 
    tempSurv <- survfit(Surv(temp$arrange_length,temp$rcensor)~1)
    temp2<-  paste0("survival_objects/childRisk_obj",yrs[i],"_", yrs[i+1],".rDATA")
    save(tempSurv, file = temp2)
    
}
