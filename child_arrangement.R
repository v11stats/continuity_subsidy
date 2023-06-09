# R-script to read in excel files and create child arrangements
# R version 4.3.0 
# Using tidyverse 2.0.0, datatable 1.14.8, readxl 1.4.2
library(readxl); library(tidyverse); library(data.table); library(broom)
library(survival);library(janitor)
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
               hours, payment, fy, factype,
               starts_with('race'),ethnicity, relative) %>%
        select(-raceethnicity) %>%
        distinct()
}
rm(list=ls(pattern = "x20[0-9][0-9]+"))
gc()
# compute TOC types for each child and fix racial coding
for(i in seq_along(d_list)) {
    temp <- d_list[[i]]
    temp$toc <- temp %>%
        mutate (
            toc = 
                case_when(
                    relative == "N" & factype =="QFM" ~ "Exempt Nonrelative",
                    relative == "N" & factype =="FAM" ~ "Exempt Nonrelative",
                    relative == "Y" & factype =="QFM" ~ "Exempt Relative",
                    relative == "Y" & factype =="FAM" ~ "Exempt Relative",
                    factype =="QEC" | factype =="NQC" ~ "Exempt Center",
                    factype =="CFM" ~ "Certified Family",
                    factype =="CNT" ~ "Certified Center",
                    factype =="RFM" ~ "Registered Family",
                )
        ) %>% pull(toc)
# deprecated code - using case_when and across instead
    #    temp <-temp %>% #convert "Y" and "N" to 1 and 0
#        mutate_at(c('racea','raceb','racep','racew','racei','ethnicity'),
#                  ~recode(.,'N'=0,'Y'=1,'0'=0,'1'=1))
    temp <-temp %>% # this re-codes Y and N to 0 and 1
        mutate(
            across(
                c(starts_with('race'),ethnicity),
                ~ case_when(
                    .=="N" | .=="0" ~ 0,
                    .=="Y" | .=="1" ~ 1
                )
            )
        )
    d_list[[i]] <- temp
}

# data pairings. groups data into two consecutive years:
# used to get a larger db of 2 year observations
biannual_arrangements <- list()
yrs <- str_sub(d_names,2,5)
for(i in 1:(length(yrs)-1)){
    biannual_arrangements[[i]] <- bind_rows(d_list[[i]],d_list[[i+1]] )
}
setwd("~/git/continuity_subsidy")
# this file is required to run this script, it contains a function
# for calculating arrangements
source("~/git/continuity_subsidy/spell_arrangement_function.R")

c_durations <- list()
#get c_durations
for(i in seq_along(biannual_arrangements)) {
    c_durations[[i]] <- get_arrangement(i,'biannual_arrangements')
}
# build c_report unstratified curves
c_report <- tibble(Year=NA,Median=NA,LCL=NA,UCL=NA, N_of_children=NA, Events=NA)
for(i in seq_along(c_durations)) {
    temp <- c_durations[[i]] 
    #only take the last benemonth as this is for a unstratified KM survival curve
    temp <- temp %>%
        group_by(childid_secure) %>%
        arrange(benemonth) %>%
        slice_tail(n=1)
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
rm(list=ls(pattern = '^temp'))
fwrite(c_report, file = "reports/biannual_arrangements.csv", col.names = T)
#save(c_report, file = 'biannual_arrangements.rDATA') can also be save as an r-file

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
# Stratified races
race_report <- tibble(Year=NA,race=NA,Median=NA,LCL=NA,UCL=NA, N_of_children=NA, Events=NA)
tempRow <- 0
for(i in seq_along(c_durations)) {
    temp <- c_durations[[i]]
    tempNam <- tibble(name =names(temp),rw = 1:length(names(temp)))
    tempNam <- data.table(tempNam)
    tempNam <- tempNam[name %like% "race"]
    for(j in 1:nrow(tempNam)){
        tempSurv <- survfit(Surv(arrange_length,rcensor)~get(tempNam$name[j]) ,data=temp)
        tempQuants <- quantile(tempSurv,probs = .5)
        race_report[j+tempRow,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
        race_report[j+tempRow,2] <- tempNam$name[j]
        race_report[j+tempRow,3] <- tempQuants$quantile[2]
        race_report[j+tempRow,4] <- tempQuants$lower[2]
        race_report[j+tempRow,5] <- tempQuants$upper[2]
        race_report[j+tempRow,6] <- tempSurv$n[2]
        race_report[j+tempRow,7] <- sum(tempSurv$n.event[2])
    }
    tempRow <- tempRow + 5
}
race_report
rm(list=ls(pattern = '^temp'))
fwrite(race_report, file = "reports/stratified_races_biannual.csv", col.names = T)
# Stratified Ethnicities
e_report <- tibble(Year=NA,hisp=NA,Median=NA,LCL=NA,UCL=NA, N_of_children=NA, Events=NA)
tempRow <- 1
for(i in seq_along(c_durations)) {
    temp <- c_durations[[i]] 
    
    tempSurv <- survfit(Surv(temp$arrange_length,temp$rcensor)~temp$ethnicity)
    tempQuants <- quantile(tempSurv,probs = .5)
    
    e_report[tempRow,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
    e_report[tempRow,2] <- "Not Hispanic"
    e_report[tempRow,3] <- tempQuants$quantile[1]
    e_report[tempRow,4] <- tempQuants$lower[1]
    e_report[tempRow,5] <- tempQuants$upper[1]
    e_report[tempRow,6] <- tempSurv$n[1]
    e_report[tempRow,7] <- sum(tempSurv$n.event[1])
    e_report[tempRow+1,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
    e_report[tempRow+1,2] <- "Hispanic"
    e_report[tempRow+1,3] <- tempQuants$quantile[2]
    e_report[tempRow+1,4] <- tempQuants$lower[2]
    e_report[tempRow+1,5] <- tempQuants$upper[2]
    e_report[tempRow+1,6] <- tempSurv$n[2]
    e_report[tempRow+1,7] <- sum(tempSurv$n.event[2])
    tempRow <- tempRow + 2
}
e_report
rm(list=ls(pattern = '^temp'))
fwrite(e_report, file = "reports/stratified_ethnicities_biannual.csv", col.names = T)

#Stratified TOC types
t_report <- tibble(Year=NA,TypeOfCare=NA,Median=NA,LCL=NA,UCL=NA, N_of_children=NA, Events=NA)
tempRow <- 0
for(i in seq_along(c_durations)) {
    temp <- c_durations[[i]]
    tempSurv <- survfit(Surv(arrange_length,rcensor)~toc ,data=temp)
    tempNam <- names(tempSurv$strata)
    x <-tidy(tempSurv)
    tempQuants <- quantile(tempSurv,probs = .5)
    for(j in 1:length(tempNam)){
        t_report[j+tempRow,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
        t_report[j+tempRow,2] <- tempNam[j]
        t_report[j+tempRow,3] <- tempQuants$quantile[j]
        t_report[j+tempRow,4] <- tempQuants$lower[j]
        t_report[j+tempRow,5] <- tempQuants$upper[j]
        t_report[j+tempRow,6] <- tempSurv$n[j]
        t_report[j+tempRow,7] <- sum(tempSurv$n.event[which(x$strata==tempNam[j])])
    }
    tempRow <- tempRow + 6
}
t_report
rm(list=ls(pattern = '^temp'))
fwrite(t_report, file = "reports/stratified_toc_child_biannual.csv", col.names = T)

