# R-script to read in excel files and create child arrangements
# R version 4.3.0 
# Using tidyverse 2.0.0, datatable 1.14.8, readxl 1.4.2
library(readxl); library(tidyverse); library(data.table)
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

durations <- list()
#get durations
for(i in seq_along(biannual_arrangements)) {
    durations[[i]] <- get_arrangement(i,'biannual_arrangements')
    summ <-summary(durations[[i]])
    print(paste0("Summary for years: ",yrs[i]," to ", yrs[i+1]))
    print(summ)
}
# build report
report <- tibble(year=NA,mean=NA,median=NA,max=NA,min=NA, N_of_children=NA, sd=NA)
for(i in seq_along(biannual_arrangements)) {
    temp <- durations[[i]][[2]] 
    report[i,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
    report[i,2] <- mean(temp)
    report[i,3] <- median(temp)
    report[i,4] <- max(temp)
    report[i,5] <- min(temp)
    report[i,6] <- length(temp)
    report[i,7] <- sd(temp)
}
report
save(report, file = 'biannual_arrangements.rDATA')


