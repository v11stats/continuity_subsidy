# Rscript to read in excel files and create family spells
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
for(i in seq_along(d_list)) {
    d_list[[i]] <- d_list[[i]] %>% select(adultid_secure, benemonth, fy) %>%
        distinct()
        }
rm(list=ls(pattern = "x20[0-9][0-9]+"))
gc()
d_names
setwd("~/git/continuity_subsidy")
source("~/git/continuity_subsidy/get_spell_function.R")
# get all possible months for each year all_months
# and all providers in each year, all_providers
# make a larger dataset of all possible provider month combinations, all_possible
# any month that a family has no billing will be NA
for(i in seq_along(d_list)) {
    all_months <- d_list[[i]] %>% select(benemonth) %>%
        distinct() %>% arrange(benemonth)
    all_providers <- d_list[[i]] %>% select(adultid_secure) %>%
         distinct() %>% arrange(adultid_secure)
    all_possible <- cross_join(all_months,all_providers)
    d_list[[i]] <- left_join(all_possible,d_list[[i]])
}
# data pairings. groups data into two consecutive years:
biannual_spells <- list()
yrs <- str_sub(d_names,2,5)
for(i in 1:(length(yrs)-1)){
    biannual_spells[[i]] <- bind_rows(d_list[[i]],d_list[[i+1]] )
}


durations <- list()
#get durations
for(i in seq_along(biannual_spells)) {
    durations[[i]] <- get_spell_length(i,'biannual_spells')
    summ <-summary(durations[[i]])
    print(paste0("Summary for years: ",yrs[i]," to ", yrs[i+1]))
    print(summ)
}
# build report
report <- tibble(year=NA,mean=NA,median=NA,max=NA,min=NA, length=NA)
for(i in seq_along(biannual_spells)) {
    temp <- durations[[i]][[2]] 
    report[i,1] <- paste0("Years: ",yrs[i]," - ", yrs[i+1])
    report[i,2] <- mean(temp)
    report[i,3] <- median(temp)
    report[i,4] <- max(temp)
    report[i,5] <- min(temp)
    report[i,6] <- length(temp)
}

save(report, file = 'biannual_spells.rDATA')


