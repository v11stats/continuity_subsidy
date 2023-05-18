# rscript to read in excel files and create family spells
library(readxl); library(tidyverse); library(data.table)
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
x2015 <- d_list[[1]]
x2016 <- d_list[[2]]
x2017 <- d_list[[3]]
x2018 <- d_list[[4]]
x2019 <- d_list[[5]]


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

# this function computes spell lengths, ignoring the first spell of the given
# period. It returns a dataframe with the adult_id and spell length for each
# family unit. 
get_spell_length <-function(list_num){
    dat <- d_list[[list_num]]
    xmin <- min(dat$benemonth)
    dat$spelstop <- !is.na(dat$fy)
    dat <- dat %>% arrange(adultid_secure)# must always do this step!
    dat <- dat %>%
        group_by(adultid_secure, ID = rleid(spelstop)) %>%
        mutate(sp_length = cumsum(spelstop)) %>%
        ungroup() %>%
        filter(sp_length !=0)
    # get lcensored observations and remove them
    # r censored not important here
    dat <- dat %>%
        group_by(adultid_secure, ID) %>%
        mutate(sMin = min(benemonth),
               lcensor = (sMin == xmin)) %>%
        filter(lcensor !=T) %>%
        select(-spelstop,-sMin, -lcensor)
    x <- dat %>%
        group_by(adultid_secure) %>%
        filter(ID == min(ID)) %>%
        summarise(max(sp_length))
    return(x)
    
        
}

x3 <- get_spell_length(5)

# data pairings. groups data into two consecutive years:
biannual_spells <- list()
yrs <- str_sub(d_names,2,5)
for(i in 1:(length(yrs)-1)){
   biannual_spells[[i]] <- bind_rows(d_list[[i]],d_list[[i+1]] )
}


