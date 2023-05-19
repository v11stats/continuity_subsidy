# R-script to read in excel files and create child arrangements
# 
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
#source("~/git/continuity_subsidy/get_spell_function.R")

x2015 <- d_list[[1]]
working <- x2015 %>% select(childid_secure, benemonth, providerdhsnum, hours, payment, fy)

### Spell creation 
#
#########################
# selects Primary providers for a child
# if there are more than one provider in a given 
# benemonth, the Primary is determined by Hours, then Payment

prov <- working %>% # distinct adult/benemonth table created
    group_by(childid_secure, benemonth) %>%
    select(providerdhsnum, hours, payment) %>%
    arrange(desc(hours), desc(payment)) %>%
    ungroup() %>%
    select(-hours, -payment) %>%
    mutate(num="Providers") %>%
    pivot_wider(names_from = num , values_from = providerdhsnum,
        values_fn = list )
prov$Providers <- sapply(prov$Providers,unique) #
# I had to use unique because since the providers are ordered by hours and payment,
# duplicates were showing up when aggregating via pivot_wider
prov$ProvNames <- sapply(prov$Providers, paste, collapse=",")
prov$NumProviders <- sapply(prov$Providers, length )
prov$primary <- sapply(prov$Providers, head,1)
prov$Providers <- NULL
prov$ProvNames <- NULL
#get a list of all the months in the data
months <- working %>% select(benemonth) %>% distinct() %>% arrange(benemonth)
prov <- prov %>% arrange(childid_secure, benemonth)
# get all the possible children
children <- working %>% select(childid_secure) %>% distinct() %>% filter(childid_secure !="")
# make a large db of all possible months for each child, along with the 
# primary provider for the month
temp2 <- cross_join(months, children)
temp2 <- left_join(temp2,prov)
temp2 <- as.data.table(temp2)
temp2$spelstop <- !is.na(temp2$primary)
temp2 <- temp2 %>% arrange(childid_secure) #you must sort by ChildID_Secure for this to work!!!
#the following line of code counts spell length, gets the first 
#beneMonth of each spell, and groups each summary by a unique group name (ID)
temp2 <- temp2 %>%
    group_by(childid_secure, ID = rleid(primary)) %>%  #advance value every time Signal changes and group by that
    mutate(arrange_length = cumsum(spelstop)) %>% #cumsum in each group
    ungroup() %>%#ungroup so you could remove the grouping column
    #select(-plag, -spell, -sp) %>%
    filter(arrange_length !=0)  
temp2 <- temp2 %>%
    group_by(childid_secure ,ID) %>%
    mutate(sMin = min(benemonth), 
           lcensor = (sMin==min(months))) %>% 
    filter(lcensor != T) %>%
    select(-spelstop, -sMin, -lcensor)
temp3 <- temp2 %>%
    group_by(childid_secure) %>%
    filter(ID == min(ID)) %>%
    summarise(max(arrange_length))

get_arrangement <- function(list_num, list_name = 'd_list'){
    mylist <- get(list_name)
    dat <- mylist[[list_num]]
    
    ### Spell creation 
    #
    #########################
    # selects Primary providers for a child
    # if there are more than one provider in a given 
    # benemonth, the Primary is determined by Hours, then Payment
    
    prov <- dat %>% # distinct adult/benemonth table created
        group_by(childid_secure, benemonth) %>%
        select(providerdhsnum, hours, payment) %>%
        arrange(desc(hours), desc(payment)) %>%
        ungroup() %>%
        select(-hours, -payment) %>%
        mutate(num="Providers") %>%
        pivot_wider(names_from = num , values_from = providerdhsnum,
                    values_fn = list )
    prov$Providers <- sapply(prov$Providers,unique) #
    # I had to use unique because since the providers are ordered by hours and payment,
    # duplicates were showing up when aggregating via pivot_wider
    prov$ProvNames <- sapply(prov$Providers, paste, collapse=",")
    prov$NumProviders <- sapply(prov$Providers, length )
    prov$primary <- sapply(prov$Providers, head,1)
    prov$Providers <- NULL
    prov$ProvNames <- NULL
    #get a list of all the months in the data
    months <- dat %>% select(benemonth) %>% distinct() %>% arrange(benemonth)
    prov <- prov %>% arrange(childid_secure, benemonth)
    # get all the possible children
    children <- dat %>% select(childid_secure) %>% distinct() %>% filter(childid_secure !="")
    # make a large db of all possible months for each child, along with the 
    # primary provider for the month
    temp2 <- cross_join(months, children)
    temp2 <- left_join(temp2,prov)
    temp2 <- as.data.table(temp2)
    temp2$spelstop <- !is.na(temp2$primary)
    temp2 <- temp2 %>% arrange(childid_secure) #you must sort by ChildID_Secure for this to work!!!
    #the following line of code counts spell length, gets the first 
    #beneMonth of each spell, and groups each summary by a unique group name (ID)
    temp2 <- temp2 %>%
        group_by(childid_secure, ID = rleid(primary)) %>%  #advance value every time Signal changes and group by that
        mutate(arrange_length = cumsum(spelstop)) %>% #cumsum in each group
        ungroup() %>%#ungroup so you could remove the grouping column
        #select(-plag, -spell, -sp) %>%
        filter(arrange_length !=0)  
    temp2 <- temp2 %>%
        group_by(childid_secure ,ID) %>%
        mutate(sMin = min(benemonth), 
               lcensor = (sMin==min(months))) %>% 
        filter(lcensor != T) %>%
        select(-spelstop, -sMin, -lcensor)
    temp3 <- temp2 %>%
        group_by(childid_secure) %>%
        filter(ID == min(ID)) %>%
        summarise(max(arrange_length))
    return(temp3)# return new db with proper arrangement logic
}

temp4 <- get_arrangement(1)


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


