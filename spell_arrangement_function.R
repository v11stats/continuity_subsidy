# this function computes spell lengths, ignoring the first spell of the given
# period. It returns a dataframe with the adult_id and spell length for each
# family unit. 
get_spell_length <-function(list_num, list_name = "f_list"){
    mylist <- get(list_name)
    dat <- mylist[[list_num]]
    xmin <- min(dat$benemonth) #min month for the period
    xmax <- max(dat$benemonth) #max month for the period
    dat$spelstop <- !is.na(dat$fy)
    dat <- dat %>% arrange(adultid_secure)# must always do this step!
    dat <- dat %>%
        group_by(adultid_secure, ID = rleid(spelstop)) %>%
        mutate(sp_length = cumsum(spelstop)) %>%
        ungroup() %>%
        filter(sp_length !=0)
    # get lcensored observations and remove them
    dat <- dat %>%
        group_by(adultid_secure, ID) %>%
        mutate(sMin = min(benemonth),#get the min 
               sMax = max(benemonth),#get the max month for the family
               lcensor = (sMin == xmin),
               rcensor = as.numeric(sMax != xmax)) %>%
        filter(lcensor !=T) %>%
        select(-spelstop,-sMin, -lcensor,-ID,-sMax,-fy)
    #only take the last benemonth as this is for a unstratified KM survival curve
    dat <- dat %>%group_by(adultid_secure) %>% 
         arrange(benemonth) %>% slice_tail(n=1) 
    return(dat)     
}
get_arrangement <- function(list_num, list_name = 'd_list'){
    mylist <- get(list_name)
    dat <- mylist[[list_num]]
    
    
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
               lcensor = (sMin==min(months)),
               rcensor = as.numeric(sMin!=max(months))) %>% 
        filter(lcensor != T) %>%
        select(-spelstop, -sMin, -lcensor)
    #only take the last benemonth as this is for a unstratified KM survival curve
    temp2 <- temp2 %>%
        group_by(childid_secure) %>%
        arrange(benemonth) %>%
        slice_tail(n=1)
        
    return(temp2)# return new db with proper arrangement logic
}
