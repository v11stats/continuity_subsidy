# this function computes spell lengths, ignoring the first spell of the given
# period. It returns a dataframe with the adult_id and spell length for each
# family unit. 
get_spell_length <-function(list_num, list_name = "d_list"){
    mylist <- get(list_name)
    dat <- mylist[[list_num]]
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