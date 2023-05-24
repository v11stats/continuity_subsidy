#test output for dashboard
library(ggplot2)
report
report %>%
    ggplot(aes(Year, Median))+
    geom_point()+
    ggtitle("Continuity of Child Care Arrangements")
c_report
c_report %>%
    ggplot(aes(Year, Median))+
    geom_point()+
    ggtitle("Continuity of Subsidy Program")
