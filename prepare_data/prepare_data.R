library(dplyr)
rm(list = ls())

## preparing data
males_temp <- read.csv("./prepare_data/males.csv")
males_temp$No <- factor(males_temp$No, levels = c("A", "G", "K", "R", "S", "123", "5", "30", "60", "62", "63", "65", "66"))

males_temp %>% arrange(Cohort, No) -> males

males %>% group_by(Mat_succ, Rep_succ, Cohort) %>% summarize(Count = n()) %>% as.data.frame -> males_agg
males
males_agg

females <- read.csv("./prepare_data/females.csv")
females %>% group_by(Mat_succ, Rep_succ, Cohort) %>% summarize(Count = n()) %>% as.data.frame -> females_agg
females
females_agg

save(males, file = "./data/males.rda", compress = "xz")
save(females, file = "./data/females.rda", compress = "xz")

males_agg$Sex <- "males"
females_agg$Sex <- "females"

rhinos_agg <- bind_rows(males_agg, females_agg)
rhinos_agg$Sex <- factor(rhinos_agg$Sex)
str(rhinos_agg)

save(rhinos_agg, file = "./data/rhinos_agg.rda", compress = "xz")
