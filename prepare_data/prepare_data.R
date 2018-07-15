library(dplyr)
rm(list = ls())

## preparing data
males <- read.csv("./prepare_data/males.csv")
males %>% group_by(Mat_succ, Rep_succ, Cohort) %>% summarize(Count = n()) %>% as.data.frame -> males_agg
males
males_agg

females <- read.csv("./prepare_data/females.csv")
females %>% group_by(Mat_succ, Rep_succ, Cohort) %>% summarize(Count = n()) %>% as.data.frame -> females_agg
females
females_agg

save(males, file = "males.rda", compress = "xz")
save(females, file = "females.rda", compress = "xz")

save(males_agg, file = "males_agg.rda", compress = "xz")
save(females_agg, file = "females_agg.rda", compress = "xz")