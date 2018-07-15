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

save(males, file = "./data/males.rda", compress = "xz")
save(females, file = "./data/females.rda", compress = "xz")

save(males_agg, file = "./data/males_agg.rda", compress = "xz")
save(females_agg, file = "./data/females_agg.rda", compress = "xz")