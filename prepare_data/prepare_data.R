library(dplyr)
rm(list = ls())

## preparing data
males_temp <- read.csv("./prepare_data/males.csv")
males_temp$No <- factor(males_temp$No, levels = c("A", "G", "K", "R", "S", "123", "5", "30", "62", "63", "65", "66"))

males_temp %>% arrange(Cohort, No) %>% filter(!is.na(No)) -> males

males %>% group_by(Mat_succ, Rep_succ, Cohort) %>% summarize(Count = n()) %>% as.data.frame %>% na.omit() -> males_agg
males
males_agg

females <- read.csv("./prepare_data/females.csv")
females %>% group_by(Mat_succ, Rep_succ, Cohort) %>% summarize(Count = n()) %>% as.data.frame %>% na.omit() -> females_agg
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


################################################################################
## simulation data used for relatedness

rm(list = ls())

## 1. Checking what is the most suitable metric for relatedness
d_sim <- read.csv("./prepare_data/results_simu_metric_relatedness.csv")

d_sim$Pop <- unlist(lapply(strsplit(as.character(d_sim$Indiv1), split = ""), function(i) paste(i[1:4], collapse = "")))
d_sim <- d_sim[order(d_sim$Pop), ]
d_sim$real <- rep(c(0.5, 0.5, 0.25, 1/8, 1/32, 0), each = 100)
d_sim$cat <- as.factor(rep(c("PO", "FS", "HS", "FC", "SC", "U"), each = 100))
d_sim$cat <- reorder(d_sim$cat, rev(d_sim$real))

save(d_sim, file = "./data/d_sim.rda", compress = "xz")

