rm(list = ls())

## 1. Checking what is the most suitable metric for relatedness
d <- read.csv("Result_all_rel3.csv")
head(d)
str(d)

d$Pop <- unlist(lapply(strsplit(as.character(d$Indiv1), split = ""), function(i) paste(i[1:4], collapse = "")))
d <- d[order(d$Pop), ]
d$real <- rep(c(0.5, 0.5, 0.25, 1/8, 1/32, 0), each = 100)
d$cat <- as.factor(rep(c("PO", "FS", "HS", "FC", "SC", "U"), each = 100))
d$cat <- reorder(d$cat, rev(d$real))

res <- c(TriolML = sum((d$TrioMl - d$real)^2),
  Wang = sum((d$Wang - d$real)^2),
  LynchLi =  sum((d$LynchLi - d$real)^2),
  LynchRd = sum((d$LynchRd - d$real)^2),
  Ritland = sum((d$Ritland - d$real)^2),
  Queller = sum((d$Queller - d$real)^2),
  DyadML = sum((d$DyadML - d$real)^2))

sort(res)

res2 <- c(TriolML = cor(d$TrioMl, d$real),
         Wang     = cor(d$Wang, d$real),
         LynchLi  = cor(d$LynchLi, d$real),
         LynchRd  = cor(d$LynchRd, d$real),
         Ritland  = cor(d$Ritland, d$real),
         Queller  = cor(d$Queller, d$real),
         DyadML   = cor(d$DyadML, d$real))

sort(res2, decreasing = TRUE)

boxplot(DyadML ~ cat, data = d)  ### COLIN: PLEASE MAKE THIS NICE
abline(h = c(0.5, 0.5, 0.25, 1/8, 1/32, 0), lty = 3)

wilcox.test(d$DyadML[d$cat == "PO"], mu = 0.5)
wilcox.test(d$DyadML[d$cat == "FS"], mu = 0.5)
wilcox.test(d$DyadML[d$cat == "HS"], mu = 0.25)
wilcox.test(d$DyadML[d$cat == "FC"], mu = 1/8)
wilcox.test(d$DyadML[d$cat == "SC"], mu = 1/32)
wilcox.test(d$DyadML[d$cat == "U"], mu = 0)
