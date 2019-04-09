data(d_sim)
## COLIN: PUT ALL ABOVE IN RDA AND STEPS IN prepare_data.R
d <- d_sim
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


#################################################################################

figure_relatedness_simulation <- function(d_sim){
  col1 <- 'black'
  col2 <- 'red'
if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
  col1 <- 'black'
  col2 <- 'grey'
}

lines <- data.frame(y = c(0.5, 0.5, 0.25, 1/8, 1/32, 0), 
                    yend = c(0.5, 0.5, 0.25, 1/8, 1/32, 0), 
                    x = c(1:6-0.4), 
                    xend = c(1:6+0.4))


out <- ggplot(d_sim, aes(x = cat, y = DyadML)) +
  geom_boxplot(width = 0.8, col = col1) +
  scale_x_discrete("Relationship") +
  geom_segment(data = lines, aes(y = y, yend = yend, xend = xend, x = x), col = "red", linetype = 2) +
  theme_classic() +
  theme(plot.margin = unit(c(10, 4, 5, 1), 'mm'),
        legend.position = 'bottom',
        legend.box.margin = margin(5, 1, 1, 1, unit = 'pt'),
        text = element_text(size = 16))

if (!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
  if (!dir.exists('./figures')) {
    dir.create('./figures')
  }
  ggsave(filename = './figures/figureS5_relatedness_simulation.pdf',
         plot = out,
         width = 11.5*2,
         height = 13.5,
         units = 'cm')
  message("figureS5_relatedness_simulation.pdf created and stored in directory 'figures'!")
}
out
return(invisible(NULL))
}

figure_relatedness_simulation(d_sim)
###############################################################################




# wilcox.test(d$DyadML[d$cat == "PO"], mu = 0.5)
# wilcox.test(d$DyadML[d$cat == "FS"], mu = 0.5)
# wilcox.test(d$DyadML[d$cat == "HS"], mu = 0.25)
# wilcox.test(d$DyadML[d$cat == "FC"], mu = 1/8)
# wilcox.test(d$DyadML[d$cat == "SC"], mu = 1/32)
# wilcox.test(d$DyadML[d$cat == "U"], mu = 0)
