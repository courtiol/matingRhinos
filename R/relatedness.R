#' Plot the distribution of relatedness
#'
#' This function creates a plot of the distribution of relatedness using the
#' package ggplot2.
#' 
#' @param relatedness 
#' @param ID 
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_relatedness}}
#'
#' @examples
#' plot_relatedness(relatedness = males$Related[males$Cohort == "C1"], ID = males$No[males$Cohort == "C1"])
#' 
plot_relatedness <- function(relatedness, ID, limits = c(-0.25, 0.50)) {
  gg <- ggplot(data = data.frame(y = relatedness, x = ID), aes(y = y, x = ID)) +
    geom_bar(stat = "identity", fill = "lightgrey", width = 0.5) +
    labs(y = "Mean relatedness to XXX") +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = limits) +
    theme_classic() +
    theme(plot.margin = unit(c(5, 2, 2, 2), "mm"))
  return(gg)
}


#' Create the figure showing the distribution of relatedness
#'
#' This function creates the figure showing the distribution of relatedness for
#' the two cohorts of males and for females. It is a wrapper around the function
#' \code{\link{plot_relatedness}}.
#' 
#' @inheritParams figure_pca
#' @seealso \code{\link{plot_relatedness}}
#' @export
#'
#' @examples
#' figure_relatedness(data = males)
#' 
figure_relatedness <- function(data, savePDF = FALSE) {
  gg1 <- plot_relatedness(relatedness = data$Related[males$Cohort == "C1"], ID = data$No[males$Cohort == "C1"])
  gg2 <- plot_relatedness(relatedness = data$Related[males$Cohort == "C2"], ID = data$No[males$Cohort == "C2"])
  pannel <- cowplot::plot_grid(gg1,
                               gg2,
                               nrow = 1,
                               labels = c("A. Males C1", "B. Males C2"),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  
  if (savePDF) {
    if (!dir.exists("./figures")) {
      dir.create("./figures")
    }
    cowplot::ggsave(filename = "./figures/figure_relatedness.pdf",
                    plot = pannel,
                    width = 12*2,
                    height = 15,
                    units = "cm")
    message("figure_relatedness.pdf created and stored in directory 'figures'!")
  }
}