#' Plot the correlation between mating/reproductive success and their correlates
#'
#' This function creates a plot of the correlations using the
#' package ggplot2.
#' 
#' @param x The name of the x covariate
#' @param y The name of the y covariate
#' @param xlab A title for the x-axis
#' @param ylab A title for the y-axis
#' @inheritParams plot_relatedness
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_correlations}}
#'
#' @examples
#' plot_correlation(data = males,
#'                  x = "Ter_map", y = "Mat_succ",
#'                  xlab = "x-axis title", ylab = "y-axis title")
#'
plot_correlation <- function(data, x, y, xlab = "x-axis title", ylab = "y-axis title", limits = NULL) {
  gg <- ggplot(data = data, aes(y = !!sym(y), x = !!sym(x), shape = Cohort)) +
    labs(y = ylab, x = xlab) +
    scale_y_continuous(limits = limits) +
    theme_classic() +
    scale_shape_manual(values = c(22, 24), name = "Cohort of males:") +
    geom_point(size = 10) +
    geom_text(aes(label = No)) +
    theme(plot.margin = unit(c(10, 4, 5, 1), "mm"), legend.position = "bottom", legend.box.margin = margin(5, 1, 1, 1, unit = "pt"))
  return(gg)
}
utils::globalVariables("Cohort")


#' Create the figure showing the correlation between mating/reproductive success and their correlates
#' 
#' This function creates the figure showing the correlations. It is a wrapper
#' around the function \code{\link{plot_correlation}}.
#'
#' @inheritParams figure_pca
#' @seealso \code{\link{plot_correlation}}
#' @export
#'
#' @examples
#' figure_correlations(data = males, savePDF = TRUE)
#' 
figure_correlations <- function(data, savePDF = FALSE) {
  limits <- c(0, 15)
  gg1 <- plot_correlation(data = data,
                          x = "Ter_map", y = "Mat_succ",
                          xlab = "x-axis title", ylab = "y-axis title",
                          limits = limits)
  gg2 <- plot_correlation(data = data,
                          x = "Ter_map", y = "Mat_succ",
                          xlab = "x-axis title", ylab = "y-axis title",
                          limits = limits)
  gg3 <- plot_correlation(data = data,
                          x = "Ter_map", y = "Mat_succ",
                          xlab = "x-axis title", ylab = "y-axis title",
                          limits = limits)
  gg4 <- plot_correlation(data = data,
                          x = "Ter_map", y = "Mat_succ",
                          xlab = "x-axis title", ylab = "y-axis title",
                          limits = limits)
  gg5 <- plot_correlation(data = data,
                          x = "Ter_map", y = "Mat_succ",
                          xlab = "x-axis title", ylab = "y-axis title",
                          limits = limits)
  gg6 <- plot_correlation(data = data,
                          x = "Ter_map", y = "Mat_succ",
                          xlab = "x-axis title", ylab = "y-axis title",
                          limits = limits)
  pannel <- cowplot::plot_grid(gg1, gg2, gg3, gg4, gg5, gg6,
                               nrow = 3,
                               labels = c("A. bla",
                                          "B. bla",
                                          "C. bla",
                                          "D. bla",
                                          "E. bla",
                                          "F. bla"),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  if (savePDF) {
    if (!dir.exists("./figures")) {
      dir.create("./figures")
    }
    cowplot::ggsave(filename = "./figures/figure_correlations.pdf",
                    plot = pannel,
                    width = 12*2,
                    height = 12*3,
                    units = "cm")
    message("figure_correlations.pdf created and stored in directory 'figures'!")
  }
  return(invisible(NULL))
}
