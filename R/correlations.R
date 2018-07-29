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
#' plot_correlation(data = males, x = "Ter_map", y = "Mat_succ", xlab = "x-axis title", ylab = "y-axis title")
#'
plot_correlation <- function(data, x, y, xlab = "x-axis title", ylab = "y-axis title", limits = NULL) {
  gg <- ggplot(data = data, aes(y = rlang::eval_tidy(y), x = rlang::eval_tidy(x))) +
    labs(y = ylab, x = xlab) +
    scale_y_continuous(limits = limits) +
    theme_classic() +
    geom_point(shape = 1, size = 4) +
    theme(plot.margin = unit(c(10, 8, 2, 2), "mm"))
  return(gg)
}
