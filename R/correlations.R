#' Compute a Spearman correlation test between two variables
#'
#' This function computes the Spearman correlation between two variables. It is
#' a simple wrapper around the R function \code{\link{cor.test}}, with better
#' display of the output which includes the outcome of the Bonferroni correction.
#' 
#' @param var1 A vector.
#' @param var2 A vector.
#' @param n_tests An integer providing the number of tests to account for during 
#' the Bonferroni adjustment. 
#' @inheritParams test_NonacsB
#'
#' @return A vector containing the correlation between two variables, the 
#' p-value of the correlation test, the expect value (i.e. the p-value 
#' multiplied by n_tests) and the sample size after discarding the 
#' missing values.
#' @export
#' 
#' @examples
#' compute_correlation(var1 = males$Mat_succ, var2 = males$Rep_succ, n_tests = 3)
#' 
compute_correlation <- function(var1, var2, n_tests = 1L, digits = 3L) {
  d <- stats::na.omit(data.frame(var1 = var1, var2 = var2))
  corr <- stats::cor.test(~ var1 + var2, data = d, method = "spearman")
  out <- c(rho = corr$estimate[[1]],
           p = corr$p.value[[1]],
           n_obs = nrow(d),
           E = corr$p.value[[1]] * n_tests,
           n_tests = n_tests)
  out_txt <- prettyNum(out, digits = digits)
  if (out['p'] < 0.001) out_txt['p'] <- '<0.001'
  if (out['E'] < 0.001) out_txt['E'] <- '<0.001'
  if (out['E'] > 1) out_txt['E'] <- '>1'
  print(out_txt, quote = FALSE)
  return(invisible(out))
}


#' Plot the correlation between mating/reproductive success and their correlates
#'
#' This function creates a plot of the correlations using the
#' package ggplot2.
#' 
#' @param x The name of the x covariate.
#' @param y The name of the y covariate.
#' @param xlab A title for the x-axis.
#' @param ylab A title for the y-axis.
#' @inheritParams plot_relatedness
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_correlations}}
#'
#' @examples
#' plot_correlation(data = males,
#'                  x = 'Ter_map', y = 'Mat_succ',
#'                  xlab = 'x-axis title', ylab = 'y-axis title')
#'
plot_correlation <- function(data, x, y, xlab = 'x-axis title', ylab = 'y-axis title', limits = NULL) {
  gg <- ggplot(data = data, aes(y = !!sym(y), x = !!sym(x), shape = Cohort)) +
    labs(y = ylab, x = xlab) +
    scale_y_continuous(limits = limits, breaks = function(x) seq(0, x[2], by = 2L)) +
    theme_classic() +
    scale_shape_manual(values = c(22, 24), name = 'Cohort of males:') +
    geom_point(size = 10) +
    geom_text(aes(label = No)) +
    theme(plot.margin = unit(c(10, 4, 5, 1), 'mm'), legend.position = 'bottom', legend.box.margin = margin(5, 1, 1, 1, unit = 'pt'))
  return(gg)
}
utils::globalVariables('Cohort')


#' Create the figures showing the correlation between mating/reproductive success and their correlates
#' 
#' This function creates the figures showing the correlations. It is a
#' wrapper around the function \code{\link{plot_correlation}}. The function
#' can either illustrate the correlations with the mating success or those with 
#' the reproductive success depending on the value of the argument \code{which}.
#'
#' @inheritParams figure_pca
#' @param which 'mating' or 'repro', to indicate which plot to draw.
#' @seealso \code{\link{plot_correlation}}
#' @export
#'
#' @examples
#' figure_correlations(data = males, which = 'mating')
#' figure_correlations(data = males, which = 'repro')
#' 
figure_correlations <- function(data, which = c('mating', 'repro'), savePDF = FALSE) {
  if (length(which) == 2) {
    Recall(data = data, which = which[1], savePDF = savePDF)
    Recall(data = data, which = which[2], savePDF = savePDF)
    return(invisible(NULL))
  }
  if (length(which) != 1L || !which %in% c('mating', 'repro')) {
    stop('argument which not as expected!')
  }
  if (which == 'mating') {
    y <- 'Mat_succ'
    ylab <- 'Number of mates'
    basename_fig <- 'figure3_correlations_mating'
    limits <- c(0L, 12L)
  }
  if (which == 'repro') {
    y <- 'Rep_succ'
    ylab <- 'Number of offspring'
    basename_fig <- 'figure4_correlations_repro'
    limits <- c(0L, 18L)
  }
  gg1 <- plot_correlation(data = data,
                          x = 'Related_mean',
                          y = y,
                          xlab = 'Mean relatedness',
                          ylab = ylab,
                          limits = limits)
  gg2 <- plot_correlation(data = data,
                          x = 'Ter_map',
                          y = y,
                          xlab = expression(paste('Territory size (km'^2,')')),
                          ylab = ylab,
                          limits = limits)
  gg3 <- plot_correlation(data = data,
                          x = 'Ter_map',
                          y = y,
                          xlab = 'x-axis title',
                          ylab = ylab,
                          limits = limits)
  gg4 <- plot_correlation(data = data,
                          x = 'Ter_map',
                          y = y,
                          xlab = 'x-axis title',
                          ylab = ylab,
                          limits = limits)
  gg5 <- plot_correlation(data = data,
                          x = 'Ter_map',
                          y = y,
                          xlab = 'x-axis title',
                          ylab = ylab,
                          limits = limits)
  gg6 <- plot_correlation(data = data,
                          x = 'Ter_map',
                          y = y,
                          xlab = 'x-axis title',
                          ylab = ylab,
                          limits = limits)
  pannel <- cowplot::plot_grid(gg1, gg2, gg3, gg4, gg5, gg6,
                               nrow = 3,
                               labels = c('A.',
                                          'B.',
                                          'C.',
                                          'D.',
                                          'E.',
                                          'F.'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  if (savePDF) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    cowplot::ggsave(filename = paste0('./figures/', basename_fig, '.pdf'),
                    plot = pannel,
                    width = 12*2,
                    height = 12*3,
                    units = 'cm')
    message(paste0(basename_fig, '.pdf ',  "created and stored in directory 'figures'!"))
  }
  return(invisible(NULL))
}
