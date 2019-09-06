#' Compute a Spearman correlation test between two variables
#'
#' This function computes the Spearman correlation between two variables. It is
#' a simple wrapper around the R function \code{\link{cor.test}}, with better
#' display of the output.
#' 
#' @note We set the argument \code{exact} to \var{FALSE} in the call to \code{\link{cor.test}},
#' so that the presence of ties in some correlation tests would not change
#' how the p-values are computed (exact tests are not possible in the presence of ties for this implementation of
#' the test). Otherwise p-values obtained can sometimes be inconsistent across tests (ex: smaller p-values despite higher rho and same sample
#' size.)
#' 
#' @param var1 A vector.
#' @param var2 A vector.
#' @inheritParams test_NonacsB
#'
#' @return A vector containing the correlation between two variables, the 
#' p-value of the correlation test, and the sample size after discarding the 
#' missing values.
#' @export
#' 
#' @seealso \code{\link{compute_correlation_table}}
#' 
#' @examples
#' compute_correlation(var1 = males$Mat_succ, var2 = males$Rep_succ)
#' 
compute_correlation <- function(var1, var2) {
  d <- stats::na.omit(data.frame(var1 = var1, var2 = var2))
  corr <- stats::cor.test(~ var1 + var2, data = d, method = "spearman", exact = FALSE)
  out <- c(rho = signif(corr$estimate[[1]], 2L),
           p = .pretty_p(corr$p.value[[1]], prefix = FALSE),
           n_obs = nrow(d))
  print(out, quote = FALSE)
  return(invisible(out))
}


#' Compute a Spearman correlation tests for a given cohort and fitness measurement
#'
#' This function computes the Spearman correlation tests between a fitness
#' component and all predictors. It is a simple wrapper around the R function
#' \code{\link{cor.test}}, with better display of the output that includes
#' correction for multiple testing (default = 'bonferroni').
#' 
#' @param cohort The cohort of males ('C1' or 'C2').
#' @param fitness The column name for the fitness component ('Mat_succ' or 'Rep_succ').
#' @param method The method for the multiple testing correction, see \code{\link{p.adjust}} or ?dunn.test::p.adjustment.methods for more details (requires to install the pacakge dunn.test).
#' @param data The dataset.
#'
#' @return A data.frame containing the predictor, the sample size after
#'   discarding the missing values, the correlation between two variables, the
#'   p-value of the correlation test, the p-value after correction for
#'   multiple testing, the number of tests, the E-value (p-value times number of 
#'   tests), and significance stars for the raw and adjusted p-values.
#'   
#' @note We set the argument \code{exact} to \var{FALSE} in the call to \code{\link{cor.test}},
#'   so that the presence of ties in some correlation tests would not change
#'   how the p-values are computed (exact tests are not possible in the presence of ties for this implementation of
#'   the test). Otherwise p-values obtained can sometimes be inconsistent across tests (ex: smaller p-values despite higher rho and same sample
#'   size.)
#'   
#' @export
#' 
#' @examples
#' compute_correlation_table(cohort = 'C1', fitness = 'Mat_succ', method = 'bonferroni', data = males)
#' 
compute_correlation_table <- function(cohort = NULL, fitness = NULL, method = 'bonferroni', data = NULL) {
  predictors <- c('Horn', 'Testo_mean', 'Ter_map', 'Open', 'Me_open', 'Me_thick', 'Thick', 'Pmax', 'Related_mean')
  fitness_var <- data[data$Cohort == cohort, fitness]
  list_corrs <- lapply(predictors, function(var) {
    if (is.null(data[data$Cohort == cohort, var])) return(c(n_obs = 0, rho = NA, p = NA))
    d <- stats::na.omit(data.frame(fitness_var = fitness_var, predictor = data[data$Cohort == cohort, var]))
    corr <- (stats::cor.test(~ fitness_var + predictor, data = d,
                                             method = "spearman", exact = FALSE))
    data.frame(n_obs = nrow(d), rho = .pretty_ps(corr$estimate[[1]], raw = TRUE), p = corr$p.value[[1]])
    })
  out <- data.frame(predictors, do.call('rbind', list_corrs))
  out$`p*` <- .pretty_star(out$p)
  out$p_adj <- stats::p.adjust(out$p, method = method)
  out$`p_adj*` <- .pretty_star(out$p_adj)
  out$n_tests <- length(out$p[!is.na(out$p)])
  out$E <- out$p*out$n_tests
  out$p <- .pretty_ps(out$p, prefix = FALSE)
  out$p_adj <- .pretty_ps(out$p_adj, prefix = FALSE)
  out$E <- .pretty_ps(out$E, prefix = FALSE)
  return(out)
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
#' @param y_limits The limit for the y-axis of the plot.
#' @param x_limits The limit for the x-axis of the plot.
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
plot_correlation <- function(data, x, y, xlab = 'x-axis title', ylab = 'y-axis title', y_limits = NULL, x_limits = NULL) {
  col1 <- 'red'
  col2 <- 'blue'
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col1 <- 'black'
    col2 <- 'black'
  }
  gg <- ggplot(data = data, aes(y = !!sym(y), x = !!sym(x), shape = Cohort, col = Cohort)) +
    labs(y = ylab, x = xlab) +
    scale_y_continuous(limits = y_limits, breaks = function(x) seq(0, x[2], by = 2L)) +
    scale_x_continuous(limits = x_limits) +
    theme_classic() +
    scale_shape_manual(values = c(22, 24), name = 'Cohort of males:') +
    scale_colour_manual(values = c(col1, col2), name = 'Cohort of males:') +
    geom_point(size = 9) +
    geom_text(aes(label = No), size = 2) +
    theme(plot.margin = unit(c(10, 10, 4, 1), 'mm'),
          legend.position = 'none',
          text = element_text(size = 16))
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
#' @inheritParams figure_PCA
#' @param which The figure to be drawn ('mating' or 'repro').
#' @seealso \code{\link{plot_correlation}}
#' @export
#'
#' @examples
#' figure_correlations(data = males, which = 'mating')
#' figure_correlations(data = males, which = 'repro')
#' 
figure_correlations <- function(data, which = c('mating', 'repro')) {
  if (length(which) == 2) {
    Recall(data = data, which = which[1])
    Recall(data = data, which = which[2])
    return(invisible(NULL))
  }
  if (length(which) != 1L || !which %in% c('mating', 'repro')) {
    stop('argument which not as expected!')
  }
  if (which == 'mating') {
    y <- 'Mat_succ'
    ylab <- 'Number of mates'
    basename_fig <- 'figure4_correlations_mating'
    limits <- c(0L, 14L)
  }
  if (which == 'repro') {
    y <- 'Rep_succ'
    ylab <- 'Number of offspring'
    basename_fig <- 'figure5_correlations_repro'
    limits <- c(0L, 20L)
  }
  
  ## compute PCA:
  malesC1 <- droplevels(data[data$Cohort == 'C1', ])
  malesC2 <- droplevels(data[data$Cohort == 'C2', ])
  PCA_C1_males <- compute_PCA(data = malesC1)
  PCA_C2_males <- compute_PCA(data = malesC2)
  data <- rbind(PCA_C1_males$data, PCA_C2_males$data)
  
  ## plots:
  gg1 <- plot_correlation(data = data,
                          x = 'Related_mean',
                          y = y,
                          xlab = 'Mean relatedness',
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(0, 0.20))
  gg2 <- plot_correlation(data = data,
                          x = 'Ter_map',
                          y = y,
                          xlab = expression(paste('Territory size (km'^2,')')),
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(0, 80))
  gg3 <- plot_correlation(data = data,
                          x = 'Open',
                          y = y,
                          xlab = 'Occurence of grassland (%)',
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(0, 25))
  gg4 <- plot_correlation(data = data,
                          x = 'Me_open',
                          y = y,
                          xlab = 'Occurence of open woodland (%)',
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(0, 100))
  gg5 <- plot_correlation(data = data,
                          x = 'Me_thick',
                          y = y,
                          xlab = 'Occurence of close woodland (%)',
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(0, 60))
  gg6 <- plot_correlation(data = data,
                          x = 'Thick',
                          y = y,
                          xlab = 'Occurence of thickets (%)',
                          ylab = ylab,
                          y_limits = limits)
  gg7 <- plot_correlation(data = data,
                          x = 'Pmax',
                          y = y,
                          xlab = expression(paste('Volume of selected food', ' (m'^3,')')),
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(0, 5))
  gg8 <- plot_correlation(data = data,
                          x = 'PC1',
                          y = y,
                          xlab = 'Horn characteristics',
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(-2, 3))
  gg9 <- plot_correlation(data = data,
                          x = 'Testo_mean',
                          y = y,
                          xlab = 'Testosterone metabolites (ng/g feces)',
                          ylab = ylab,
                          y_limits = limits,
                          x_limits = c(20, 100))
  pannel <- cowplot::plot_grid(gg8, gg9, gg2, gg3, gg4, gg5, gg6, gg7, gg1,
                               nrow = 3,
                               labels = c('A.',
                                          'B.',
                                          'C.',
                                          'D.',
                                          'E.',
                                          'F.',
                                          'G.',
                                          'H.',
                                          'I.'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  if (!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    ggsave(filename = paste0('./figures/', basename_fig, '.pdf'),
           plot = pannel,
           width = 11.5*3,
           height = 11*3,
           units = 'cm')
    message(paste0(basename_fig, '.pdf ',  "created and stored in directory 'figures'!"))
  }
  return(invisible(NULL))
}
