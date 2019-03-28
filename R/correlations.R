#' Compute a Spearman correlation test between two variables
#'
#' This function computes the Spearman correlation between two variables. It is
#' a simple wrapper around the R function \code{\link{cor.test}}, with better
#' display of the output.
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
#' @examples
#' compute_correlation(var1 = males$Mat_succ, var2 = males$Rep_succ)
#' 
compute_correlation <- function(var1, var2) {
  d <- stats::na.omit(data.frame(var1 = var1, var2 = var2))
  corr <- stats::cor.test(~ var1 + var2, data = d, method = "spearman")
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
#' correction for multiple testing (default = 'holm').
#' 
#' @param cohort The cohort of males ('C1' or 'C2').
#' @param fitness The column name for the fitness component ('Mat_succ' or 'Rep_succ').
#' @param method The method for the multiple testing correction, see \code{\link{p.adjust}}.
#'
#' @return A data.frame containing the predictor, the sample size after
#'   discarding the missing values, the correlation between two variables, the
#'   p-value of the correlation test, the p-value after correction for
#'   multiple testing, and significance stars for the raw and adjusted p-values.
#'   
#' @export
#' 
#' @examples
#' compute_correlation_table(cohort = 'C1', fitness = 'Mat_succ', method = 'holm', data = males)
#' 
compute_correlation_table <- function(cohort = NULL, fitness = NULL, method = 'holm', data = NULL) {
  predictors <- c('Related_mean', 'Ter_map', 'Open', 'Me_open', 'Me_thick', 'Thick', 'Pmax', 'Horn', 'Testo_mean')
  fitness_var <- data[data$Cohort == cohort, fitness] 
  list_corrs <- lapply(predictors, function(var) {
    d <- stats::na.omit(data.frame(fitness_var = fitness_var, predictor = data[data$Cohort == cohort, var]))
    corr <- stats::cor.test(~ fitness_var + predictor, data = d, method = "spearman")
    c(n_obs = nrow(d), rho = signif(corr$estimate[[1]], 2), p = corr$p.value[[1]])
    })
  out <- data.frame(predictors, do.call('rbind', list_corrs))
  out$`p*` <- .pretty_star(out$p)
  out$p_adj <- stats::p.adjust(out$p, method = method)
  out$`p_adj*` <- .pretty_star(out$p_adj)
  out$p <- .pretty_ps(out$p, prefix = FALSE)
  out$p_adj <- .pretty_ps(out$p_adj, prefix = FALSE)
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
  col1 <- 'red'
  col2 <- 'blue'
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col1 <- 'black'
    col2 <- 'black'
  }
  gg <- ggplot(data = data, aes(y = !!sym(y), x = !!sym(x), shape = Cohort, col = Cohort)) +
    labs(y = ylab, x = xlab) +
    scale_y_continuous(limits = limits, breaks = function(x) seq(0, x[2], by = 2L)) +
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
    basename_fig <- 'figure3_correlations_mating'
    limits <- c(0L, 14L)
  }
  if (which == 'repro') {
    y <- 'Rep_succ'
    ylab <- 'Number of offspring'
    basename_fig <- 'figure4_correlations_repro'
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
                          limits = limits)
  gg2 <- plot_correlation(data = data,
                          x = 'Ter_map',
                          y = y,
                          xlab = expression(paste('Territory size (km'^2,')')),
                          ylab = ylab,
                          limits = limits)
  gg3 <- plot_correlation(data = data,
                          x = 'Open',
                          y = y,
                          xlab = 'Occurence of grassland (%)',
                          ylab = ylab,
                          limits = limits)
  gg4 <- plot_correlation(data = data,
                          x = 'Me_open',
                          y = y,
                          xlab = 'Occurence of open woodland (%)',
                          ylab = ylab,
                          limits = limits)
  gg5 <- plot_correlation(data = data,
                          x = 'Me_thick',
                          y = y,
                          xlab = 'Occurence of close woodland (%)',
                          ylab = ylab,
                          limits = limits)
  gg6 <- plot_correlation(data = data,
                          x = 'Thick',
                          y = y,
                          xlab = 'Occurence of thickets (%)',
                          ylab = ylab,
                          limits = limits)
  gg7 <- plot_correlation(data = data,
                          x = 'Pmax',
                          y = y,
                          xlab = expression(paste('Volume of ', italic('Panicum maximum'), ' (m'^3,')')),
                          ylab = ylab,
                          limits = limits)
  gg8 <- plot_correlation(data = data,
                          x = 'PC1',
                          y = y,
                          xlab = 'Horn characteristics',
                          ylab = ylab,
                          limits = limits)
  gg9 <- plot_correlation(data = data,
                          x = 'Testo_mean',
                          y = y,
                          xlab = 'Testosterone metabolites (ng/g feces)',
                          ylab = ylab,
                          limits = limits)
  pannel <- cowplot::plot_grid(gg1, gg2, gg3, gg4, gg5, gg6, gg7, gg8, gg9,
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
