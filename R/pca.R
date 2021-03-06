#' Compute a PCA on horn characteristics
#'
#' This function computes a Principal Component Analysis (PCA) on male horn
#' measurements using \code{\link{prcomp}}. The PCA is computed on the centered 
#' and scaled variables.
#'
#' @param data A dataframe containing the data to be processed.
#'
#' @return A list providing the original data with the first 2 principal
#'   components added to it, the proportion of variance explained by each
#'   component, the cummulated proportion of variance explained by each
#'   component, and the PCA object as returned by \code{\link{prcomp}}.
#' @seealso \code{\link{plot_PCA}} \code{\link{figure_PCA}}
#' @export
#'
#' @examples
#' PCA_C1_males <- compute_PCA(males[males$Cohort == 'C1', ])
#' PCA_C2_males <- compute_PCA(males[males$Cohort == 'C2', ])
#' 
compute_PCA <- function(data) {
  PCA_horn <- stats::prcomp(~ Circ_1 + Length_1 + Circ_2 + Length_2, data = data, scale. = TRUE)
  var_expl <- PCA_horn$sdev^2/sum(PCA_horn$sdev^2)
  cum_var_expl <- cumsum(var_expl)
  data$PC2 <- data$PC1 <- NA
  complete <- rownames(data)[match(rownames(PCA_horn$x), rownames(data))]
  data[complete, 'PC1'] <- PCA_horn$x[, 'PC1'][complete]
  data[complete, 'PC2'] <- PCA_horn$x[, 'PC2'][complete]
  print(paste('var explained by PC1 (%) =', signif(100*var_expl[1], digits = 3L)), quote = 'FALSE')
  return(invisible(list(data = data,
                        var_expl = var_expl,
                        cum_var_expl = cum_var_expl,
                        PCA = PCA_horn)))
}


#' Plot the result of a PCA
#'
#' This function creates a plot of the outcome of a Principal Component Analysis
#' (PCA) using the package ggplot2.
#'
#' @param x The object returned by the function \code{\link{compute_PCA}}.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @seealso \code{\link{compute_PCA}} \code{\link{figure_PCA}}
#' @export
#'
#' @examples
#' PCA_C1_males <- compute_PCA(males[males$Cohort == 'C1', ])
#' PCA_C2_males <- compute_PCA(males[males$Cohort == 'C2', ])
#' plot_PCA(PCA_C1_males)
#' plot_PCA(PCA_C2_males)
#' 
plot_PCA <- function(x) {
  col <- 'black'
  if (length(unique(x$data$Cohort)) == 1 &&
      !is.null(options('matingRhinos_colours')[[1]]) &&
      options('matingRhinos_colours')[[1]]) {
    col <- ifelse(unique(x$data$Cohort) == 'C1', 'red', 'blue')
  }
  labels <- as.character(rownames(x$PCA$rotation))
  labels[labels == 'Circ_1'] <- 'circ. anterior horn'
  labels[labels == 'Circ_2'] <- 'circ. posterior horn'
  labels[labels == 'Length_1'] <- 'length anterior horn'
  labels[labels == 'Length_2'] <- 'length posterior horn'
  gg <- ggplot(data = data.frame(x$PCA$rotation)) +
    geom_vline(xintercept = 0, lty = 2, col = 'lightgrey') +
    geom_hline(yintercept = 0, lty = 2, col = 'lightgrey') +
    ggforce::geom_circle(mapping = aes(x0 = 0, y0 = 0, r = 1), lty = 3, lwd = 0.2) +
    geom_segment(mapping = aes(x = PC1, y = PC2),
                 xend = 0, yend = 0,
                 col = col) +
    geom_label(aes(x = PC1, y = PC2, label = labels), col = col) +
    coord_fixed() +
    scale_y_continuous(limits = c(-1.2, 1.1), breaks = seq(-1, 1, by = 0.5)) +
    scale_x_continuous(limits = c(-1.2, 1.2), breaks = seq(-1, 1, by = 0.5)) +
    labs(x = paste0('PC1 (', signif(x$var_expl[1]*100, 3), '%)'),
         y =  paste0('PC2 (', signif(x$var_expl[2]*100, 3), '%)')) +
    theme_classic() +
    theme(plot.margin = unit(c(10, 2, 2, 2), 'mm'),
          text = element_text(size = 16))
  return(gg)
}
utils::globalVariables(c('PC1', 'PC2'))


#' Create the figure showing the PCA results
#' 
#' This function creates the figure showing the results of the PCAs. It is a wrapper
#' around the function \code{\link{compute_PCA}} and \code{\link{plot_PCA}}.
#'
#' @inheritParams compute_PCA
#' @seealso \code{\link{compute_PCA}} \code{\link{plot_PCA}}
#' @export
#'
#' @examples
#' figure_PCA(data = males)
#' 
figure_PCA <- function(data) {
  PCA_C1_males <- compute_PCA(data[data$Cohort == 'C1', ])
  PCA_C2_males <- compute_PCA(data[data$Cohort == 'C2', ])
  gg1 <- plot_PCA(PCA_C1_males)
  gg2 <- plot_PCA(PCA_C2_males)
  pannel <- cowplot::plot_grid(gg1,
                               gg2,
                               nrow = 1,
                               labels = c('A. Males C1', 'B. Males C2'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  if (!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    ggsave(filename = './figures/figure3_PCA.pdf',
           plot = pannel,
           width = 11.5*2,
           height = 11,
           units = 'cm')
    message("figure3_PCA.pdf created and stored in directory 'figures'!")
  }
  return(invisible(NULL))
}
