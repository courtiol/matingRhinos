#' Compute a PCA on horn characteristics
#'
#' This function computes a Principal Component Analysis (PCA) on male horn
#' measurements using \code{\link{prcomp}}. The PCA is used on the centered and
#' scaled variables.
#'
#' @param data A dataframe used as an input for the PCA.
#' @inheritParams test_NonacsB
#'
#' @return A list providing the original data with the first 2 principal
#'   components added to it, the proportion of variance explained by each
#'   component, the cummulated proportion of variance explained by each
#'   component, and the PCA object as returned by \code{\link{prcomp}}.
#' @seealso \code{\link{plot_pca}} \code{\link{figure_pca}}
#' @export
#'
#' @examples
#' pca_C1_males <- compute_pca(males[males$Cohort == 'C1', ])
#' pca_C2_males <- compute_pca(males[males$Cohort == 'C2', ])
#' 
compute_pca <- function(data, digits = 3L) {
  pca_horn <- stats::prcomp(~ Circ_1 + Length_1 + Circ_2 + Length_2, data = data, scale. = TRUE)
  var_expl <- pca_horn$sdev^2/sum(pca_horn$sdev^2)
  cum_var_expl <- cumsum(var_expl)
  data$PC2 <- data$PC1 <- NA
  complete <- rownames(data)[match(rownames(pca_horn$x), rownames(data))]
  data[complete, 'PC1'] <- pca_horn$x[, 'PC1'][complete]
  data[complete, 'PC2'] <- pca_horn$x[, 'PC2'][complete]
  print(paste('var explained by PC1 (%) =', prettyNum(100*var_expl[1], digits = digits)), quote = 'FALSE')
  return(invisible(list(data = data,
                        var_expl = var_expl,
                        cum_var_expl = cum_var_expl,
                        pca = pca_horn)))
}


#' Plot the result of a PCA
#'
#' This function creates a plot of the outcome of a Principal Component Analysis
#' (PCA) using the package ggplot2.
#'
#' @param x The object returned by the function \code{\link{compute_pca}}.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @seealso \code{\link{compute_pca}} \code{\link{figure_pca}}
#' @export
#'
#' @examples
#' pca_C1_males <- compute_pca(males[males$Cohort == 'C1', ])
#' pca_C2_males <- compute_pca(males[males$Cohort == 'C2', ])
#' plot_pca(pca_C1_males)
#' plot_pca(pca_C2_males)
#' 
plot_pca <- function(x) {
  labels <- as.character(rownames(x$pca$rotation))
  labels[labels == 'Circ_1'] <- 'circ. anterior horn'
  labels[labels == 'Circ_2'] <- 'circ. posterior horn'
  labels[labels == 'Length_1'] <- 'length anterior horn'
  labels[labels == 'Length_2'] <- 'length posterior horn'
  gg <- ggplot(data = data.frame(x$pca$rotation)) +
    geom_vline(xintercept = 0, lty = 2, col = 'lightgrey') +
    geom_hline(yintercept = 0, lty = 2, col = 'lightgrey') +
    ggforce::geom_circle(mapping = aes(x0 = 0, y0 = 0, r = 1), lty = 3, lwd = 0.2) +
    geom_segment(mapping = aes(x = PC1, y = PC2), xend = 0, yend = 0
    ) +
    geom_label(aes(x = PC1, y = PC2, label = labels)) +
    coord_fixed() +
    scale_y_continuous(limits = c(-1.2, 1.1), breaks = seq(-1, 1, by = 0.5)) +
    scale_x_continuous(limits = c(-1.2, 1.2), breaks = seq(-1, 1, by = 0.5)) +
    labs(x = paste0('PC1 (', signif(x$var_expl[1]*100, 3), '%)'),
         y =  paste0('PC2 (', signif(x$var_expl[2]*100, 3), '%)')) +
    theme_classic() +
    theme(plot.margin = unit(c(5, 2, 2, 2), 'mm'))
  return(gg)
}
utils::globalVariables(c('PC1', 'PC2'))


#' Create the figure showing the PCA results
#' 
#' This function creates the figure showing the results of the PCAs. It is a wrapper
#' around the function \code{\link{compute_pca}} and \code{\link{plot_pca}}.
#'
#' @param data The dataset on which to perform the PCAs.
#' @param savePDF A boolean indicating whether to create a PDF of the plot in the
#' current working directory (TRUE) or not (FALSE, default).
#' @seealso \code{\link{compute_pca}} \code{\link{plot_pca}}
#' @export
#'
#' @examples
#' figure_pca(data = males)
#' 
figure_pca <- function(data, savePDF = FALSE) {
  pca_C1_males <- compute_pca(data[data$Cohort == 'C1', ])
  pca_C2_males <- compute_pca(data[data$Cohort == 'C2', ])
  gg1 <- plot_pca(pca_C1_males)
  gg2 <- plot_pca(pca_C2_males)
  pannel <- cowplot::plot_grid(gg1,
                               gg2,
                               nrow = 1,
                               labels = c('A. Males C1', 'B. Males C2'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  if (savePDF) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    cowplot::ggsave(filename = './figures/figure_pca.pdf',
                    plot = pannel,
                    width = 12*2,
                    height = 12,
                    units = 'cm')
    message("figure_pca.pdf created and stored in directory 'figures'!")
  }
  return(invisible(NULL))
}
