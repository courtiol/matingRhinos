#' Plot the distribution of testosterone
#'
#' This function creates a plot of the distribution of testosterone using the
#' package ggplot2.
#' 
#' @inheritParams plot_relatedness
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_testosterone}}
#'
#' @examples
#' plot_testosterone(data = males[males$Cohort == 'C1', ])
#' plot_testosterone(data = males[males$Cohort == 'C2', ])
#' plot_testosterone(data = males)
#'
plot_testosterone <- function(data, limits = c(0, 120)) {
  col1 <- 'red'
  col2 <- 'blue'
  if (length(unique(data$Cohort)) == 1) {
    col1 <- col2 <- ifelse(data$Cohort == 'C1', 'red', 'blue')
  }
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col1 <- col2 <- 'lightgrey'
  }
  data <- data[!is.na(data$Testo_mean), ]
  gg <- ggplot(data = data, aes(y = Testo_mean, x = No, fill = Cohort)) +
    geom_bar(stat = 'identity', width = 0.5) +
    labs(y = 'Mean testosterone metababolites (ng/g feces)', x = 'Individual') +
    geom_hline(yintercept = 0, colour = 'lightgrey') +
    scale_fill_manual(values = c(col1, col2), guide = FALSE) +
    scale_y_continuous(limits = limits) +
    theme_classic() +
    geom_linerange(aes(ymax = Testo_mean + stats::qnorm(0.975)*Testo_SD/sqrt(Testo_N),
                      ymin = Testo_mean + stats::qnorm(0.025)*Testo_SD/sqrt(Testo_N)), size = 0.5) +
    geom_point(shape = 3, size = 0.5) +
    theme(plot.margin = unit(c(10, 8, 2, 2), 'mm'))
  return(gg)
}
utils::globalVariables(c('Testo_mean', 'Testo_SD', 'Testo_N', 'No'))

#' Create the figure showing the distribution of testosterone
#'
#' This function creates the figure showing the distribution of testosterone for
#' the two cohorts of males. It is a wrapper around the function 
#' \code{\link{plot_testosterone}}.
#' 
#' @inheritParams figure_PCA
#' @seealso \code{\link{plot_testosterone}}
#' @export
#'
#' @examples
#' figure_testosterone(data = males)
#' 
figure_testosterone <- function(data) {
  gg1 <- plot_testosterone(data = data[data$Cohort == 'C1', ])
  gg2 <- plot_testosterone(data = data[data$Cohort == 'C2', ])
  pannel <- cowplot::plot_grid(gg1,
                               gg2,
                               nrow = 1,
                               labels = c('A. Males C1', 'B. Males C2'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  
  if (!is.null(!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) && !is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    cowplot::ggsave(filename = './figures/figureS4_testosterone.pdf',
                    plot = pannel,
                    width = 12*2,
                    height = 12,
                    units = 'cm')
    message("figureS4_testosterone.pdf created and stored in directory 'figures'!")
  }
}