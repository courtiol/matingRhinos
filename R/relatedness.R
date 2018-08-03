#' Plot the distribution of relatedness
#'
#' This function creates a plot of the distribution of relatedness using the
#' package ggplot2.
#' 
#' @inheritParams compute_PCA
#' @param limits The limit for the y-axis of the plot.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_relatedness}}
#'
#' @examples
#' plot_relatedness(data = males[males$Cohort == 'C1', ])
#' plot_relatedness(data = males[males$Cohort == 'C2', ])
#' plot_relatedness(data = males)
#'
plot_relatedness <- function(data, limits = c(-0.45, 0.45)) {
  col1 <- 'red'
  col2 <- 'blue'
  if (length(unique(data$Cohort)) == 1) {
    col1 <- col2 <- ifelse(data$Cohort == 'C1', 'red', 'blue')
  }
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col <- 'lightgrey'
  }
  data <- data[!is.na(data$Related_mean), ]
  gg <- ggplot(data = data, aes(y = Related_mean, x = No, fill = Cohort)) +
    geom_bar(stat = 'identity', width = 0.5) +
    labs(y = 'Mean relatedness to females', x = 'Individual') +
    geom_hline(yintercept = 0, colour = 'lightgrey') +
    scale_fill_manual(values = c(col1, col2), guide = FALSE) +
    scale_y_continuous(limits = limits) +
    theme_classic() +
    geom_linerange(aes(ymax = Related_mean + stats::qnorm(0.975)*Related_SD/sqrt(Related_N),
                       ymin = Related_mean + stats::qnorm(0.025)*Related_SD/sqrt(Related_N)), size = 0.5) +
    geom_point(shape = 3, size = 0.5) +
    theme(plot.margin = unit(c(10, 8, 2, 2), 'mm'))
  return(gg)
}
utils::globalVariables(c('Related_mean', 'Related_SD', 'Related_N', 'No'))


#' Create the figure showing the distribution of relatedness
#'
#' This function creates the figure showing the distribution of relatedness for
#' the two cohorts of males and for females. It is a wrapper around the function
#' \code{\link{plot_relatedness}}.
#' 
#' @inheritParams figure_PCA
#' @seealso \code{\link{plot_relatedness}}
#' @export
#'
#' @examples
#' figure_relatedness(data = males)
#' 
figure_relatedness <- function(data) {
  gg1 <- plot_relatedness(data = data[data$Cohort == 'C1', ])
  gg2 <- plot_relatedness(data = data[data$Cohort == 'C2', ])
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
    cowplot::ggsave(filename = './figures/figureS1_relatedness.pdf',
                    plot = pannel,
                    width = 12*2,
                    height = 12,
                    units = 'cm')
    message("figureS1_relatedness.pdf created and stored in directory 'figures'!")
  }
}