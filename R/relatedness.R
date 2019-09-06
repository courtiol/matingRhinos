#' Plot the distribution of relatedness
#'
#' This function creates a plot of the distribution of relatedness using the
#' package ggplot2.
#' 
#' @inheritParams compute_PCA
#' @param limits The limit for the y-axis of the plot.
#' @param to Either 'males' or 'females', to indicate of the relatedness between
#' the focal male is computed to males or females.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_relatedness}}
#'
#' @examples
#' plot_relatedness(data = males, to = 'males')
#' plot_relatedness(data = males, to = 'females')
#'
plot_relatedness <- function(data, limits = c(0, 0.5), to = 'males') {
  if (!to %in% c('males', 'females')) {
    stop("Wrong arugment for the parameter 'to'!")
  }
  col1 <- 'red'
  col2 <- 'blue'
  if (length(unique(data$Cohort)) == 1) {
    col1 <- col2 <- ifelse(data$Cohort == 'C1', 'red', 'blue')
  }
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col1 <- col2 <- 'lightgrey'
  }
  data <- data[!is.na(data$Related_mean), ]
  if (to == 'males') {
    var_y <- 'Related_mean_males'
    var_y_min <- with(data, Related_mean_males + stats::qnorm(0.025)*Related_SD_males/sqrt(Related_N_males))
    var_y_max <- with(data, Related_mean_males + stats::qnorm(0.975)*Related_SD_males/sqrt(Related_N_males))
    lab_y <- 'Mean relatedness to males'
  } else {
    var_y <- 'Related_mean'
    var_y_min <- with(data, Related_mean + stats::qnorm(0.025)*Related_SD/sqrt(Related_N))
    var_y_max <- with(data, Related_mean + stats::qnorm(0.975)*Related_SD/sqrt(Related_N))
    lab_y <- 'Mean relatedness to females'
  }
  gg <- ggplot(data = data, aes(y = !!sym(var_y), x = No, fill = Cohort)) +
    geom_bar(stat = 'identity', width = 0.5) +
    labs(y = lab_y, x = 'Individual') +
    geom_hline(yintercept = 0, colour = 'lightgrey') +
    scale_fill_manual(values = c(col1, col2), guide = FALSE) +
    scale_y_continuous(limits = limits) +
    theme_classic() +
    geom_linerange(aes(ymax = var_y_max, ymin = !!sym(var_y)), size = 0.5) +
    #geom_point(shape = 3, size = 0.5) +
    theme(plot.margin = unit(c(10, 8, 2, 2), 'mm'),
          text = element_text(size = 16))
  return(gg)
}
utils::globalVariables(c('var_y', 'No', 'Cohort'))


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
  gg1 <- plot_relatedness(data = data[data$Cohort == 'C1', ], limits = c(0, 0.3), to = 'males')
  gg2 <- plot_relatedness(data = data[data$Cohort == 'C2', ], limits = c(0, 0.5), to = 'males')
  gg3 <- plot_relatedness(data = data[data$Cohort == 'C1', ], limits = c(0, 0.3), to = 'females')
  gg4 <- plot_relatedness(data = data[data$Cohort == 'C2', ], limits = c(0, 0.3), to = 'females')
  pannel <- cowplot::plot_grid(gg1, gg2, gg3, gg4,
                               nrow = 2,
                               labels = c('A. Males C1 to males C1', 'B. Males C2 to males C2',
                                          'C. Males C1 to females C1', 'D. Males C2 to females C2'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  
  if (!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    ggsave(filename = './figures/figureS2_relatedness.pdf',
           plot = pannel,
           width = 11.5*2,
           height = 11*2,
           units = 'cm')
    message("figureS2_relatedness.pdf created and stored in directory 'figures'!")
  }
}


#' Create the figures showing the outcome of the simulations of alternative relatedness metrics
#' 
#' This function creates the figures used to identify the best relatedness metric.
#'
#' @inheritParams figure_PCA
#' @export
#'
#' @examples
#' figure_relatedness_simulation(data = relat_sim)
#' wilcox.test(relat_sim$DyadML[relat_sim$cat == "PO"], mu = 0.5)
#' wilcox.test(relat_sim$DyadML[relat_sim$cat == "FS"], mu = 0.5)
#' wilcox.test(relat_sim$DyadML[relat_sim$cat == "HS"], mu = 0.25)
#' wilcox.test(relat_sim$DyadML[relat_sim$cat == "FC"], mu = 1/8)
#' wilcox.test(relat_sim$DyadML[relat_sim$cat == "SC"], mu = 1/32)
#' wilcox.test(relat_sim$DyadML[relat_sim$cat == "U"], mu = 0)
#
figure_relatedness_simulation <- function(data){
  col1 <- 'black'
  col2 <- 'red'
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col1 <- 'black'
    col2 <- 'grey'
  }
  
  lines <- data.frame(y = c(0.5, 0.5, 0.25, 1/8, 1/32, 0), 
                      yend = c(0.5, 0.5, 0.25, 1/8, 1/32, 0), 
                      x = c(1:6-0.4), 
                      xend = c(1:6+0.4))
  
  
  out <- ggplot(data, aes(x = cat, y = DyadML)) +
    geom_boxplot(width = 0.8, col = col1) +
    scale_x_discrete("Relationship") +
    geom_segment(data = lines, aes(y = y, yend = yend, xend = xend, x = x), col = "red", linetype = 2) +
    theme_classic() +
    theme(plot.margin = unit(c(10, 4, 5, 1), 'mm'),
          legend.position = 'bottom',
          legend.box.margin = margin(5, 1, 1, 1, unit = 'pt'),
          text = element_text(size = 16))
  
  if (!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    ggsave(filename = './figures/figureS5_relatedness_simulation.pdf',
           plot = out,
           width = 11.5*2,
           height = 13.5,
           units = 'cm')
    message("figureS5_relatedness_simulation.pdf created and stored in directory 'figures'!")
  }
  out
  return(invisible(NULL))
}