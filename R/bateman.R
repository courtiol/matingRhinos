#' Compute the Bateman gradient
#'
#' This function computes the Bateman gradient. The Bateman gradient is the
#' slope of the regression line of the reproductive success on the mating
#' success. Following Jones 2009 (and unlike some other work), we consider the
#' relative reproductive success and the relative mating success to compute the
#' gradient; i.e. we divide each variable by the population mean before
#' fitting the linear model.
#' 
#' @param mating_success A vector of mating success outcomes.
#' @param reproductive_success A vector of reproductive success outcomes.
#' @inheritParams test_NonacsB
#'
#' @return The Bateman gradient.
#' @export
#' @references Jones A.G (2009).On the opportunity for sexual selection, the
#'   Bateman gradient and the maximum intensity of sexual selection. Evolution,
#'   63(7), 1673-1684.
#' 
#' @seealso \code{\link{test_NonacsB}}
#'
#' @examples
#' compute_Bateman(mating_success = males$Mat_succ,
#'                 reproductive_success = males$Rep_succ)
#' 
compute_Bateman <- function(mating_success, reproductive_success) {
  d <- data.frame(mating_success_rel = mating_success / mean(mating_success, na.rm = TRUE),
                  reproductive_success_rel = reproductive_success / mean(reproductive_success, na.rm = TRUE),
                  mating_success = mating_success,
                  reproductive_success = reproductive_success)
  d <- stats::na.omit(d)
  B <- stats::coef(stats::lm(reproductive_success_rel ~ mating_success_rel, data = d))[['mating_success_rel']]
  print(.pretty_p(B, digits = 3L, raw = TRUE))
  return(invisible(B))
}


#' Plot the relationship between mating and reproductive success
#'
#' This function creates a plot of the relationship between mating and 
#' reproductive success.
#'
#' @param data_agg A dataframe of aggregated data to be processed.
#' @param sex The sex of the rhinos to plot ('males' or 'females').
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_Bateman}}
#'
#' @examples
#' plot_Bateman(data_agg = rhinos_agg, sex = 'males')
#' plot_Bateman(data_agg = rhinos_agg, sex = 'females')
#' 
plot_Bateman <- function(data_agg, sex) {
  col1 <- 'red'
  col2 <- 'blue'
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col1 <- 'black'
    col2 <- 'black'
  }
  if (!sex %in% c('males', 'females')) stop("The argument sex must be 'males' or 'females'.")
  if (sex == 'males') {
    steps <- 2
    counts_nice <- c(4, seq(4, 10, length = 17)[2])
    data_agg <- data_agg[data_agg$Sex == 'males', ]
    do_guide1 <- guide_legend(override.aes = list(size = 5))
    do_guide2 <- FALSE
  } else {
    steps <- 1
    counts_nice <- c(4, 10)
    data_agg <- data_agg[data_agg$Sex == 'females', ]
    do_guide1 <- FALSE
    do_guide2 <- guide_legend(override.aes = list(shape = 22, fill = 'white', col = col1))
  }
  Mat_succ <- Rep_succ <- Cohort <- Count <- NULL ## to please R CMD check
  y_max_nice <- ifelse(max(data_agg$Mat_succ) %% steps == 0, max(data_agg$Rep_succ), max(data_agg$Rep_succ))
  x_max_nice <- ifelse(max(data_agg$Mat_succ) %% steps == 0, max(data_agg$Mat_succ), max(data_agg$Mat_succ) + 1)
  gg <- ggplot(data = data_agg, mapping = aes(x = Mat_succ,
                                              y = Rep_succ,
                                              shape = Cohort,
                                              colour = Cohort,
                                              size = Count,
                                              fill = Cohort)) + 
    geom_point(alpha = 0.8) +
    labs(x = 'Number of mates', y = 'Number of offspring') +
    scale_x_continuous(limits = c(0, x_max_nice), breaks = function(x) seq(0, x[2], by = steps)) +
    scale_y_continuous(limits = c(0, y_max_nice), breaks = function(x) seq(0, x[2], by = steps)) +
    scale_shape_manual(values = c(22, 24), name = 'Cohort of males:', guide = do_guide1) +
    scale_fill_manual(values = c('white', 'white'), name = 'Cohort of males:', guide = do_guide1) +
    scale_colour_manual(values = c(col1, col2), name = 'Cohort of males:', guide = do_guide1) +
    scale_radius(range = counts_nice, breaks = c(1, 10, 17), name = 'Number of rhinos:', guide = do_guide2) +
    theme_classic() +
    theme(plot.margin = unit(c(10, 4, 5, 1), 'mm'),
          legend.position = 'bottom',
          legend.box.margin = margin(5, 1, 1, 1, unit = 'pt'),
          text = element_text(size = 16))
  return(gg)
}


#' Create the figure showing the relationship between mating and reproductive success for both sexes
#' 
#' This function creates the figure showing the results of the the relationship between mating and reproductive success for both sexes.
#' It is a wrapper around the function \code{\link{plot_Bateman}}.
#'
#' @inheritParams figure_PCA
#' @inheritParams plot_Bateman
#' @seealso \code{\link{plot_Bateman}}
#' @export
#'
#' @examples
#' figure_Bateman(data_agg = rhinos_agg)
#' 
figure_Bateman <- function(data_agg) {
  gg1 <- plot_Bateman(data_agg = data_agg, sex = 'males')
  gg2 <- plot_Bateman(data_agg = data_agg, sex = 'females')
  pannel <- cowplot::plot_grid(gg1,
                               gg2,
                               nrow = 1,
                               labels = c('A. Males', 'B. Females'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0) 
  print(pannel)
  
  if (!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    ggsave(filename = './figures/figure2_Bateman.pdf',
           plot = pannel,
           width = 11.5*2,
           height = 13.5,
           units = 'cm')
    message("figure2_Bateman.pdf created and stored in directory 'figures'!")
  }
  return(invisible(NULL))
}
