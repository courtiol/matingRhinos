#' Plot the relationship between mating and reproductive success
#'
#' This function creates a plot of the relationship between mating and 
#' reproductive success.
#'
#' @param data_agg A dataframe of aggregated data.
#' @param sex The sex of the rhinos to plot ('males' or 'females').
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#' @seealso \code{\link{figure_Bateman}}
#'
#' @examples
#' plot_Bateman(data_agg = rhinos_agg, sex = "males")
#' plot_Bateman(data_agg = rhinos_agg, sex = "females")
#' 
plot_Bateman <- function(data_agg, sex) {
  if (!sex %in% c("males", "females")) stop("The argument sex must be 'males' or 'females'.")
  if (sex == "males") {
    steps <- 2
    counts_nice <- c(4, seq(4, 10, length = 17)[2])
    data_agg <- data_agg[data_agg$Sex == "males", ]
    do_guide1 <- guide_legend(override.aes = list(size = 10))
    do_guide2 <- FALSE
  } else {
    steps <- 1
    counts_nice <- c(4, 10)
    data_agg <- data_agg[data_agg$Sex == "females", ]
    do_guide1 <- FALSE
    do_guide2 <- guide_legend(override.aes = list(shape = 1))
  }
  Mat_succ <- Rep_succ <- Cohort <- Count <- NULL ## to please R CMD check
  y_max_nice <- ifelse(max(data_agg$Mat_succ) %% steps == 0, max(data_agg$Rep_succ), max(data_agg$Rep_succ))
  x_max_nice <- ifelse(max(data_agg$Mat_succ) %% steps == 0, max(data_agg$Mat_succ), max(data_agg$Mat_succ) + 1)
  gg <- ggplot(data = data_agg, mapping = aes(x = Mat_succ,
                                              y = Rep_succ,
                                              shape = Cohort,
                                              size = Count,
                                              fill = Cohort)) + 
    geom_point(alpha = 0.8) +
    labs(x = "Number of mates", y = "Number of offspring") +
    scale_x_continuous(limits = c(0, x_max_nice), breaks = function(x) seq(0, x[2], by = steps)) +
    scale_y_continuous(limits = c(0, y_max_nice), breaks = function(x) seq(0, x[2], by = steps)) +
    scale_shape_manual(values = c(22, 24), name = "Cohort of males:", guide = do_guide1) +
    scale_fill_manual(values = c("black", "white"), name = "Cohort of males:", guide = do_guide1) +
    scale_radius(range = counts_nice, breaks = c(1, 10, 17), name = "Number of rhinos:", guide = do_guide2) +
    theme_classic() +
    theme(plot.margin = unit(c(10, 4, 5, 1), "mm"), legend.position = "bottom", legend.box.margin = margin(5, 1, 1, 1, unit = "pt"))
  return(gg)
}


#' Create the figure showing the relationship between mating and reproductive success for both sexes
#' 
#' This function creates the figure showing the results of the the relationship between mating and reproductive success for both sexes.
#' It is a wrapper around the function \code{\link{plot_Bateman}}.
#'
#' @inheritParams figure_pca
#' @inheritParams plot_Bateman
#' @seealso \code{\link{plot_Bateman}}
#' @export
#'
#' @examples
#' figure_Bateman(data_agg = rhinos_agg)
#' 
figure_Bateman <- function(data_agg, savePDF = FALSE) {
  gg1 <- plot_Bateman(data_agg = data_agg, sex = "males")
  gg2 <- plot_Bateman(data_agg = data_agg, sex = "females")
  pannel <- cowplot::plot_grid(gg1,
                               gg2,
                               nrow = 1,
                               labels = c("A. Males", "B. Females"),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0) 
  print(pannel)
  
  if (savePDF) {
    if (!dir.exists("./figures")) {
      dir.create("./figures")
    }
    cowplot::ggsave(filename = "./figures/figure_Bateman.pdf",
                    plot = pannel,
                    width = 12*2,
                    height = 15,
                    units = "cm")
    message("figure_Bateman.pdf created and stored in directory 'figures'!")
  }
  return(invisible(NULL))
}
