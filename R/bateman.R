#' Plot the relationship between mating and reproductive success
#'
#' This function creates a plot of the relationship between mating and 
#' reproductive success.
#'
#' @param data_agg A dataframe of aggregated data.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @export
#'
#' @examples
#' plot_bateman(data_agg = males_agg)
#' 
plot_bateman <- function(data_agg) {
  Mat_succ <- Rep_succ <- Cohort <- Count <- NULL ## to please R CMD check
  y_max_nice <- ifelse(max(data_agg$Mat_succ) %% 2 == 0, max(data_agg$Rep_succ), max(data_agg$Rep_succ) + 1)
  x_max_nice <- ifelse(max(data_agg$Mat_succ) %% 2 == 0, max(data_agg$Mat_succ), max(data_agg$Mat_succ) + 1)
  gg <- ggplot(data = data_agg, mapping = aes(x = Mat_succ,
                                              y = Rep_succ,
                                              shape = Cohort,
                                              size = Count,
                                              fill = Cohort)) + 
    geom_point(alpha = 0.8) +
    labs(x = "Number of mates", y = "Number of offspring") +
    scale_x_continuous(limits = c(0, x_max_nice), breaks = function(x) seq(0, x[2], by = 2)) +
    scale_y_continuous(limits = c(0, y_max_nice), breaks = function(x) seq(0, x[2], by = 2)) +
    scale_shape_manual(values = c(22, 24), name = "Cohort of males:") +
    scale_fill_manual(values = c("black", "white"), name = "Cohort of males:") +
    scale_size(range = c(3, 6), breaks = c(1, 2), name = "Number of rhinos:") +
    theme_classic() +
    theme(plot.margin = unit(c(10, 4, 5, 1), "mm"), legend.position = c(0.2, 0.8))
  return(gg)
}
