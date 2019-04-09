#' Plot the results of the simulated relatedness
#'
#' This function creates a plot of the simulated relatedness 
#' 
#' @param d_sim A dataframe of simulated relatedness
#' @name figure_relatedness_simulation
#' @return A ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' data(d_sim)
#' figure_relatedness_simulation(d_sim)
#' 
figure_relatedness_simulation <- function(d_sim){
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

DyadML <- x <- xend <- y <- yend <- NULL

out <- ggplot(d_sim, aes(x = cat, y = DyadML)) +
  geom_boxplot(width = 0.8, col = col1) +
  scale_x_discrete("True Relationship") +
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
