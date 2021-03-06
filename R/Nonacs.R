#' Compute the binomial skew index of Nonacs
#'
#' This function computes Nonacs' binomial skew index (B). Nonacs defines it as
#' an index ``based on the observed variance in a group corrected by the
#' expected variance if each member had an equal probability of gaining any
#' given group benefit or reproductive opportunity.''
#'
#' @param benef The vector of benefits (i.e. mating or reproductive success).
#' @param time The vector of time-in (i.e. the time each individual spent in the
#'   group).
#' 
#' @aliases compute_NonacsB NonacsB Nonacs
#'
#' @return The observed Nonacs' binomial skew index value.
#' @export
#' @references Nonacs, P. (2000). Measuring and using skew in the study of
#' social behavior and evolution. The American Naturalist, 156(6), 577-589.
#' 
#' @seealso \code{\link{test_NonacsB}}
#'
#' @examples
#' compute_NonacsB(benef = c(1, 1, 5), time = c(1, 1, 1))
#' 
compute_NonacsB <- function(benef, time) {
  if (length(benef) != length(time)) stop("arguments of wrong length")
  K <- sum(benef)
  p <- benef/K
  Nt <- sum(time)
  Nmax <- max(time)
  var_raw <- sum((p - time/Nt)^2)
  correction <- (1 - 1/(Nt/Nmax))/K
  B <- var_raw - correction
  return(B)
}


#' Test the significance of the Nonacs' binomial skew index
#'
#' This functions compare the observed values of the Nonacs' binomial skew index
#' to expectation under the null hypothesis of a contribution to the mating or
#' breeding pool proportional to the time spent in the group.
#'
#' @inheritParams compute_NonacsB
#' @param nsim The number of simulation to run.
#' @param keep_H0 A boolean indicating whether to export the values of
#'  the Nonacs' binomial skew index simulated under the null hypothesis.
#' @param seed The seed for the random number generator.
#'
#' @return A list with the observed Nonacs' binomial skew index value, its
#'   corresponding p-value and (optionally) the values computed on data
#'   simulated under the null hypothesis.
#' @export
#' 
#' @seealso \code{\link{compute_NonacsB}}
#' 
#' @examples
#' test_NonacsB(benef = males$Rep_succ, time = males$Time)
#' 
test_NonacsB <- function(benef, time, nsim = 1e5L, keep_H0 = FALSE, seed = 1L) {
  set.seed(seed)
  Obs <- compute_NonacsB(benef = benef, time = time)
  H0 <- replicate(nsim, {
    benef_no_skew <- as.numeric(stats::rmultinom(n = 1, size = sum(benef), prob = time/sum(time)))
    compute_NonacsB(benef = benef_no_skew, time = time)
  })
  pv <- (sum(H0 > Obs) + 1) / (nsim + 1)
 out <- list(B_obs = Obs, B_obs_pretty = .pretty_p(Obs, raw = TRUE), p = .pretty_p(pv), N = length(benef))
 if (keep_H0) {
   out[['B_H0']] <- H0
 }
 return(out)
}



#' Plot the result of the null hypothesis testing of the Nonacs' binomial skew
#' index
#' 
#' This function creates a plot of the outcome of the null hypothesis testing of
#' the Nonacs' binomial skew index using the package ggplot2.
#'
#' @param x The object returned by the function \code{\link{test_NonacsB}}.
#' @inheritParams plot_relatedness 
#'
#' @return A ggplot object.
#' @export
#' @import ggplot2
#'
#' @examples
#' plot_NonacsB(test_NonacsB(benef = males$Rep_succ,
#'                           time = males$Time,
#'                           keep_H0 = TRUE))
#' 
plot_NonacsB <- function(x, limits = c(-0.02, 0.05)) {
  col <- 'red'
  if (!is.null(options('matingRhinos_colours')[[1]]) && !options('matingRhinos_colours')[[1]]) {
    col <- 'black'
  }
  
  if (is.null(x$B_H0)) {
    stop("The object 'x' is missing an element 'B_H0', rerun the function 'test_NonacsB()' with argument 'keep_H0 = TRUE'")
  }
  gg <- ggplot() + 
   geom_histogram(aes(x = x$B_H0), fill = 'lightgrey', colour = 'grey') +
   labs(x = "Nonac's binomial skew index (B)", y = 'Number of simulations | H0') +
   geom_vline(aes(xintercept = x$B_obs), colour = col, lwd = 1, lty = 2) +
   scale_x_continuous(limits = limits, breaks = seq(-0.2, 0.2, by = 0.02)) +
   theme_classic() +
   theme(plot.margin = unit(c(10, 4, 5, 1), 'mm'),
         axis.ticks.y = element_blank(), axis.text.y = element_blank(),
         text = element_text(size = 16))
  r <- ggplot_build(gg)$layout$panel_scales_y[[1]]$range$range
  pos_y <- (max(r) - min(r))*1.1
  
  gg <- gg + 
    scale_y_continuous(limits = c(0, (max(r) - min(r))*1.3)) +
    geom_text(aes(x = x$B_obs + 0.003, y = pos_y, label = x$p),
                       colour = col, vjust = 0, hjust = 0)
  return(gg)
}


#' Create the figure showing the results of the Nonacs' binomial skew analysis
#' 
#' This function creates the figure showing the results of the Nonacs' binomial skew analysis for both sexes.
#' It is a wrapper around the function \code{\link{plot_NonacsB}}.
#'
#' @param data_males The dataset for males.
#' @param data_females The dataset for females.
#' @inheritParams figure_PCA
#'
#' @export
#'
#' @examples
#' figure_NonacsB(data_males = males, data_females = females)
#' 
figure_NonacsB <- function(data_males, data_females) {
  malesMat   <- test_NonacsB(benef = data_males$Mat_succ, time = data_males$Time, keep_H0 = TRUE)
  femalesMat <- test_NonacsB(benef = data_females$Mat_succ, time = data_females$Time, keep_H0 = TRUE)
  malesRep   <- test_NonacsB(benef = data_males$Rep_succ, time = data_males$Time, keep_H0 = TRUE)
  femalesRep <- test_NonacsB(benef = data_females$Rep_succ, time = data_females$Time, keep_H0 = TRUE)
  gg1 <- plot_NonacsB(x = malesMat)
  gg2 <- plot_NonacsB(x = femalesMat)
  gg3 <- plot_NonacsB(x = malesRep)
  gg4 <- plot_NonacsB(x = femalesRep)
  pannel <- cowplot::plot_grid(gg1, gg3, gg2, gg4,   ## gg3 before gg2 to have males on top and females below. 
                               nrow = 2,
                               labels = c('A. Males C1 & C2 mating success',
                                          'B. Males C1 & C2 reproductive success',
                                          'C. Females mating success',
                                          'D. Females reproductive success'),
                               label_x = 0.02,
                               label_y = 1,
                               hjust = 0)
  print(pannel)
  if (!is.null(options('matingRhinos_PDF')[[1]]) && options('matingRhinos_PDF')[[1]][[1]]) {
    if (!dir.exists('./figures')) {
      dir.create('./figures')
    }
    ggsave(filename = './figures/figure1_NonacsB.pdf',
           plot = pannel,
           width = 11.5*2,
           height = 11*2,
           units = 'cm')
    message("figure1_NonacsB.pdf created and stored in directory 'figures'!")
  }
  return(invisible(NULL))
}


