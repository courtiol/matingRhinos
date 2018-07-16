#' Compute the the binomial skew index (B) of Nonacs
#' 
#' This function computes the Nonacs' binomial skew index. Nonacs defines it as an index 
#' ``based on the observed variance in a group corrected by the expected variance if 
#' each member had an equal probability of gaining any given group benefit or reproductive opportunity.''
#'
#' @param benef The vector of benefits (i.e. mating or reproductive success).
#' @param time The vector of time-in (i.e. the time each individual spent in the group).
#'
#' @return
#' @export
#' @references
#' Nonacs, P. (2000). Measuring and using skew in the study of social behavior
#' and evolution. The American Naturalist, 156(6), 577-589.
#'
#' @examples
#' Nonacs(benef = c(1, 1, 1), time = c(1, 1, 1))
#' 
Nonacs <- function(benef, time) {
  K <- sum(benef)
  p <- benef/K
  Nt <- sum(time)
  Nmax <- max(time)
  sum((p - time/Nt)^2) - (1 - 1/(Nt/Nmax))/K
}