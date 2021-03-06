% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Nonacs.R
\name{compute_NonacsB}
\alias{compute_NonacsB}
\alias{NonacsB}
\alias{Nonacs}
\title{Compute the binomial skew index of Nonacs}
\usage{
compute_NonacsB(benef, time)
}
\arguments{
\item{benef}{The vector of benefits (i.e. mating or reproductive success).}

\item{time}{The vector of time-in (i.e. the time each individual spent in the
group).}
}
\value{
The observed Nonacs' binomial skew index value.
}
\description{
This function computes Nonacs' binomial skew index (B). Nonacs defines it as
an index ``based on the observed variance in a group corrected by the
expected variance if each member had an equal probability of gaining any
given group benefit or reproductive opportunity.''
}
\examples{
compute_NonacsB(benef = c(1, 1, 5), time = c(1, 1, 1))

}
\references{
Nonacs, P. (2000). Measuring and using skew in the study of
social behavior and evolution. The American Naturalist, 156(6), 577-589.
}
\seealso{
\code{\link{test_NonacsB}}
}
