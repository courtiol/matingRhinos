% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relatedness.R
\name{figure_relatedness}
\alias{figure_relatedness}
\title{Create the figure showing the distribution of relatedness}
\usage{
figure_relatedness(data)
}
\arguments{
\item{data}{A dataframe containing the data to be processed.}
}
\description{
This function creates the figure showing the distribution of relatedness for
the two cohorts of males and for females. It is a wrapper around the function
\code{\link{plot_relatedness}}.
}
\examples{
figure_relatedness(data = males)

}
\seealso{
\code{\link{plot_relatedness}}
}
