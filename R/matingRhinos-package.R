#' Analysis of the Mating, Reproductive Success and Their Correlates in White
#' Rhinos
#'
#' This R package aims at providing the data and documenting the R code behind
#' the analysis of the paper entitled "Mate choice, reproductive success and
#' inbreeding in white rhinos" by Kretzschmar Petra et al. (in prep).
#'
#' This package has not been conceived for general use!
#'
#' All main functions of this package contain a small documentation and examples
#' which could be useful for those who try to understand our code. Type
#' \code{ls("package:matingRhinos")} for a list of all exported functions and
#' datasets.
#'
#' You may directly explore the files contained in the package on GitHub at
#' \url{https://github.com/courtiol/matingRhinos}, or after uncompressing the
#' content of the *.tar.gz file (link available on the GitHub as well). You can
#' use the R function \code{\link{untar}} to extract the content of the tarball.
#'
#' The package contains all the original data.
#'
#' In the examples below, we provide the workflow leading the results presented
#' in the paper.
#'
#' @name matingRhinos-package
#' @aliases matingRhinos-package matingRhinos
#' @docType package
#'
#' @references
#' Kretzschmar Petra et al. (in prep)
#' Mate choice, reproductive success and inbreeding in white rhinos.
#' 
#'
#' @keywords package
#' 
#' @examples
#' 
#' 
#' ################################################
#' ##  Setting general options for this workflow ##
#' ################################################
#'
#' ### Note: set the following options to TRUE or FALSE depending on what you want.
#'
#' save_figures_on_disk <- FALSE # export figures on drive?
#' 
#'
#'
#' ###########################################################
#' ##  Relationship between mating and reproductive success ##
#' ###########################################################
#' 
#' ### 1. Computing the correlations and testing them:
#' 
#' cor.test(~ Mat_succ + Rep_succ, data = males[males$Cohort == "C1", ], method = "spearman")
#' cor.test(~ Mat_succ + Rep_succ, data = males[males$Cohort == "C2", ], method = "spearman")
#'
#'  
#' ### 2. Creating the figure pannel:
#' 
#' figure_bateman(data_agg = rhinos_agg, savePDF = save_figures_on_disk)      
#'
#'
#'
#' #############################################################################
#' ##  Horn characteristics as a correlate of mating and reproductive success ##
#' #############################################################################
#'
#' ### 1. Running the Principal Component Analyses:
#' 
#' pca_C1_males <- do_pca(data = males[males$Cohort == "C1", ])
#' pca_C2_males <- do_pca(data = males[males$Cohort == "C2", ])
#' 
#' 
#' ### 2. Computing the correlations and testing them:
#' 
#' cor.test(~ PC1 + Mat_succ, data = pca_C1_males$data, method = "spearman")
#' cor.test(~ PC1 + Rep_succ, data = pca_C1_males$data, method = "spearman")
#' 
#' cor.test(~ PC1 + Mat_succ, data = pca_C2_males$data, method = "spearman")
#' cor.test(~ PC1 + Rep_succ, data = pca_C2_males$data, method = "spearman")
#'
#' 
#' ### 3. Creating the figure pannel:
#' 
#' figure_pca(data = males, savePDF = save_figures_on_disk)
#'
#'
NULL