#' Analysis of the Mating, Reproductive Success and Their Correlates in White
#' Rhinos
#'
#' This R package aims at providing the data and documenting the R code behind
#' the analysis of the paper entitled 'Mate choice, reproductive success and
#' inbreeding in white rhinos' by Kretzschmar Petra et al. (in prep).
#'
#' This package has not been conceived for general use!
#'
#' All main functions of this package contain a small documentation and examples
#' which could be useful for those who try to understand our code. Type
#' \code{ls('package:matingRhinos')} for a list of all exported functions and
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
#' ###############################################
#' ## Setting general options for this workflow ##
#' ###############################################
#'
#' ### Note: set the following options to TRUE or FALSE depending on what you want.
#'
#' save_figures_on_disk <- TRUE # export figures on drive?
#' 
#'
#' ############################
#' ## Preparing the datasets ##
#' ############################
#' 
#' ### 1. We split the dataset per cohorts:
#' malesC1 <- droplevels(males[males$Cohort == 'C1', ])
#' malesC2 <- droplevels(males[males$Cohort == 'C2', ])
#' femalesC1 <- droplevels(females[females$Cohort == 'C1', ])
#' femalesC2 <- droplevels(females[females$Cohort == 'C2', ])
#' 
#' ### 2. We visuallise the first 6 rows of all the datasets:
#' head(males)
#' head(malesC1)
#' head(malesC2)
#' head(females)
#'
#'
#' ########################
#' ## Parentage analysis ##
#' ########################
#'
#' ### 1. Note: the bioinformatic work leading to parentage has not been done using R.
#' 
#' ### 2. Result about the higher relatedness among C2 males than among C1 males:
#' round(mean(malesC1$Related_mean), digits = 3L)
#' round(sd(malesC1$Related_mean), digits = 3L)
#' round(mean(malesC2$Related_mean), digits = 3L)
#' round(sd(malesC2$Related_mean), digits = 3L)
#' 
#' wilcox.test(malesC1$Related_mean, malesC2$Related_mean)
#' 
#'
#' #####################################
#' ## Mating and reproductive success ##
#' #####################################
#' 
#' ### 1. Computing the skewness tests for males: 
#' test_NonacsB(benef = males$Rep_succ, time = males$Time, digits = 3L)
#' test_NonacsB(benef = males$Mat_succ, time = males$Time, digits = 3L)
#'
#' ### 2. Creating figure 1:
#' figure_NonacsB(data_males = males, data_females = females, savePDF = save_figures_on_disk)
#'
#' ### 3. Relationship between mating and reproductive success for males: 
#' compute_correlation(var1 = malesC1$Mat_succ, var2 = malesC1$Rep_succ)
#' compute_Bateman(mating_success = malesC1$Mat_succ, reproductive_success = malesC1$Rep_succ)
#' 
#' compute_correlation(var1 = malesC2$Mat_succ, var2 = malesC2$Rep_succ)
#' compute_Bateman(mating_success = malesC2$Mat_succ, reproductive_success = malesC2$Rep_succ)
#' 
#' compute_correlation(var1 = males$Mat_succ, var2 = males$Rep_succ)
#' compute_Bateman(mating_success = males$Mat_succ, reproductive_success = males$Rep_succ)
#'
#' ### 4. Creating figure 2:
#' figure_Bateman(data_agg = rhinos_agg, savePDF = save_figures_on_disk)
#'
#' ### 5. Computing the skewness tests for females: 
#' test_NonacsB(benef = females$Mat_succ, time = females$Time, digits = 3L)
#' test_NonacsB(benef = females$Rep_succ, time = females$Time, digits = 3L)
#'
#' ### 6. Relationship between mating and reproductive success for females:
#' compute_correlation(var1 = femalesC1$Mat_succ, var2 = femalesC1$Rep_succ)
#' compute_Bateman(mating_success = femalesC1$Mat_succ, reproductive_success = femalesC1$Rep_succ)
#' 
#' compute_correlation(var1 = femalesC2$Mat_succ, var2 = femalesC2$Rep_succ)
#' compute_Bateman(mating_success = femalesC2$Mat_succ, reproductive_success = femalesC2$Rep_succ)
#' 
#' compute_correlation(var1 = females$Mat_succ, var2 = females$Rep_succ)
#' compute_Bateman(mating_success = females$Mat_succ, reproductive_success = females$Rep_succ)
#'
#'
#' #################
#' ## Relatedness ##
#' #################
#' 
#' ### 1. Relatedness of male most related to females:
#' males[which.max(males$Related_mean), c('No', 'Cohort', 'Related_mean', 'Related_SD')]
#' 
#' ### 2. Creating figure S1:
#' figure_relatedness(data = males, savePDF = save_figures_on_disk)
#' 
#' ### 3. Correlation mating, reproductive success and relatedness:
#' compute_correlation(var1 = malesC1$Mat_succ, var2 = malesC1$Related_mean, n_tests = 6)
#' compute_correlation(var1 = malesC1$Rep_succ, var2 = malesC1$Related_mean, n_tests = 6)
#' compute_correlation(var1 = malesC2$Mat_succ, var2 = malesC2$Related_mean, n_tests = 6)
#'
#'
#' ######################
#' ## Male territories ##
#' ######################
#' 
#' ### 1. Computing the ranges of territories:
#' rbind(malesC1[which.min(malesC1$Ter_map), c('No', 'Cohort', 'Ter_map')],
#'       malesC1[which.max(malesC1$Ter_map), c('No', 'Cohort', 'Ter_map')])
#' 
#' rbind(malesC2[which.min(malesC1$Ter_map), c('No', 'Cohort', 'Ter_map')],
#'       malesC2[which.max(malesC1$Ter_map), c('No', 'Cohort', 'Ter_map')])
#' 
#' ### 2. Correlation mating, reproductive success and territory size:
#' compute_correlation(var1 = malesC1$Mat_succ, var2 = malesC1$Ter_map, n_tests = 6)
#' compute_correlation(var1 = malesC1$Rep_succ, var2 = malesC1$Ter_map, n_tests = 6)
#' compute_correlation(var1 = malesC2$Mat_succ, var2 = malesC2$Ter_map, n_tests = 6)
#' 
#' 
#' ##########################
#' ## Horn characteristics ##
#' ##########################
#'
#' ### 1. Running the Principal Component Analyses:
#' pca_C1_males <- compute_pca(data = malesC1)
#' pca_C2_males <- compute_pca(data = malesC2)
#' 
#' ### 2. Correlation mating, reproductive success and horn characteristics:
#' compute_correlation(var1 = pca_C1_males$data$Mat_succ, var2 = pca_C1_males$data$PC1, n_tests = 6)
#' compute_correlation(var1 = pca_C1_males$data$Rep_succ, var2 = pca_C1_males$data$PC1, n_tests = 6)
#' compute_correlation(var1 = pca_C2_males$data$Mat_succ, var2 = pca_C2_males$data$PC1, n_tests = 6)
#'
#' ### 3. Creating figure S3:
#' figure_pca(data = males, savePDF = save_figures_on_disk)
#' 
#' 
#' #####################
#' ## Habitat quality ##
#' #####################
#' 
#' ### 1. Correlation mating, reproductive success and occurence of medium open to dense thickets:
#' compute_correlation(var1 = malesC1$Mat_succ, var2 = malesC1$Me_open, n_tests = 6)
#' compute_correlation(var1 = malesC1$Rep_succ, var2 = malesC1$Me_open, n_tests = 6)
#' compute_correlation(var1 = malesC2$Mat_succ, var2 = malesC2$Me_open, n_tests = 6)
#' 
#' ### 2. Correlation mating, reproductive success and occurence of dense thickets:
#' compute_correlation(var1 = malesC1$Mat_succ, var2 = malesC1$Thick, n_tests = 6)
#' compute_correlation(var1 = malesC1$Rep_succ, var2 = malesC1$Thick, n_tests = 6)
#' compute_correlation(var1 = malesC2$Mat_succ, var2 = malesC2$Thick, n_tests = 6)
#' 
#' 
#' ########################
#' ## Vegetation quality ##
#' ########################
#' 
#' ### 1. Correlation mating, reproductive success and volume of Panicum maximum:
#' compute_correlation(var1 = malesC1$Mat_succ, var2 = malesC1$Pmax, n_tests = 6)
#' compute_correlation(var1 = malesC1$Rep_succ, var2 = malesC1$Pmax, n_tests = 6)
#' compute_correlation(var1 = malesC2$Mat_succ, var2 = malesC2$Pmax, n_tests = 6)
#' 
#' 
#' #############################################
#' ##  Testosterone metabolites concentration ##
#' #############################################
#' 
#' ### 1. Computing the correlations and testing them:
#' compute_correlation(var1 = malesC1$Mat_succ, var2 = malesC1$Testo_mean, n_tests = 6)
#' compute_correlation(var1 = malesC1$Rep_succ, var2 = malesC1$Testo_mean, n_tests = 6)
#' compute_correlation(var1 = malesC2$Mat_succ, var2 = malesC2$Testo_mean, n_tests = 6)
#'
#' ### 2. Creating figure S4:
#' figure_testosterone(data = males, savePDF = save_figures_on_disk)
#'
#'
#' ######################
#' ## All correlations ##
#' ######################
#' 
#' ### Creating figures 3 & 4:
#' figure_correlations(data = males, savePDF = save_figures_on_disk)
#'
NULL