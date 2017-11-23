#' distdecay
#'
#' Calcalate distance decay functions based on (dis)similarity matrices
#'
#' Input: One square matrix of (dis-)similarities between pairs of locations; one
#' squre matrix of corresponding distances.
#' 
#' Output: Distance decay functions for each location, quantified as either
#' covariance or mutual information coefficients.
#'
#' @name distdecay
#' @docType package
#' @author Mark Padgham
#' @importFrom bikedata bike_match_matrices
#' @importFrom ggplot2 facet_wrap geom_smooth ggplot guides 
#' @importFrom ggplot2 scale_fill_gradientn scale_x_log10 scale_y_log10 
#' @importFrom ggplot2 stat_binhex xlab ylab 
#' @importFrom graphics legend lines title
#' @importFrom jsonlite fromJSON
#' @importFrom Rcpp evalCpp
#' @importFrom stats AIC lm median nls predict sd t.test
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @useDynLib distdecay, .registration = TRUE
NULL
