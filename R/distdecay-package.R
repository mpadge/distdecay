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
#' @importFrom jsonlite fromJSON
#' @importFrom Rcpp evalCpp
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @useDynLib distdecay, .registration = TRUE
NULL
