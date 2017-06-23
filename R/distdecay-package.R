#' Calcalate distance decay functions based on (dis)similarity matrices
#'
#' Imports OpenStreetMap (OSM) data into R as either 'sf' or 'sp' objects.  OSM
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
#' @importFrom jsonlite fromJSON
#' @importFrom utils setTxtProgressBar txtProgressBar
NULL
