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
#' @importFrom graphics legend lines locator title
#' @importFrom jsonlite fromJSON
#' @importFrom Rcpp evalCpp
#' @importFrom stats AIC lm median nls predict sd t.test
#' @useDynLib distdecay, .registration = TRUE
NULL

#' distmats
#' 
#' Station-to-station distance matrices between bicycle hire stations for all
#' cities of the \pkg{bikedata} package, calculated with the \code{bike_distmat}
#' function. These matrices have been reconciled against the corresponding
#' \link{tripmats} matrices with the function
#' \code{bikedata::bike_match_matrices}.
#' 
#' @name distmats
#' @docType data
NULL

#' tripmats
#' 
#' Station-to-station trip matrices between bicycle hire stations for all cities
#' of the \pkg{bikedata} package, calculated with the \code{bike_tripmat}
#' function. These matrices have been reconciled against the corresponding
#' \link{distmats} matrices with the function
#' \code{bikedata::bike_match_matrices}.
#' 
#' @name tripmats
#' @docType data
NULL
