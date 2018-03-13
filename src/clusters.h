#pragma once

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

Rcpp::IntegerVector rcpp_clusters (arma::mat dmat, arma::mat cmat);
