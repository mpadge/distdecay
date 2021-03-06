#pragma once

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Standardise trips to unit sum so covariances do not depend on absolute
// numbers of trips
const bool _standardise = true;

Rcpp::NumericMatrix rcpp_calc_cov (arma::mat tmat);
Rcpp::NumericMatrix rcpp_calc_mi (arma::mat tmat);
