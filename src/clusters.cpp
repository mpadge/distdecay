#include "common.h"
#include "clusters.h"

//' rcpp_clusters
//'
//' Calculate clusters from covariance and distance matrices.
//' 
//' while not all allocated:
//'     i = station with highest covariance
//'     j = corresponding station
//'     if i unallocated:
//'         if j allocated:
//'             cluster (i) <- cluster (j)
//'         else:
//'             allocate i to new cluster
//'     cov (i, j) = 0.0
//'     cmax (i) = max (cov (i, ))
//' 
//' @noRd
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_clusters (arma::mat dmat, arma::mat cmat)
{
    dmat.replace (arma::datum::nan, INFINITE_DOUBLE);
    cmat.replace (arma::datum::nan, 0.0);
    const int n = dmat.n_rows;

    // cmax is the maximal covariance TO each station:
    // max (., 0) is a rowvec representing the COLUMN maxima;
    // max (., 1) is a colvec representing the ROW maxima;
    //const arma::colvec cmax = arma::max (cmat, 1);
    arma::rowvec rmax = arma::max (cmat, 0); // column maxima!

    Rcpp::IntegerVector clusters (n, 0);
    size_t cl_num = 1; // start at cluster = 1
    size_t nout = n;

    //arma::colvec junk = cmat.col (10);
    // -> rmax (10) is then equal to junk.max ()

    nout--;
    while (nout > 0)
    {
        Rcpp::checkUserInterrupt ();
        arma::uword imax = rmax.index_max ();
        arma::colvec coli = cmat.col (imax);
        arma::uword jmax = coli.index_max ();
        if (clusters (imax) == 0)
        {
            if (clusters (jmax) > 0)
                clusters (imax) = clusters (jmax);
            else
                clusters (imax) = cl_num++;
            nout--;
        }
        // cmat.max () is then cmat (jmax, imag)
        cmat (jmax, imax) = 0.0;
        coli = cmat.col (imax);
        rmax (imax) = coli.max ();
    }

    return clusters;
}

