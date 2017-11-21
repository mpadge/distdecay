#include "Matrix-fns.h"




/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             CALC_COV                               **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

//' rcpp_calc_cov
//'
//' Calculate pairwise covariance matrix between input trip matrix. Upper
//' diagonal is between all trips **from** i and j; lower diagonal holds
//' covariances between all trips **to** j and i. This can't be done easily in
//' Armadillo because the zero entries need to be removed, and arma's inbuilt
//' cov function doesn't ignore zeros, and any nan or inf values generate
//' resultant NA covariances for the whole row.
//' 
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_calc_cov (arma::mat tmat)
{
    //arma::mat acov = cov (tmat); // does not work

    tmat.replace (arma::datum::nan, 0.0);
    const int n = tmat.n_rows;

    Rcpp::NumericMatrix covmat (n, n);

    for (arma::uword i=0; i < (tmat.n_rows - 1); ++i)
    {
        arma::rowvec xrow = tmat.row (i);
        const double sx = arma::sum (xrow);

        for (arma::uword j=(i + 1); j < tmat.n_rows; ++j)
        {
            arma::rowvec yrow = tmat.row (j);
            arma::rowvec xy = xrow % yrow; // arma element-wise multiplication
            const double nnz = static_cast <double> (
                    arma::size (arma::find (xy > 0.0)) [0]);
            const double sy = arma::sum (yrow),
                  sxy = arma::sum (xy);

            covmat (i, j) = (sxy - sx * sy / nnz) / nnz;
        }
    }

    // Then repeat for columns
    tmat = tmat.t ();
    for (arma::uword i=0; i < (tmat.n_rows - 1); ++i)
    {
        arma::rowvec xrow = tmat.row (i);
        const double sx = arma::sum (xrow);

        for (arma::uword j=(i + 1); j < tmat.n_rows; ++j)
        {
            arma::rowvec yrow = tmat.row (j);
            arma::rowvec xy = xrow % yrow;
            const double nnz = static_cast <double> (
                    arma::size (arma::find (xy > 0.0)) [0]);
            const double sy = arma::sum (yrow),
                  sxy = arma::sum (xy);

            covmat (j, i) = (sxy - sx * sy / nnz) / nnz;
        }
    }

    return covmat;
}
