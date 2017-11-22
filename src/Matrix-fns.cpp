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
//' @noRd
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


/************************************************************************
 ************************************************************************
 **                                                                    **
 **                             CALC_MI                                **
 **                                                                    **
 ************************************************************************
 ************************************************************************/

//' rcpp_calc_mi
//'
//' Calculate pairwise Mutual Information matrix between input trip matrix.
//' Upper diagonal is between all trips **from** i and j; lower diagonal holds
//' covariances between all trips **to** j and i. The zero values can simply be
//' ignored here because all marginal and joint distributions are normalised by
//' total sums, which are unaffected by zeros.
//' 
//' In R terms, NI between x and y is calculated from
//' pxy <- cbind (x, y) / sum (x + y) # joint density
//' px <- rowSums (pxy) # marginal densities
//' py <- colSums (pxy)
//' fnull <- px %o% py # independent null model
//' xy <- ifelse (pxy > 0, log2 (pxy / fnull), 0)
//' mi <- sum (pxy * xy)
//' 
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_calc_mi (arma::mat tmat)
{
    tmat.replace (arma::datum::nan, 0.0);
    const int n = tmat.n_rows;

    Rcpp::NumericMatrix mimat (n, n);

    for (arma::uword i=0; i < (tmat.n_rows - 1); ++i)
    {
        arma::colvec xcol = tmat.row (i).t ();

        for (arma::uword j=(i + 1); j < tmat.n_rows; ++j)
        {
            arma::colvec ycol = tmat.row (j).t ();

            double sxy = arma::sum (xcol + ycol);

            arma::colvec xcol2 = xcol / sxy;
            ycol = ycol / sxy;
            arma::mat pxy (xcol.n_elem, 2);
            pxy.col (0) = xcol2;
            pxy.col (1) = ycol;

            // The arma matrix product is a colvec times a rowvec
            arma::colvec px = xcol2 + ycol;
            arma::rowvec py = {arma::sum (xcol2), arma::sum (ycol)};
            arma::mat fnull = px * py; // null model

            arma::uvec indx = arma::find (pxy > 0.0);
            arma::mat xy (xcol.n_elem, 2, arma::fill::zeros);
            xy.elem (indx) = log2 (pxy.elem (indx) / fnull.elem (indx));

            mimat (i, j) = arma::sum (arma::sum (pxy % xy, 0));
        }
    }

    // Then repeat for columns
    for (arma::uword i=0; i < (tmat.n_rows - 1); ++i)
    {
        arma::colvec xcol = tmat.col (i);

        for (arma::uword j=(i + 1); j < tmat.n_rows; ++j)
        {
            arma::colvec ycol = tmat.col (j);

            double sxy = arma::sum (xcol + ycol);

            arma::colvec xcol2 = xcol / sxy;
            ycol = ycol / sxy;
            arma::mat pxy (xcol.n_elem, 2);
            pxy.col (0) = xcol2;
            pxy.col (1) = ycol;

            // The arma matrix product is a colvec times a rowvec
            arma::colvec px = xcol2 + ycol;
            arma::rowvec py = {arma::sum (xcol2), arma::sum (ycol)};
            arma::mat fnull = px * py; // null model

            arma::uvec indx = arma::find (pxy > 0.0);
            arma::mat xy (xcol.n_elem, 2, arma::fill::zeros);
            xy.elem (indx) = log2 (pxy.elem (indx) / fnull.elem (indx));

            mimat (j, i) = arma::sum (arma::sum (pxy % xy, 0));
        }
    }

    return mimat;
}
