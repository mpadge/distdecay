#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _distdecay_rcpp_calc_cov(SEXP);
extern SEXP _distdecay_rcpp_calc_mi(SEXP);
extern SEXP _distdecay_rcpp_clusters(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_distdecay_rcpp_calc_cov", (DL_FUNC) &_distdecay_rcpp_calc_cov, 1},
    {"_distdecay_rcpp_calc_mi",  (DL_FUNC) &_distdecay_rcpp_calc_mi,  1},
    {"_distdecay_rcpp_clusters", (DL_FUNC) &_distdecay_rcpp_clusters, 2},
    {NULL, NULL, 0}
};

void R_init_distdecay(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
