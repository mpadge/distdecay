#' dd_cov
#'
#' Calculate station-to-station covariance matrix for a given city
#'
#' @param city City for which distance matrix is to be extracted
#'
#' @note The directory from which distance matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}.
#'
#' @export
dd_cov <- function (city)
{
    dm <- dd_get_distmat (city)
    tm <- dd_get_tripmat (city)
    mats <- bikedata::bike_match_matrices (dm, tm)
    dm <- mats$dist
    tm <- mats$trip

    cmat <- rcpp_calc_cov (tm)
    rownames (cmat) <- rownames (tm)
    colnames (cmat) <- colnames (tm)
    cmat [!is.finite (cmat)] <- NA
    return (cmat)
}

