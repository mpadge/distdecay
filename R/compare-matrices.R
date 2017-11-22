#' dd_cov
#'
#' Calculate station-to-station covariance matrix for a given city. Upper
#' diagonal of matrix is between all trips \strong{from} i and j, so
#' \code{dd_cov()[i, j]} holds the covariance between all trips \strong{from}
#' station \code{i} to all other stations and \strong{from} station \code{j} to
#' all other stations. Analogously, the lower diagonal (\code{dd_cov()[j, i]})
#' holds covariances between all trips \strong{to} i and j.
#'
#' @param city City for which covariance matrix is to be extracted
#'
#' @note The directory from which trip matrices are loaded can be retrieveed
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

#' dd_mi
#'
#' Calculate station-to-station mutual information matrix for a given city.
#' Upper diagonal of matrix is between all trips \strong{from} i and j, so
#' \code{dd_cov()[i, j]} holds the covariance between all trips \strong{from}
#' station \code{i} to all other stations and \strong{from} station \code{j} to
#' all other stations. Analogously, the lower diagonal (\code{dd_cov()[j, i]})
#' holds covariances between all trips \strong{to} i and j.
#'
#' @param city City for which mutual information matrix is to be extracted
#'
#' @note The directory from which trip matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}.
#'
#' @export
dd_mi <- function (city)
{
    dm <- dd_get_distmat (city)
    tm <- dd_get_tripmat (city)
    mats <- bikedata::bike_match_matrices (dm, tm)
    dm <- mats$dist
    tm <- mats$trip

    mmat <- rcpp_calc_mi (tm)
    rownames (mmat) <- rownames (tm)
    colnames (mmat) <- colnames (tm)
    mmat [!is.finite (mmat)] <- NA
    return (mmat)
}

