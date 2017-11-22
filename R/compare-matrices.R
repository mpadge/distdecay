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
    mats <- dd_get_tripdistmats (city)
    cmat <- rcpp_calc_cov (mats$trip)

    rownames (cmat) <- rownames (mats$trip)
    colnames (cmat) <- colnames (mats$trip)
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
    mats <- dd_get_tripdistmats (city)
    mmat <- rcpp_calc_mi (mats$trip)

    rownames (mmat) <- rownames (mats$trip)
    colnames (mmat) <- colnames (mats$trip)
    mmat [!is.finite (mmat)] <- NA
    return (mmat)
}
