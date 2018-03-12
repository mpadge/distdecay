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
#' @param lower Lower limit (0-1) for distance cutoff used to calculate
#' covariances (see details)
#' @param upper Upper limit (0-1) for distance cutoff used to calculate
#' covariances (see details)
#' @param osm If \code{FALSE}, return straight-line distances, otherwise street
#' network distances.
#'
#' @note The directory from which trip matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}. Covariances
#' can be calculated between stations lying within a defined distance range
#' using the \code{lower} and \code{upper} parameters. For example, to calculate
#' covariances only using the nearest half of all stations, set \code{upper =
#' 0.5}.
#'
#' @export
dd_cov <- function (city, lower = 0, upper = 1, osm = TRUE)
{
    mats <- dd_get_tripdistmats (city, osm = osm)
    if (lower > 0 | upper < 1)
    {
        indx <- dist_thresholds (mats, lower, upper)
        for (i in seq (indx))
            if (length (indx [[i]]) > 0)
                mats$trip [i, indx [[i]]] <- mats$trip [indx [[i]], i] <- 0
    }
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
#' @param lower Lower limit (0-1) for distance cutoff used to calculate
#' covariances (see details)
#' @param upper Upper limit (0-1) for distance cutoff used to calculate
#' covariances (see details)
#' @param osm If \code{FALSE}, return straight-line distances, otherwise street
#' network distances.
#'
#' @note The directory from which trip matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}.
#'
#' @export
dd_mi <- function (city, lower = 0, upper = 1, osm = TRUE)
{
    mats <- dd_get_tripdistmats (city)
    if (lower > 0 | upper < 1)
    {
        indx <- dist_thresholds (mats, lower, upper)
        for (i in seq (indx))
            mats$trips [i, indx [i]] <- mats$trips [indx [i], i] <- 0
    }
    mmat <- rcpp_calc_mi (mats$trip)

    rownames (mmat) <- rownames (mats$trip)
    colnames (mmat) <- colnames (mats$trip)
    mmat [!is.finite (mmat)] <- NA
    return (mmat)
}

#' dist_thresholds
#'
#' Convert relative values of \code{lower} and \code{upper} limits to indices of
#' elements that lie \strong{outside} those limits. These indices can then be
#' used to set all those values to zero.
#'
#' @noRd
dist_thresholds <- function (mats, lower, upper)
{
    apply (mats$dist, 1, function (i)
           {
               dlo <- lower * min (i, na.rm = TRUE)
               dhi <- upper * max (i, na.rm = TRUE)
               as.numeric (which (i < dlo | i > dhi))
           })
}
