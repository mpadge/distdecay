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
#' @param neutral If \code{TRUE}, return a covariance matrix corresponding to a
#' neutral spatial interaction model rather than the observed trip matrix. These
#' neutral covariance matrices can be compared with observed ones to reveal
#' station pairs with higher-than-expected covariances.
#' @param k For neutral covariance matrices, the exponential decay parameter of
#' the spatial interaction model. If not specified, the value is determined by
#' maximising fit between neutral and observed models.
#'
#' @note The directory from which trip matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}. Covariances
#' can be calculated between stations lying within a defined distance range
#' using the \code{lower} and \code{upper} parameters. For example, to calculate
#' covariances only using the nearest half of all stations, set \code{upper =
#' 0.5}.
#'
#' @export
dd_cov <- function (city, lower = 0, upper = 1, osm = TRUE,
                    neutral = FALSE, k = NULL)
{
    chk_city (city)

    distmat <- distmats [[city]]
    tripmat <- tripmats [[city]]
    if (neutral)
        tripmat <- neutral_trips (distmat, tripmat, k)

    tripmat [tripmat == 0] <- NA

    if (lower > 0 | upper < 1)
    {
        indx <- dist_thresholds (distmat, lower, upper)
        for (i in seq (indx))
            if (length (indx [[i]]) > 0)
                tripmat [i, indx [[i]]] <- tripmat [indx [[i]], i] <- 0
    }

    cmat <- rcpp_calc_cov (tripmat)

    rownames (cmat) <- rownames (tripmat)
    colnames (cmat) <- colnames (tripmat)
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
dist_thresholds <- function (distmat, lower, upper)
{
    apply (distmat, 1, function (i)
           {
               dlo <- lower * min (i, na.rm = TRUE)
               dhi <- upper * max (i, na.rm = TRUE)
               as.numeric (which (i < dlo | i > dhi))
           })
}

#' neutral_trips
#'
#' Generate a neutral trip matrix by fitting a spatial interaction model. If no
#' distance decay parameter, \code{k}, is given, the value is derived by
#' maximising the fit of the spatial interaction model to the observed trip
#' matrix.
#'
#' @param distmat A station-to-station distance matrix, generally obtained from
#' internal \link{distmats} data
#' @param tripmat A station-to-station trip matrix, generally obtained from
#' internal \link{tripmats} data
#' @param k Value of exponential decay used in spatial interaction model; if not
#' specified, value is determined by maximising fit between neutral and observed
#' models.
#'
#' @return A matrix equivalent to \code{tripmat}, but in which values are
#' generated by fitting a spatial interaction model.
#'
#' @export
neutral_trips <- function (distmat, tripmat, k = NULL)
{
    if (is.null (k))
        k <- fit_si (distmat, tripmat)

    n <- dim (tripmat) [1]
    nt <- sum (tripmat)
    n_from <- rowSums (tripmat)
    n_to <- colSums (tripmat)
    n_from_mat <- array (n_from, dim = c (n, n))
    n_to_mat <- t (array (n_to, dim = c (n, n)))

    n_si <- array (n_to, dim = c (n, n)) * exp (-distmat / k)
    cs <- t (array (colSums (n_si, na.rm = TRUE), dim = c (n, n)))
    n_si <- n_si * n_from_mat / cs

    rownames (n_si) <- colnames (n_si) <- rownames (tripmat)
    attr (n_si, "variable") <- "numtrips"
    attr (n_si, "k") <- k
    return (n_si)
}


#' fit_si
#'
#' Fit a spatial interaction model to distance and trip matrices, and return the
#' parameter of the exponential decay yielding the lower squared residual error
#' @noRd
fit_si <- function (distmat, tripmat)
{
    n <- dim (tripmat) [1]
    nt <- sum (tripmat)
    n_from <- rowSums (tripmat)
    n_to <- colSums (tripmat)
    n_from_mat <- array (n_from, dim = c (n, n))
    n_to_mat <- t (array (n_to, dim = c (n, n)))

    fitk <- function (k)
    {
        n_si <- array (n_to, dim = c (n, n)) * exp (-distmat / k)
        cs <- t (array (colSums (n_si, na.rm = TRUE), dim = c (n, n)))
        n_si <- n_si * n_from_mat / cs
        x <- as.vector (log10 (n_si))
        y <- as.vector (log10 (tripmat))
        x [!is.finite (x)] <- NA
        y [!is.finite (y)] <- NA
        mean (summary (lm (y ~ x))$residuals ^ 2)
    }
    optimise (fitk, lower = 0.01, upper = 3, maximum = FALSE, tol = 0.01)$objective
}
