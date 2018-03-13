#' dd_cov_clusters
#'
#' Cluster points which have covariances above values expected from a neutral
#' model.
#'
#' @param city City for which models are to be compared
#'
#' @return nuffin
#'
#' @export
dd_cov_clusters <- function (city = "nyc")
{
    chk_city (city)

    cmat <- dd_cov (city = city) - dd_cov (city = city, neutral = TRUE)
    dmat <- distmats [[city]]
    cl <- rcpp_clusters (dmat, cmat)
    names (cl) <- rownames (cmat)
    return (cl)
}

#' dd_cluster_stations
#'
#' Convert the results of \link{dd_cov_clusters} to a \pkg{tibble} including
#' station IDs, longitudes, and latitudes.
#'
#' @param cl Vector of cluster numbers obtained from \link{dd_cov_clusters}
#' @param city City for which clusters were obtained
#' @param stns A \pkg{tibble} of station data, including \code{stn_id},
#' \code{longitude}, and \code{latitude}, typically obtained from
#' \code{bikedata::bike_stations(city)}.
#' @param min_size Lower threshold above which to plot clusters
#'
#' @return A \pkg{tibble} of (\code{stn_id}, \code{longitude}, \code{latitude},
#' and \code{cl}), where the latter denotes the cluster number.
#' @export
dd_cluster_stations <- function (cl, city, stns, min_size = 3)
{
    cl <- tibble::tibble (stn_id = names (cl), cl = cl)
    stns %<>% dplyr::select (stn_id, longitude, latitude) %>%
        dplyr::distinct (stn_id, .keep_all = TRUE) %>%
        dplyr::inner_join (cl, by = "stn_id")
    # set IDs of small clusters to NA
    tb <- table (stns$cl)
    cl_small <- as.numeric (names (tb) [which (tb < min_size)])
    stns$cl [stns$cl %in% cl_small] <- NA
    stns %<>% dplyr::filter (!is.na (cl)) %>%
        dplyr::mutate (cl = as.integer (factor (cl)))
    # boston stns have a lat-lon outlier which has to be filtered out
    if (city == "bo")
        stns %<>% dplyr::filter (longitude > -72 & latitude < 43)
    return (stns)
}
