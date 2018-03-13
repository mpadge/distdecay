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
    rcpp_clusters (dmat, cmat)
}
