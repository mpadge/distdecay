#' dd_get_distmat
#'
#' Load pre-calculated distance matrix for a given city
#'
#' @param city City for which distance matrix is to be extracted
#' @return Square matrix of distances between all stations for nominated
#' city.
#'
#' @note The directory from which distance matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}.
#'
#' @export
dd_get_distmat <- function (city)
{
    ci <- convert_city_name (city)

    files <- list.files (dd_get_data_dir ())
    f <- file.path (dd_get_data_dir (), files [grepl ("dist", files)])
    load (f)
    obj <- ls () [grep ("distmat", ls ())]

    if (!ci %in% names (get (obj)))
        stop (city, " not in distance matrix data")

    get (obj)[[ci]]
}

#' dd_get_tripmat
#'
#' Load pre-calculated trip matrix for a given city
#'
#' @param city City for which trip matrix is to be extracted
#' @return Square matrix of numbers of trips between all stations for nominated
#' city.
#'
#' @note The directory from which trip matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}.
#'
#' @export
dd_get_tripmat <- function (city)
{
    ci <- convert_city_name (city)

    files <- list.files (dd_get_data_dir ())
    f <- file.path (dd_get_data_dir (), files [grepl ("trip", files)])
    load (f)
    obj <- ls () [grep ("tripmat", ls ())]

    if (!ci %in% names (get (obj)))
        stop (city, " not in trip matrix data")

    get (obj)[[ci]]
}

#' dd_get_tripdistsmatS
#'
#' Load pre-calcualted trip \strong{and} distance matrices for a given city,
#' ensuring that all row and column names match.
#'
#' @param city City for which matrices are to be extracted
#' @return List of two square matrices of (i) numbers of trips (\code{$trip})
#' and (ii) distances (\code{$dist}) between all stations for nominated city.
#'
#' @note The directory from which trip matrices are loaded can be retrieveed
#' with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}.
#'
#' @export
dd_get_tripdistmats <- function (city)
{
    tm <- dd_get_tripmat (city)
    dm <- dd_get_distmat (city)
    bikedata::bike_match_matrices (dm, tm)
}


#' dd_get_vecs
#'
#' Get data.frame of distances (\code{d}) and numbers of trips (\code{n}) for a
#' given city
#' @noRd
dd_get_vecs <- function (city, from = TRUE, mi = FALSE)
{
    if (mi)
        n <- dd_mi (city)
    else
        n <- dd_cov (city)
    d <- dd_get_distmat (city)

    if (from)
    {
        d <- d [lower.tri (d)]
        n <- n [lower.tri (n)]
    } else
    {
        d <- d [upper.tri (d)]
        n <- n [upper.tri (n)]
    }

    indx <- which (!is.na (n) & !is.na (d) & n > 0)
    if (length (indx) == 0)
        stop ("No or insufficient data available for ", city)
    data.frame (d = d [indx], n = n [indx])
}

#' Calculate distance matrix between pairs of points using the google API
#'
#' @param xy A two-column matrix of lon-lat coordinates
#' @param dmat If submitted, any missing values not returned in previous queries
#' are filled.
#' @param g_units The units of measurement requested from the google API
#' @param g_mode The mode of transport requested from the google API
#' 
#' @export
#'
#' @note the google server frequently returns no values, so this produces a
#' matrix with lots of missing values.
#'
#' @examples
#' \dontrun{
#' test_fn ()
#' }
distmat_g <-  function (xy, dmat, g_units = "metric", g_mode = "bicycling")
{
    base_url <- paste0 ("https://maps.googleapis.com/maps/api/",
                        "distancematrix/json?units=", g_units)

    nst <- nrow (xy)
    if (missing (dmat))
        dmat <- array (NA, dim = rep (nst, 2))

    nmax <- 30 # maximal number of distances per request; approx <= 37
    gr_indx <- group_index (seq (nst), nmax = nmax)

    # Getting google responses is the slowest bit by miles, so loops okay here
    pb <- txtProgressBar (max = 1, style = 3)
    for (i in seq (nst))
    {
        from <- paste (xy [i, 2], xy [i, 1], sep = ",")
        for (j in seq (gr_indx))
        {
            if (any (is.na (dmat [i, gr_indx [[j]] ])))
            {
                to <- paste (xy [gr_indx [[j]], 2],
                             xy [gr_indx [[j]], 1], sep = ",")
                to <- paste0 (to, collapse = "|")

                url_travel <- paste0 (base_url, "&origins=", from,
                                      "&destinations=", to, "&mode=", g_mode)
                url <- utils::URLencode(url_travel, repeated = FALSE,
                                        reserved = FALSE)
                obj <- jsonlite::fromJSON(url)
                if (obj$status == "OK")
                {
                    dists <- obj$rows$elements [[1]]$distance$value
                    if (length (dists) > 0)
                        dmat [i, gr_indx [[j]] ] <- dists
                }
            }
        }
        setTxtProgressBar(pb, i / nst)
    }
    close (pb)

    return (dmat)
}

group_index <- function (x, nmax = 30)
{
    if (length (x) <= nmax)
        group_index <- seq (x)
    else
    {
        ngr <- ceiling (length (x) / nmax)
        group_index <- by (seq (x), cut (seq (x), ngr), FUN = I)
    }

    return (group_index)
}
