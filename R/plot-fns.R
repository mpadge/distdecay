#' dd_plot
#'
#' Plot distance decay functions.
#'
#' @param city City for which distance decay functions are to be plotted.
#' @param from Plot covariance or MI values for trips \strong{from} each
#' station. If \code{FALSE}, plot equivalent values for trips \strong{to} each
#' station.
#' @param mi If \code{TRUE}, plot decay functions for mutual information,
#' otherwise plot covariances.
#' @param smoother if \code{TRUE}, overlay a loess-smoothed line. (This will
#' generally take much longer to plot.)
#'
#' @export
dd_plot <- function (city, from = TRUE, mi = FALSE, smoother = TRUE)
{
    ylab <- "Covariance"
    if (mi)
        ylab <- "Mutual Information"

    dat <- dd_get_vecs (city = city)
    dat$n [dat$n < 1e-6] <- 0 # Very low values muck up the log plots

    x <- y <- ..count.. <- NULL # suppress R CMD check messges
    g <- ggplot2::ggplot (dat, ggplot2::aes (x = x, y = y)) +
        ggplot2::stat_binhex (ggplot2::aes (fill = log (..count..)))#nolint
    if (smoother)
        g <- g + ggplot2::geom_smooth (method = "loess")
    g <- g + ggplot2::scale_x_log10 () +
        ggplot2::scale_y_log10 () +
        ggplot2::xlab ("Distance") +
        ggplot2::ylab (ylab) +
        ggplot2::ggtitle (full_city_name (city)) +
        ggplot2::scale_fill_gradientn(colours = c("seagreen", "goldenrod1"),
                                      name = "Frequency", na.value = NA) +
        ggplot2::guides (fill = FALSE)

    print (g)
    invisible (g)
}

#' dd_plot_all
#'
#' Plot distance decay functions for all cities
#'
#' @param from Plot covariance or MI values for trips \strong{from} each
#' station. If \code{FALSE}, plot equivalent values for trips \strong{to} each
#' station.
#' @param mi If \code{TRUE}, plot decay functions for mutual information,
#' otherwise plot covariances.
#' @param smoother if \code{TRUE}, overlay a loess-smoothed line. (This will
#' generally take much longer to plot.)
#'
#' @export
dd_plot_all <- function (from = TRUE, mi = FALSE, smoother = FALSE)
{
    cities <- c ("la", "bo", "ch", "dc", "ny", "lo") # no pa data at present

    n <- d <- ci <- NULL

    ylab <- "Covariance"
    if (mi)
        ylab <- "Mutual Information"

    message ("Extracting data ... ", appendLF = FALSE)
    for (i in cities)
    {
        dat <- dd_get_vecs (city = i)
        dat$n [dat$n < 1e-6] <- 0 # Very low values muck up the log plots
        n <- c (n, dat$n)
        d <- c (d, dat$d)
        ci <- c (ci, rep (full_city_name (i), nrow (dat)))
    }

    if (mi)
        n [n < 1e-6] <- 0 # Very low values muck up the log plots


    dat <- data.frame (x = d, y = n, city = ci)
    message ("done\nThere are a total of ", nrow (dat), " data points")

    x <- y <- ..count.. <- NULL # suppress R CMD check messges
    g <- ggplot2::ggplot (dat, ggplot2::aes (x = x, y = y)) +
        ggplot2::stat_binhex (ggplot2::aes (fill = log (..count..)))#nolint
    if (smoother)
        g <- g + ggplot2::geom_smooth (method = "loess")
    g <- g + ggplot2::scale_x_log10 () +
        ggplot2::scale_y_log10 () +
        ggplot2::xlab ("Distance") +
        ggplot2::ylab (ylab) +
        ggplot2::scale_fill_gradientn(colours = c("seagreen", "goldenrod1"),
                                      name = "Frequency", na.value = NA) +
        ggplot2::guides (fill = FALSE) +
        ggplot2::facet_wrap (~ city, ncol = 3)

    print (g)
    invisible (g)
}

#' dd_cluster_hulls
#'
#' Calculate convex hulls around clusters, mostly cribbed from
#' osmplotr/R/add-osm-groups.R
#'
#' @param stns tibble of station coordinates plus cluster numbers
#' @return tibble of (id, x, y), where the coordinates trace the convex hulls
#' for each cluster id
#' @noRd
dd_cluster_hulls <- function (stns)
{
    if (!"cl" %in% names (stns))
        stop ("stns has no cl column to identify clusters")

    bdry <- list ()
    for (i in seq (unique (stns$cl)))
    {
        indx <- which (stns$cl == i) # col = group membership
        if (length (indx) > 1)
        {
            x <- stns$longitude [indx]
            y <- stns$latitude [indx]
            indx <- which (!duplicated (cbind (x, y)))
            x <- x [indx]
            y <- y [indx]
            xy2 <- spatstat::ppp (x, y, xrange = range (x),
                                  yrange = range (y))
            ch <- spatstat::convexhull (xy2)
            bdry [[i]] <- cbind (ch$bdry[[1]]$x, ch$bdry[[1]]$y)
        }
        bdry [[i]] <- cbind (i, bdry [[i]])
    }
    bdry <- data.frame (do.call (rbind, bdry))
    names (bdry) <- c ("id", "x", "y")
    return (bdry)
}

#' dd_plot_clusters
#'
#' Plot the results of the \link{dd_cov_clusters} function
#'
#' @param cl Tibble of station data and cluster numbers obtained from
#' \link{dd_cluster_stations}
#' @param interactive If \code{FALSE}, produce a static \pkg{ggplot2} object,
#' otherwise an interactive map via \pkg{mapview}.
#'
#' @export
dd_plot_clusters <- function (cl, interactive = TRUE)
{
    # suppress no visible binding notes:
    stns <- x <- y <- id <- longitude <- latitude <- . <- NULL

    bdry <- dd_cluster_hulls (cl)

    cols <- rainbow (length (unique (stns$cl)))
    if (!interactive)
    {
        hull_aes <- ggplot2::aes (x = x, y = y, group = id)
        hull_width <- 0.5
        m <- ggplot2::ggplot (stns, ggplot2::aes (x = longitude,
                                                  y = latitude,
                                                  colour = cols [cl])) +
            ggplot2::geom_point (show.legend = FALSE) +
            ggplot2::geom_polygon (data = bdry,
                                   mapping = hull_aes,
                                   colour = cols [bdry$id],
                                   fill = "transparent",
                                   size = hull_width) +
            ggthemes::theme_solarized ()
    } else
    {
        xy <- stns %>%
            select (longitude, latitude) %>%
            as.matrix () %>%
            split (., seq (nrow (.))) %>%
            lapply (sf::st_point) %>%
            sf::st_sfc () %>%
            sf::st_sf ()
        sf::st_crs (xy) <- 4326
        xy$col <- cols [stns$cl]
        # hulls to sf:
        grps <- sort (unique (bdry$id))
        polys <- list ()
        for (g in grps)
        {
            pg <- bdry %>% filter (id == g) %>% select (x, y) %>% as.matrix ()
            pg <- rbind (pg, pg [1, ])
            polys <- c (polys, list (sf::st_polygon (list (pg))))
        }
        polys <- sf::st_sfc (polys) %>% sf::st_sf ()
        sf::st_crs (polys) <- 4326
        polys$col <- cols

        m <- mapview::mapview (polys, color = polys$col,
                 col.regions = polys$col, alpha.regions = 0.1) %>%
                mapview::addFeatures (xy, color = xy$col, radius = 2)
    }

    print (m)
    invisible (m)
}

