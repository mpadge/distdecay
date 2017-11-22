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
    {
        m <- dd_mi (city)
        m [m < 1e-6] <- 0 # Very low values muck up the log plots
        ylab <- "Mutual Information"
    } else
        m <- dd_cov (city)
    d <- dd_get_distmat (city)

    if (from)
    {
        d <- d [lower.tri (d)]
        m <- m [lower.tri (m)]
    } else
    {
        d <- d [upper.tri (d)]
        m <- m [upper.tri (m)]
    }

    indx <- which (!is.na (m) & !is.na (d) & m > 0)
    if (length (indx) == 0)
        stop ("No or insufficient data available for ", city)
    d <- d [indx]
    m <- m [indx]

    dat <- data.frame (x = d, y = m)

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

    m <- d <- ci <- NULL

    ylab <- "Covariance"
    if (mi)
        ylab <- "Mutual Information"

    message ("Extracting data ... ", appendLF = FALSE)
    for (i in cities)
    {
        if (mi)
            mtemp <- dd_mi (city = i)
        else
            mtemp <- dd_cov (city = i)
        dtemp <- dd_get_distmat (city = i)

        if (from)
        {
            dtemp <- dtemp [lower.tri (dtemp)]
            mtemp <- mtemp [lower.tri (mtemp)]
        } else
        {
            dtemp <- dtemp [upper.tri (dtemp)]
            mtemp <- mtemp [upper.tri (mtemp)]
        }

        indx <- which (!is.na (mtemp) & !is.na (dtemp) & mtemp > 0)
        dtemp <- dtemp [indx]
        mtemp <- mtemp [indx]

        m <- c (m, mtemp)
        d <- c (d, dtemp)
        ci <- c (ci, rep (full_city_name (i), length (mtemp)))
    }

    if (mi)
        m [m < 1e-6] <- 0 # Very low values muck up the log plots


    dat <- data.frame (x = d, y = m, city = ci)
    message ("done\nPreparing plot ...")

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
