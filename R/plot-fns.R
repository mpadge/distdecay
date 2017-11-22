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
        dat <- dd_get_vecs (city = city)
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
