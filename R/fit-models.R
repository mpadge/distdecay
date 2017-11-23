#' compare_models
#'
#' Statistical comparisons of 6 different functional forms of distance decay:
#' power-law, Gaussian, exponential, Weibull, Cauchy, and Box-Cox
#'
#' @param city City for which models are to be compared
#' @param from Plot covariance or MI values for trips \strong{from} each
#' station. If \code{FALSE}, plot equivalent values for trips \strong{to} each
#' station.
#' @param mi If \code{TRUE}, plot decay functions for mutual information,
#' otherwise plot covariances.
#' @param plot = TRUE allows visual inspection of individual station results
#'
#' @return a list of data frames, one for each model, containg data frame with
#' rows for each station and columns of [SS, k, b, AIC], where k is the width
#' parameter of the model, and b is the shape parameter (which does not apply to
#' power-law, Gaussian or Cauchy models).
#'
#' @export
dd_compare_models <- function (city = "nyc", from = TRUE, mi = FALSE,
                               plot = FALSE)
{
    ci <- convert_city_name (city)
    dat <- dd_get_vecs (ci, mi = mi)
    d <- dat$d
    y <- dat$n
    if (mi)
        y <- 1 - y

    if (!mi)
        message (city, ": Fitting distance decays to covariances\n")
    else
        message (city, ": Fitting distance decays to mutual information\n")

    # ***** THE FUNCTIONAL FORMS *****
    # For all models, b and k are respective form and width parameters.
    # Covariances generally decay to a slightly negative value, requiring all
    # models to include an additive y0 parameter. This is also necessary for MI,
    # which increases from zero, and so models are fitted to negative values,
    # ultimately decaying to some value << 0.
    mods <- list ()
    # Gaussian (b0 not used)
    mods [[1]] <- function (y, d, a0 = 2 * mean(y), k0, b0)
                tryCatch (nls (y ~ y0 + a * exp (-(d / k)^2), #nolint
                            start = list (a = diff (range(y)), k = k0, y0 = 0)),
                            error = function (e) NULL)
    mods [[2]] <- function (y, d, a0 = 2 * mean(y), k0, b0) # Exponential
                tryCatch (nls (y ~ y0 + a * exp (-(d / k) ^ b), #nolint
                            start = list(a = a0, k = k0, b = 2, y0 = 0)),
                            error = function (e) NULL)
    mods [[3]] <- function (y, d, a0 = 2 * mean(y), k0, b0) # Weibull
                tryCatch (nls (y ~ y0 + a * (b / k) * (d / k) ^ (b - 1) *
                            exp (-(d / k) ^ b), #nolint
                            start = list(a = a0, k = k0, b = b0, y0 = 0)),
                            error = function (e) NULL)
    # Cauchy (b0 not used)
    mods [[4]] <- function (y, d, a0 = 2 * mean(y), k0, b0)
                tryCatch (nls (y ~ y0 + (a / k) * (k ^ 2 / (d ^ 2 + k ^ 2)),
                            start = list(a = a0, k = k0, y0 = 0)),
                            error = function (e) NULL)
    mods [[5]] <- function (y, d, a0 = 2 * mean(y), k0, b0) # Box-Cox
                tryCatch (nls (y ~ y0 + a *
                               exp (-d ^ (2 * b) / (k ^ 2 * b ^ 2)),
                            start = list (a = a0, k = k0, b = b0, y0 = 0)),
                            error = function (e) NULL)
    # ***** END FUNCTIONAL FORMS *****

    results <- data.frame (array (NA, dim = c(6, 4)), stringsAsFactors = FALSE)
    names (results) <- c ("ss", "k", "b", "aic")
    rownames (results) <- c("power", "gauss", "exp", "weibull",
                            "cauchy", "boxcox")

    cols <- c ("black", "red", "red", "blue", "lawngreen", "magenta")
    ltys <- c (1, 1, 2, 1, 1, 1)

    if (plot)
        plot (d, y, pch = 1, col = "orange", log = "xy")
    dfit <- seq(min(d), max(d), length.out = 100)

    # ***** power-law is done separately:
    mod <- lm (log10 (y) ~ log10 (d))
    coeffs <- summary (mod)$coefficients
    yfit <- 10 ^ (coeffs [1] + log10 (d) * coeffs [2])
    results$ss [1] <- mean ( (yfit - y) ^ 2)
    results$k [1] <- coeffs [2]
    results$aic [1] <- AIC (mod)
    if (plot)
    {
        yfit <- 10 ^ (coeffs [1] + log10 (dfit) * coeffs [2])
        lines (dfit, yfit, col = cols [1], lty = ltys [1])
    }

    # Then the remaining 5 models
    for (i in 1:length (mods))
    {
        k0 <- 0
        b0 <- 1
        mod <- NULL
        while (is.null (mod) & k0 < 10)
        {
            k0 <- k0 + 1
            mod <- do.call (mods [[i]], list(y = y, d = d,
                                             a0 = 2 * mean (y), k0, b0))
        }
        if (!is.null (mod))
        {
            yfit <- mean ( (predict (mod) - y) ^ 2)
            coeffs <- summary (mod)$coefficients
            # coeffs are either [a,k,y0] or [a,k,b,y0], where y0 is ignored
            results$ss [i + 1] <- yfit
            results$k [i + 1] <- coeffs [2]
            results$aic [i + 1] <- AIC (mod)
            if (nrow (coeffs) > 3)
                results$b [i + 1] <- summary (mod)$coefficients [3]
            if (plot)
            {
                yfit <- predict (mod, new = data.frame (d = dfit))
                lines (dfit, yfit, col = cols [i + 1],
                       lty = ltys [i + 1], lwd = 2)
            }
        }
    }
    if (plot)
    {
        legend ("topright", lwd = 2, col = cols, lty = ltys,
                bty = "n", legend = c("power", "gauss", "exp",
                                      "weibull", "cauchy", "boxcox"))
    }

    results$k [1] <- -results$k [1] # power-law
    # Sign of Gaussian k is sometimes negative, so
    results$k [2] <- abs (results$k [2])

    # standardise SS vals
    results$ss <- results$ss / min (results$ss)

    cat ("Model\t|\tSS\t\tAIC\t|\tk\t\tb\t|\n")
    cat (c (rep ("-", 73), "\n"), sep = "")
    for (i in seq (nrow (results)))
    {
        ss <- formatC (results$ss [i], format = "f", digits = 4)
        aic <- round (results$aic [i])
        k <- formatC (results$k [i], format = "f", digits = 2)
        b <- ""
        if (!is.na (results$b [i]))
            b <- formatC (results$b [i], format = "f", digits = 2)
        cat (rownames (results) [i])
        if (rownames (results) [i] != "weibull")
            cat ("\t")
        cat ("|\t", ss, "\t", aic, "\t|\t", k, "\t\t")
        if (!is.na (results$b [i]))
            cat (b, "\t|\n")
        else
            cat ("\t|\n")

    }
    cat (c (rep ("-", 73), "\n"), sep = "")

    return (results)
} # end compare.models()


#' dd_fit_expmod
#'
#' Fit a generalised exponential decay model to data from one station
#'
#' @param mats List of distance and trip matrices for a particular city as
#' returned from \code{dd_get_tripdistmats()}.
#' @param from Analyse decay functions for trips \strong{from} each station. If
#' \code{FALSE}, analyse for trips \strong{to} each station.
#' @param i Number of station as row or column of matrices
#' @param plot If \code{TRUE}, plot decay function
#'
#' @return A \code{data.frame} of four values: \code{id}, the ID of the station;
#' \code{k}, the width parameter of the exponential decay; \code{b}, the value
#' of the exponent; and \code{ss}, the standardised sum of squared residuals.
#'
#' @export
dd_fit_expmod <- function (mats, i, from = TRUE, plot = FALSE)
{
    if (from)
    {
        indx <- which (mats$trip [i, ] > 0 & !is.na (mats$d [i, ]) &
                       mats$d [i, ] > 0)
        d <- as.numeric (mats$dist [i, indx])
        y <- as.numeric (mats$trip [i, indx])
    } else
    {
        indx <- which (mats$trip [, i] > 0 & !is.na (mats$d [, i]) &
                       mats$d [, i] > 0)
        d <- as.numeric (mats$dist [indx, i])
        y <- as.numeric (mats$trip [indx, i])
    }
    if (plot)
        plot (d, y, pch = 1, col = "orange", log = "xy")
    # exponential model
    mod <- tryCatch (nls (y ~ a * exp (-(d / k) ^ b), #nolint
                          start = list(a = 10 * mean (y), k = 1, b = 1)),

                     error = function (e) NULL)
    if (plot & !is.null (mod))
    {
        dfit <- seq(min(d), max(d), length.out = 100)
        yfit <- predict (mod, new = data.frame (d = dfit))
        lines (dfit, yfit, col = "red")
    }

    k <- b <- ss <- NA
    if (!is.null (mod))
    {
        coeffs <- summary (mod)$coefficients # a, k, b
        # Re-scale to unit intercept and calculate SS residuals
        ysc <- y / coeffs [1]
        mod <- nls (ysc ~ a * exp (-(d / k) ^ b), #nolint
                    start = list(a = 10 * mean (ysc),
                                 k = coeffs [2], b = coeffs [3]))
        ss <- summary (mod)$residuals
        ss <- sum (ss ^ 2) / length (ss)
        k <- coeffs [2]
        b <- coeffs [3]
    }
    id <- rownames (mats$trip) [i]
    data.frame (id = id, k = k, b = b, ss = ss, stringsAsFactors = FALSE)
}

#' dd_decay_fns
#'
#'
#'
#' @param city City for which decay parameters are to be calculated
#' @param from Plot covariance or MI values for trips \strong{from} each
#' station. If \code{FALSE}, plot equivalent values for trips \strong{to} each
#' station.
#' @param mi If \code{TRUE}, plot decay functions for mutual information,
#' otherwise plot covariances.
#'
#' @return A \code{data.frame} of statistics for both straight-line distances
#' (\code{_str}) and OpenStreetMap network distances (\code{_osm}) for the
#' exponential decay function, exp (-(d / k) ^ b), of:
#' \itemize{
#'  \item \code{k}, the width of the exponential;
#'  \item \code{b}, the exponent; and
#'  \item \code{ss}, a standardised sum of sqaured residuals of the model fit
#' }
#' k-values quantifying the widths of exponential fits to the decay functions.
#'
#' @export
dd_decay_fns <- function (city, from = TRUE)
{
    mats1 <- dd_get_tripdistmats (city = city, osm = TRUE)
    n <- seq (nrow (mats1$trip))
    res1 <- lapply (n, function (i) dd_fit_expmod (mats1, i = i, from = from))
    res1 <- do.call (rbind, res1)

    mats2 <- dd_get_tripdistmats (city = city, osm = FALSE)
    n <- seq (nrow (mats2$trip))
    res2 <- lapply (n, function (i) dd_fit_expmod (mats2, i = i, from = from))
    res2 <- do.call (rbind, res2)

    nms <- intersect (res1$id, res2$id)
    res1 <- res1 [match (nms, res1$id), ]
    res2 <- res2 [match (nms, res2$id), ]

    indx <- which (!is.na (res1$k) & !is.na (res2$k))
    res1 <- res1 [indx, ]
    res2 <- res2 [indx, ]
    names (res1) [2:4] <- paste0 (names (res1) [2:4], "_osm")
    names (res2) [2:4] <- paste0 (names (res2) [2:4], "_str")

    cbind (res1, res2 [, 2:4])
}
