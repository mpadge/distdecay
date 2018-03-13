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
    dat <- dd_get_vecs (ci, from = from, mi = mi)
    d <- dat$d
    y <- dat$n
    if (mi)
        y <- 1 - y
    else
        d [d < 0] <- 0

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


#' dd_fit_stations
#'
#' Fit a generalised exponential decay model to data from one station
#'
#' @param city City for which model is to be fitted
#' @param from Analyse decay functions for trips \strong{from} each station. If
#' \code{FALSE}, analyse for trips \strong{to} each station.
#' @param lower Lower limit (0-1) for distance cutoff used to calculate
#' covariances (see details)
#' @param upper Upper limit (0-1) for distance cutoff used to calculate
#' covariances (see details)
#' @param mi If \code{TRUE}, fit decay functions for mutual information,
#' otherwise fit covariances.
#' @param expmod If \code{TRUE}, fit exponential decay models, otherwise fit
#' power-law decays.
#' @param osm If \code{FALSE}, use straight-line distances for distance decay
#' parameters, otherwise street network distances.
#' @param plot If \code{TRUE}, plot decay function
#'
#' @return A \code{data.frame} of four values: \code{id}, the ID of the station;
#' \code{k}, the width parameter of the exponential decay; \code{b}, the value
#' of the exponent; and \code{ss}, the standardised sum of squared residuals.
#'
#' @note Power-law fits (with \code{expmod = FALSE}) are not reliable at all.
#' The decays really are not power-laws, so this ought not be used.
#'
#' @export
dd_fit_stations <- function (city, from = TRUE, lower = 0, upper = 1,
                             mi = FALSE, expmod = TRUE, osm = TRUE,
                             plot = FALSE)
{
    ci <- convert_city_name (city)
    if (mi)
        cv <- 1 - dd_mi (city = ci, lower = lower, upper = upper)
    else
        cv <- dd_cov (city = ci, lower = lower, upper = upper)

    cv [cv < 0] <- NA
    #dists <- dd_get_distmat (city = ci, osm = osm)
    dists <- distmats [[city]] # TODO: Re-implement osm option there!
    if (!identical (rownames (dists), rownames (cv))) # generally for !osm
    {
        nms <- intersect (rownames (dists), rownames (cv))
        indx <- match (nms, rownames (dists))
        dists <- dists [indx, indx]
        indx <- match (nms, rownames (cv))
        cv <- cv [indx, indx]
    }

    if (from)
        cv [upper.tri (cv)] <- NA
    else
        cv [lower.tri (cv)] <- NA

    id <- k <- b <- ss <- rep (NA, nrow (dists))
    for (i in seq (nrow (dists)))
    {
        d <- c (dists [i, ], dists [, i])
        y <- c (cv [i, ], cv [, i])
        indx <- which (d > 0 & !is.na (d) & !is.na (y) & y > 0)
        d <- d [indx]
        y <- y [indx]

        if (length (y) > 1)
        {
            if (plot)
                plot (d, y, pch = 1, col = "orange", log = "xy")

            if (expmod)
            {
                # exponential model
                mod <- tryCatch (nls (y ~ a * exp (-(d / k) ^ 1), #nolint
                                      start = list(a = 2 * mean (y), k = 1)),
                                 error = function (e) NULL)
                if (!is.null (mod))
                {
                    coeffs <- summary (mod)$coefficients
                    mod <- tryCatch (nls (y ~ a * exp (-(d / k) ^ b), #nolint
                                          start = list(a = coeffs [1],
                                                       k = coeffs [2], b = 2)),
                                     error = function (e) NULL)
                }
            } else
            {
                # statistically invalid linear fit to log-scaled data!
                yl <- log (y) #nolint
                dl <- log (d) #nolint
                mod <- lm (yl ~ dl)
                km <- 10 ^ summary (mod)$coefficients [2]
                mod <- tryCatch (nls (y ~ a * d ^ (-k),
                                      start = list(a = max (y), k = km)),
                                 error = function (e) NULL)
            }
            if (plot & !is.null (mod))
            {
                dfit <- seq(min(d), max(d), length.out = 100)
                yfit <- predict (mod, new = data.frame (d = dfit))
                lines (dfit, yfit, col = "red")
                title (main = paste (i, ": ", rownames (dists) [i], "; k = ",
                       formatC (summary (mod)$coefficients [2],
                                format = "f", digits = 2)))
                loc <- locator (n = 1) #nolint
            }

            if (!is.null (mod))
            {
                coeffs <- summary (mod)$coefficients # a, k, b
                # Re-scale to unit intercept and calculate SS residuals
                ysc <- y / coeffs [1]
                if (expmod)
                    mod <- nls (ysc ~ a * exp (-(d / k) ^ b), #nolint
                                start = list(a = 10 * mean (ysc),
                                             k = coeffs [2], b = coeffs [3]))
                else
                    mod <- nls (y ~ a * d ^ (-k),
                                start = list(a = coeffs [1], k = coeffs [2]))
                ssi <- summary (mod)$residuals
                ss [i] <- sum (ssi ^ 2) / length (ssi)
                k [i] <- coeffs [2]
                if (expmod)
                    b [i] <- coeffs [3]
            }
        } # end if length (y) > 1
        id [i] <- rownames (cv) [i]
    }

    data.frame (id = id, k = k, b = b, ss = ss, stringsAsFactors = FALSE)
}

#' dd_decay_fns
#'
#' @param city City for which decay parameters are to be calculated
#' @param from Plot covariance or MI values for trips \strong{from} each
#' station. If \code{FALSE}, plot equivalent values for trips \strong{to} each
#' station.
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
    # TODO: This no longer works at all! Fix by re-implementing dd_fit_stations
    # for OSM = T/F.
    chk_city (city)
    #mats1 <- dd_get_tripdistmats (city = city, osm = TRUE)
    tmat <- tripmats [[city]]
    n <- seq (nrow (tmat))
    #res1 <- lapply (n, function (i) dd_fit_stations (city, i = i, from = from))
    #res1 <- do.call (rbind, res1)
    res1 <- dd_fit_stations (city, from = from)

    #mats2 <- dd_get_tripdistmats (city = city, osm = FALSE)
    #n <- seq (nrow (mats2$trip))
    #res2 <- lapply (n, function (i) dd_fit_stations (city, i = i, from = from))
    #res2 <- do.call (rbind, res2)
    res2 <- dd_fit_stations (city, from = from)

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

#' width_ratios
#'
#' Calculate ratios of widths of decay parameters for covariances calculated
#' over different distance deciles.
#'
#' @param city City for which ratios are to be calculated
#' @return \code{data.frame} of deciles, equivalent distances, and ratios of
#' both exponential decay width parameters (\code{k}), and squared errors
#' (\code{ss}).
#' @export
width_ratios <- function (city)
{
    pb <- txtProgressBar (style = 3)
    d <- rk <- rss <- rep (NA, 10)
    for (i in 1:10)
    {
        lo <- (i - 1) / 10
        hi <- i / 10
        x_osm <- dd_fit_stations (city = city, lower = lo, upper = hi)
        setTxtProgressBar (pb, (i - 0.5) / 10)
        x_str <- dd_fit_stations (city = city, lower = lo, upper = hi,
                                  osm = FALSE)
        rk [i] <- mean (x_osm$k, na.rm = TRUE) / mean (x_str$k, na.rm = TRUE)
        rss [i] <- mean (x_osm$ss, na.rm = TRUE) /
            mean (x_str$ss, na.rm = TRUE)
        # Then convert deciles to distance estimates
        #mats <- dd_get_tripdistmats (city, osm = TRUE)
        dmat_i <- distmats [[city]]
        indx <- dist_thresholds (dmat_i, lower = lo, upper = hi)
        for (j in seq (indx))
            if (length (indx [[j]]) > 0)
                mats$dist [j, indx [[j]]] <- mats$dist [indx [[j]], j] <- 0
        mats$dist [mats$dist == 0] <- NA
        d [i] <- mean (mats$dist, na.rm = TRUE)

        setTxtProgressBar (pb, i  / 10)
    }
    close (pb)
    data.frame (decile = 1:10 / 10, d = d, rk = rk, rss = rss)
}
