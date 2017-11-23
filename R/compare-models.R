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
    dat <- dd_get_vecs (ci)
    n <- nrow (dat)
    d <- dat$d
    y <- dat$n

    if (!mi)
        message (city, ": Fitting distance decays to covariances\n")
    else
        message (city, ": Fitting distance decays to mutual information\n")

    ssmult <- 1 / 10000
    if (mi)
        ssmult <- 1 / 10000

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

    mod1 <- data.frame (array (NA, dim = c(n, 3)))
    names (mod1) <- c ("ss", "k", "aic")
    mod2 <- data.frame (array (NA, dim = c(n, 4)))
    names (mod2) <- c ("ss", "k", "b", "aic")
    results <- list (pow = mod1, gauss = mod1, expo = mod2, weib = mod2,
                     cauch = mod1, boxcox = mod2)
    cols <- c ("black", "red", "red", "blue", "lawngreen", "magenta")
    ltys <- c (1, 1, 2, 1, 1, 1)

    if (plot)
        plot (d, y, pch = 1, col = "orange", log = "xy")
    dfit <- seq(min(d), max(d), length.out = 100)

    # ***** power-law is done separately:
    if (mi)
    {
        indx <- which (d > 0) # Because 0 distances do occur
        y2 <- -y[indx]
    }
    else
    {
        indx <- which (y > 0 & d > 0) # Because 0 distances do occur
        y2 <- y [indx]
    }
    d2 <- d [indx]

    mod <- lm (log10 (y2) ~ log10 (d2))
    coeffs <- summary (mod)$coefficients
    yfit <- 10 ^ (coeffs [1] + log10 (d2) * coeffs [2])
    results$pow$ss [1] <- mean ( (yfit - y2) ^ 2) * ssmult
    results$pow$k [1] <- coeffs [2]
    results$pow$aic [1] <- AIC (mod)
    if (plot)
    {
        yfit <- 10 ^ (coeffs [1] + log10 (dfit) * coeffs [2])
        if (mi)
            yfit <- -yfit
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
            results [[i + 1]]$ss [i] <- yfit * ssmult
            results [[i + 1]]$k [i] <- summary (mod)$coefficients [2]
            results [[i + 1]]$aic [i] <- AIC (mod)
            if (dim (results[[i + 1]])[2] > 3)
                results [[i + 1]]$b <- summary (mod)$coefficients [3]
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
        title (main = i)
        #loc <- locator (n = 1) #nolint
    }

    if (mi)
        results$pow$k <- -results$pow$k

    # Box-Cox fits yield occassionally enormous k-values, so
    indx <- which (results$boxcox$k > (5 * median (results$boxcox$k,
                                                   na.rm = TRUE)))
    results$boxcox [indx, ] <- NA
    # And sign of Gaussian k is sometimes negative, so
    results$gauss$k <- abs (results$gauss$k)

    cat ("Model\t|\tSS\tAIC\t|\tk\t\tb\t|\n")
    cat (c (rep ("-", 66), "\n"), sep = "")
    for (i in 1:length (results))
    {
        ss <- formatC (mean (results [[i]]$ss, na.rm = TRUE),
                    format = "f", digits = 2)
        aic <- round (mean (results [[i]]$aic, na.rm = TRUE))
        k <- formatC (mean (results [[i]]$k, na.rm = TRUE),
                    format = "f", digits = 2)
        if (dim (results [[i]])[2] > 3)
            b <- formatC (mean (results [[i]]$b, na.rm = TRUE),
                    format = "f", digits = 2)
        else
            b <- ""
        cat (names (results) [i], "\t|\t", ss, "\t", aic, "\t|\t",
             k, "\t\t")
        if (dim (results [[i]])[2] == 3)
            cat ("\t|\n")
        else
            cat (b, "\t|\n")

    }
    cat (c (rep ("-", 66), "\n"), sep = "")

    return (results)
} # end compare.models()
