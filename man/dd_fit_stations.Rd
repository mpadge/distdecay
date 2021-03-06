% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit-models.R
\name{dd_fit_stations}
\alias{dd_fit_stations}
\title{dd_fit_stations}
\usage{
dd_fit_stations(city, from = TRUE, lower = 0, upper = 1, mi = FALSE,
  expmod = TRUE, osm = TRUE, plot = FALSE)
}
\arguments{
\item{city}{City for which model is to be fitted}

\item{from}{Analyse decay functions for trips \strong{from} each station. If
\code{FALSE}, analyse for trips \strong{to} each station.}

\item{lower}{Lower limit (0-1) for distance cutoff used to calculate
covariances (see details)}

\item{upper}{Upper limit (0-1) for distance cutoff used to calculate
covariances (see details)}

\item{mi}{If \code{TRUE}, fit decay functions for mutual information,
otherwise fit covariances.}

\item{expmod}{If \code{TRUE}, fit exponential decay models, otherwise fit
power-law decays.}

\item{osm}{If \code{FALSE}, use straight-line distances for distance decay
parameters, otherwise street network distances.}

\item{plot}{If \code{TRUE}, plot decay function}
}
\value{
A \code{data.frame} of four values: \code{id}, the ID of the station;
\code{k}, the width parameter of the exponential decay; \code{b}, the value
of the exponent; and \code{ss}, the standardised sum of squared residuals.
}
\description{
Fit a generalised exponential decay model to data from one station
}
\note{
Power-law fits (with \code{expmod = FALSE}) are not reliable at all.
The decays really are not power-laws, so this ought not be used.
}
