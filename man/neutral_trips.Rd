% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov-mi-matrices.R
\name{neutral_trips}
\alias{neutral_trips}
\title{neutral_trips}
\usage{
neutral_trips(distmat, tripmat, k = NULL)
}
\arguments{
\item{distmat}{A station-to-station distance matrix, generally obtained from
internal \link{distmats} data}

\item{tripmat}{A station-to-station trip matrix, generally obtained from
internal \link{tripmats} data}

\item{k}{Value of exponential decay used in spatial interaction model; if not
specified, value is determined by maximising fit between neutral and observed
models.}
}
\value{
A matrix equivalent to \code{tripmat}, but in which values are
generated by fitting a spatial interaction model.
}
\description{
Generate a neutral trip matrix by fitting a spatial interaction model. If no
distance decay parameter, \code{k}, is given, the value is derived by
maximising the fit of the spatial interaction model to the observed trip
matrix.
}
