% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_matrices.R
\name{dd_get_tripdistmats}
\alias{dd_get_tripdistmats}
\title{dd_get_tripdistsmatS}
\usage{
dd_get_tripdistmats(bikedb)
}
\arguments{
\item{bikedb}{Name of database}
}
\description{
Function to calculate station-to-station trip and distance matrices from a
database generated with \code{bikedata::store_bikedb}. This function
(re-)generates the internally bundled \link{distmats} and \link{tripmats}
data. It should only need to be used to generate data that are more recent
that these internally bundled versions.
}
\note{
The directory containing the database can be retrieveed
with \link{dd_get_data_dir}, and set with \link{dd_set_data_dir}. The data
generated by this function can replace current internal data by running
\code{save(res$tripmats,
system.file("data","tripmats.rda",package="distdecay")}, and
\code{save(res$distmats,
system.file("data","distmats.rda",package="distdecay")}.
}
