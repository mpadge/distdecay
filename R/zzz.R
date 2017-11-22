.onLoad <- function (libname, pkgname)
{
    op <- options ()
    op.distdecay <- list (data_dir = file.path ("data", "data", "bikes"))
    toset <- !(names (op.distdecay) %in% names (op))
    if (any (toset))
        options (op.distdecay [toset])
    invisible ()
}


#' dd_get_data_dir
#'
#' Return the directory of pre-calculated distance matrices
#'
#' @export
dd_get_data_dir <- function ()
{
    op <- options ()
    if (!'data_dir' %in% names (op))
        stop ('distdecay data dir be retrieved')
    options ()$data_dir
}

#' dd_set_data_dir
#'
#' Set the directory of pre-calculated distance matrices
#'
#' @param dir Directory of pre-calculated distance matrices
#'
#' @export
dd_set_data_dir <- function (dir)
{
    if (!is.character (dir))
        stop ("Data directory must be a character string")
    if (!file.exists (dir))
        stop ("Data directory does not exist")

    op <- options ()
    op.distdecay <- list (data_dir = dir)
    options (op.distdecay)
}

