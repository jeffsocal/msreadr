#' Help function to convert a spectra data object to a data.frame
#'
#' @param x
#' The spectra data object
#'
#' @param ...
#' Unused legacy
#'
#' @exportS3Method
#'
as.data.frame.ms2spectra <- function(
    x,
    ...
) {

  check_ms2spectra(x)
  if('peaks' %in% names(x)) { x$peaks <- NULL }

  class(x) <- 'data.frame'
  return(x)
}
