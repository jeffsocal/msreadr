#' Tidy-Quant data object print definition
#'
#' @param x rmfasta data object
#' @param ... unused legacy
#'
#' @return print object summary
#'
#' @exportS3Method
#'
as.data.frame.ms2spectra <- function(
    x, ...
) {

  check_ms2spectra(x)
  if('peaks' %in% names(x)) { x$peaks <- NULL }

  class(x) <- 'data.frame'
  return(x)
}
