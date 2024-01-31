#' Declaring the spectra data object
#'
#' @param obj
#' The current tibble or list object
#'
ms2spectra <- function(
    obj
) {
  class(obj) <- "ms2spectra"
  return(obj)
}

#' Check the integrity of a ms2spectra data object
#'
#' @description
#' `check_ms2spectra()` is a helper function that checks the structure and contents of
#' an ms2spectra data object
#'
#' @param x
#' An ms2spectra data object
#'
#' @return silent on success, an abort message on fail
#'
check_ms2spectra <- function(
    x = NULL
){

  # fail if x is NULL
  if(is.null(x)) {
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
    cli::cli_abort(c("x" = "Input cannot be {.emph NULL}"))
  }
  if(mode(x) != "list") {
    cli::cli_abort(c("x" = "Input is {.emph mode(x)}}, should be an {.emph list}"))
  }
  if(!methods::is(x, 'ms2spectra')) {
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
    cli::cli_abort(c("x" = "Input must be of type {.emph ms2spectra}"))
  }

}
