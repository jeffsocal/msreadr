#' Declaring the spectra data object
#'
#' @param obj
#' The current tibble or list object
#'
msNspectra <- function(
    obj
) {
  class(obj) <- "msNspectra"
  return(obj)
}

#' Check the integrity of a msNspectra data object
#'
#' @description
#' `check_msNspectra()` is a helper function that checks the structure and contents of
#' an msNspectra data object
#'
#' @param x
#' An msNspectra data object
#'
#' @return silent on success, an abort message on fail
#'
check_msNspectra <- function(
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
  if(!methods::is(x, 'msNspectra')) {
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
    cli::cli_abort(c("x" = "Input must be of type {.emph msNspectra}"))
  }

}
