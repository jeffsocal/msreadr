#' Create a data subset
#'
#' @description
#' `subset()` is the main function for sub-setting spectra data from a ms2spectra
#' data-object based on a regular expression and targeted annotation. This function
#' will return a smaller ms2spectra data-object.
#'
#' @param data
#' An ms2spectra data object
#'
#' @param mz
#' The mz value (Th) to extract
#'
#' @param mz_tolerance
#' The tolerance +/- the mz value (Th) to extract
#'
#' @export
#'
xic <- function(
    data = NULL,
    mz = NULL,
    mz_tolerance = 0.1
){

  # visible bindings
  peaks <- NULL

  check_ms2spectra(data)
  if(!"ms1" %in% names(data)){
    cli::cli_abort("data does not contain ms1 survey scans")
  }

  if(is.null(mz)){
    data$ms1$TIC <- data$ms1$peaks |> lapply(function(x){x$intensity |> sum()}) |> unlist(use.names = FALSE)
    return(data$ms1 |> dplyr::select(-peaks))
  } else if(!is.numeric(mz)){
    cli::cli_abort("{.emph {mz}} must be numeric")
  }
  if(!is.numeric(mz_tolerance)){ cli::cli_abort("{.emph {mz_tolerance}} must be numeric") }

  data$ms1$XIC <- data$ms1$peaks |>
    lapply(
      function(x, m, t){
        w <- which(abs(x$mz - m) <= t)
        x$intensity[w] |> sum()
      },
      mz,
      mz_tolerance) |>
    unlist(use.names = FALSE)

  data$ms1 |> dplyr::select(-peaks)
}


