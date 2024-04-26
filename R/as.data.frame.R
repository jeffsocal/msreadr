#' Help function to convert a spectra data object to a data.frame
#'
#' @param x
#' The spectra data object
#'
#' @param ...
#' Unused legacy
#'
#' @param .data
#' The ms2spectra data to extract as a data.frame
#'
#' @exportS3Method
#'
as.data.frame.ms2spectra <- function(
    x,
    ...,
    .data = c("combined", "ms1", "ms2")
) {

  #visible bindings
  ms_event <- NULL

  .data = rlang::arg_match(.data)
  check_ms2spectra(x)

  if(nrow(x$ms1) > 0){ x$ms1$peaks <- NULL }
  if(nrow(x$ms2) > 0){ x$ms2$peaks <- NULL }

  if(.data == 'combined'){ y <- list(x$ms1, x$ms2) |> dplyr::bind_rows() |> dplyr::arrange(ms_event) }
  if(.data == 'ms1'){ y <- x$ms1 }
  if(.data == 'ms2'){ y <- x$ms2 }

  class(y) <- 'data.frame'
  return(y)
}
