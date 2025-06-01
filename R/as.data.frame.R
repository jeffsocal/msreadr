#' Help function to convert a spectra data object to a data.frame
#'
#' @param x
#' The spectra data object
#'
#' @param ...
#' Unused legacy
#'
#' @param .data
#' The msNspectra data to extract as a data.frame
#'
#' @exportS3Method
#'
as.data.frame.msNspectra <- function(
    x,
    ...,
    .data = c("combined", "ms1", "ms2")
) {

  #visible bindings
  ms_event <- NULL

  .data = rlang::arg_match(.data)
  check_msNspectra(x)

  data_names <- names(x)
  msn_data <- c('ms1', 'ms2')
  get_data <- .data

  if(get_data == 'combined'){ get_data <- intersect(data_names, msn_data) }

  if(length(intersect(get_data, data_names)) != length(get_data)) {
    cli::cli_abort("{get_data} not present in data")
  }

  y <- list()
  for(get in get_data){
    x[[get]]$peaks <- NULL
    y <- y[[length(y) + 1]] <- x[[get]]
  }

  y <- y |> dplyr::bind_rows() |> dplyr::arrange(ms_event)

  class(y) <- 'data.frame'
  return(y)
}
