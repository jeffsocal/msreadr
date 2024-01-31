#' Read and ms2 spectra file
#'
#' @description
#' `read_spectra()` is the main function for parse a given mzML or MGF file into
#' a ms2spectra standardized data object for use among the tidyproteomics packages.
#'
#' @param path
#' A character string of the path to the mzML or MGF formatted file
#'
#' @param include_spectra
#' A boolean to keep/drop the nested MSn spectrum
#'
#' @export
#'
#' @examples
#' # read in the data
#' data <- path_to_example() |> read_spectra()
#'
#' # print the data object
#' data
#'
read_spectra <- function(
    path = NULL,
    include_spectra = TRUE
){

  # visible binding
  peaks <- NULL

  cli::cli_div(theme = list(span.info = list(color = "#ff4500")))
  if(is.null(path)){ cli::cli_abort("no path to a file given") }
  if(!is.character(path)) { cli::cli_abort("`path` must be a character string")}
  if(!file.exists(path)) { cli::cli_abort("Not Found! `file:` {path}")}
  if(!is.logical(include_spectra)) { cli::cli_abort("`include_spectra` is not a boolean")}

  file_exts <- c(".mgf", ".mzML")
  file_ext <- stringr::str_extract(basename(path), "\\.[a-zA-Z]+$")
  ######################################################################
  # read in the data file
  if(!file_ext %in% file_exts) {
    cli::cli_abort(c("x" = "expected a {.emph {file_exts}} file, got {.emph {file_ext}}"))
  }

  if(file_ext == ".mgf") { tbl_spectra <- path |> import_mgf() }
  if(file_ext == ".mzML") { tbl_spectra <- path |> import_mzml() }

  if(include_spectra == FALSE){
    tbl_spectra <- tbl_spectra |> dplyr::select(-peaks)
  }

  return(ms2spectra(tbl_spectra))
}
