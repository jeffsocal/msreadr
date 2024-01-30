#' Table of spectrum values
#'
#' @description
#' `read_spectra()` get search results from a variety of platforms
#'
#' @param path location of file to parse
#' @param include_spectra boolean to keep/drop the nested MSn spectrum
#'
#' @return a tibble
#' @export
#'
read_spectra <- function(
    path = NULL,
    include_spectra = TRUE
){

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
