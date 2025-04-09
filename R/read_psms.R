#' Read a proteomic search results file from Comet, MS Amanda, OMSSA, Sage and X!Tandem
#'
#' @description
#' `read_psms()` is the main function for importing tandem ms search results
#' into a standardized data table from various platforms.
#'
#' @param path_to_psms
#' String path to file for importing
#'
#' @param path_to_mzml
#' The number of cpus to use for importing
#'
#' @param platform
#' The number of cpus to use for importing
#'
#' @param ...
#' pass-through arguments
#'
#' @return a tibble
#'
read_psms <- function(
    path_to_psms = NULL,
    path_to_mzml = NULL,
    platform = c('comet','ms_amanda','omssa','sage','xtandem','tide'),
    ...
){

  if(is.null(path_to_psms)){ cli::cli_abort("no path to PSM file given") }
  if(!file.exists(path_to_psms)){ cli::cli_abort("PSM file does not exist") }

  if(!is.null(path_to_mzml)){
    if(!file.exists(path_to_mzml)){ cli::cli_abort("mzML file does not exist") }
    spec <- path_to_mzml |> read_spectra(include_spectra = FALSE)
  }
  # if(!cpus %in% 1:parallel::detectCores()){
  #   cli::cli_abort("number of cores outside the limit of 1|{parallel::detectCores()}")
  # }
  rlang::arg_match(platform)

  switch (platform,
          comet = {
            out <- import_comet(path_to_psms, ...)
          },
          ms_amanda = {
            out <- import_msamanda(path_to_psms)
          },
          omssa = {
            out <- import_omssa(path_to_psms)
          },
          sage = {
            out <- import_sage(path_to_psms)
          },
          xtandem = {
            out <- import_xtandem(path_to_psms)
          },
          tide = {
            out <- import_tide(path_to_psms)
          },
          {
            return(NULL)
          }
  )

  # add file meta data and down-select columns
  out <- out |>
    dplyr::mutate(platform = platform,
                  file = path_to_psms |> basename() |> stringr::str_remove("\\..+")) |>
    dplyr::select(dplyr::matches('file|platform|decoy|psm|ms_event|spectrum_num'))

  # order the columns
  out <- out |> dplyr::relocate(sort(colnames(out)))

  # add in scan level information if present
  if(!is.null(path_to_mzml)){
    switch (platform,
            xtandem = {
              out <- spec |> dplyr::left_join(out, by = 'spectrum_num')
            },
            {
              out <- spec |> dplyr::left_join(out, by = 'ms_event')
            }
    )
  }

  return(out)
}
