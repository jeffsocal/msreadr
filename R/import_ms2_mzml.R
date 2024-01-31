#' Parse an mzML file
#'
#' @description
#' `import_mzml()` a helper function to parse a given mzML file into a ms2spectra
#' standardized data object for use among the tidyproteomics packages.
#'
#' @param path
#' A character string of the path to the mzML formatted file
#'
import_mzml <- function(
    path = NULL
){

  #visible bindings
  precursorCharge <- NULL
  seqNum <- NULL
  acquisitionNum <- NULL
  msLevel <- NULL
  filterString <- NULL
  retentionTime <- NULL
  precursorMZ <- NULL
  precursorIntensity <- NULL
  collisionEnergy <- NULL
  injectionTime <- NULL
  ms_event_level <- NULL
  precursor_mz <- NULL
  precursor_z <- NULL
  precursor_mh <- NULL

  cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
  if(is.null(path)) {cli::cli_abort(c("x" = "path is empty"))}
  if(!grepl("\\.mzML", path)) {
    file_ext <- stringr::str_extract(path, "\\.[a-zA-Z]+$")
    cli::cli_abort(c("x" = "expected a {.emph .mzML} file, got {.emph {file_ext}}"))
  }

  proton_mass <- mspredictr::mass_proton()

  cli::cli_progress_step("Parsing mzML file {basename(path)}")

  tryCatch({

    obj_mzml <- path |> mzR::openMSfile()

    ## Get the header a data frame of attributes
    tbl_hdr <- mzR::header(obj_mzml) |>
      dplyr::mutate(precursorCharge = ifelse(precursorCharge == 0, 2, precursorCharge)) |>
      tibble::as_tibble() |>
      dplyr::select(
        spectrum_num = seqNum,
        ms_event = acquisitionNum,
        ms_event_level = msLevel,
        ms_event_info = filterString,
        precursor_rt = retentionTime,
        precursor_mz = precursorMZ,
        precursor_z = precursorCharge,
        precursor_intensity = precursorIntensity,
        collision_energy = collisionEnergy,
        injection_time = injectionTime
      ) |>
      ## Get the spectra a list of mz and intensity
      dplyr::mutate(peaks = mzR::spectra(obj_mzml)) |>
      dplyr::filter(ms_event_level == 2) |>
      dplyr::mutate(precursor_mh = purrr::map2(precursor_mz, precursor_z, mspredictr::mass_neutral) |> unlist() + proton_mass,
                    file = sub("\\.mzML", "", basename(path))) |>
      dplyr::relocate(precursor_mh, .before = 'peaks') |>
      dplyr::relocate(file)

  }, error = function(err) {
    err = as.character(as.vector(err))
    cli::cli_process_failed()
    cli::cli_abort(err)
  })

  return(tbl_hdr)
}
