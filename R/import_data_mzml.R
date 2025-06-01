#' Parse an mzML file
#'
#' @description
#' `import_mzml()` a helper function to parse a given mzML file into a msNspectra
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
  isolationWindowTargetMZ <- NULL
  isolationWindowLowerOffset <- NULL
  isolationWindowUpperOffset <- NULL
  collisionEnergy <- NULL
  injectionTime <- NULL
  ms_level <- NULL
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
    data_lcms <- mzR::header(obj_mzml) |>
      dplyr::mutate(precursorCharge = ifelse(precursorCharge == 0, 2, precursorCharge)) |>
      tibble::as_tibble() |>
      dplyr::select(
        spectrum_num = seqNum,
        ms_level = msLevel,
        ms_event = acquisitionNum,
        ms_event_info = filterString,
        ms_event_time = retentionTime,
        ms_injection_time = injectionTime,
        precursor_mz = precursorMZ,
        precursor_z = precursorCharge,
        precursor_intensity = precursorIntensity,
        precursor_isolation_target = isolationWindowTargetMZ,
        precursor_isolation_lower = isolationWindowLowerOffset,
        precursor_isolation_upper = isolationWindowUpperOffset,
        precursor_collision_energy = collisionEnergy
      ) |>
      ## Get the spectra a list of mz and intensity
      dplyr::mutate(peaks = mzR::spectra(obj_mzml))

    data_lcms$peaks <- data_lcms$peaks |>
      lapply(
        function(x){
          x <- x |> as.data.frame()
          colnames(x) <- c('mz', 'intensity')
          x
        }
      )

    ms1 <- NULL
    ms2 <- NULL

    if(1 %in% data_lcms$ms_level){
      ms1 <- data_lcms |> dplyr::filter(ms_level == 1) |>
        dplyr::select(!dplyr::matches('precursor')) |>
        dplyr::mutate(spectrum_num = dplyr::row_number())
    }

    if(2 %in% data_lcms$ms_level){
      ms2 <- data_lcms |> dplyr::filter(ms_level == 2) |> dplyr::mutate(
        precursor_mh = purrr::map2(precursor_mz, precursor_z, mspredictr::mass_neutral) |> unlist() + proton_mass) |>
        dplyr::relocate(precursor_mh, .before = 'peaks') |>
        dplyr::mutate(spectrum_num = dplyr::row_number())
    }

    out <- list(
      file = sub("\\.mzML", "", basename(path)),
      ms1 = NULL,
      ms2 = NULL
    )

    out$ms1 <- ms1
    out$ms2 <- ms2

  }, error = function(err) {
    err = as.character(as.vector(err))
    cli::cli_process_failed()
    cli::cli_abort(err)
  })

  return(out)
}
