#' Write an mzML spectrum file
#'
#' @description
#' `write_mzml()` write the contents of an ms2spectrum data object to an mzML
#' formmated file.
#'
#' @param data
#' An ms2spectra data object
#'
#' @param path
#' A character string of the path to the mzML formatted file
#'
#' @return silent on success, an abort message on fail
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- path_to_example() |> read_spectra()
#'
#' data |> write_mzml("path_to_file.mzML")
#' }
write_mzml <- function(
    data = NULL,
    path = NULL
){

  # visible binding
  spectra <- NULL
  seqNum <- NULL
  peaks <- NULL
  pre_rt <- NULL
  pre_mz <- NULL
  pre_z <- NULL
  totIonCurrent <- NULL
  scan_info <- NULL
  lowMZ <- NULL
  highMZ <- NULL

  check_ms2spectra(data)

  cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
  if(is.null(path)) {cli::cli_abort(c("x" = "path is empty"))}
  if(!grepl("\\.mzML", path)) {
    file_ext <- stringr::str_extract(basename(path), "\\.[a-zA-Z]+$")
    cli::cli_abort(c("x" = "expected a {.emph .mzML} file, got {.emph {file_ext}}"))
  }

  cli::cli_progress_step("Writing mzML file {basename(path)}")

  colsneeded <- c("seqNum",                     "acquisitionNum",
                  "msLevel",                    "polarity",
                  "peaksCount",                 "totIonCurrent",
                  "retentionTime",              "basePeakMZ",
                  "basePeakIntensity",          "collisionEnergy",
                  "ionisationEnergy",           "lowMZ",
                  "highMZ",                     "precursorScanNum",
                  "precursorMZ",                "precursorCharge",
                  "precursorIntensity",         "mergedScan",
                  "mergedResultScanNum",        "mergedResultStartScanNum",
                  "mergedResultEndScanNum",     "injectionTime",
                  "filterString",               "spectrumId",
                  "centroided",                 "ionMobilityDriftTime",
                  "isolationWindowTargetMZ",    "isolationWindowLowerOffset",
                  "isolationWindowUpperOffset", "scanWindowLowerLimit",
                  "scanWindowUpperLimit")


  xml_header <- tibble::tibble(
    seqNum = data$spectrum_num,
    acquisitionNum = seqNum * 2,
    msLevel = 2,
    polarity = 1,
    peaksCount = lapply(data$peaks, nrow) |> unlist(),
    totIonCurrent = lapply(data$peaks, function(x){sum(x[,2])}) |> unlist(),
    retentionTime = data$precursor_rt,
    basePeakMZ = lapply(data$peaks, function(x){sum(x[,2])}) |> unlist(),
    basePeakIntensity = lapply(data$peaks, function(x){x[which(x[,2] == max(x[,2]))[1],1]}) |> unlist(),
    collisionEnergy = 28,
    ionisationEnergy = 0,
    lowMZ = lapply(data$peaks, function(x){min(x[,1])}) |> unlist(),
    highMZ = lapply(data$peaks, function(x){max(x[,1])}) |> unlist(),
    precursorScanNum = seqNum,
    precursorMZ = data$precursor_mz,
    precursorCharge = data$precursor_z,
    precursorIntensity = totIonCurrent * 3.14,
    mergedScan = as.numeric(NA),
    mergedResultScanNum = as.numeric(NA),
    mergedResultStartScanNum = as.numeric(NA),
    mergedResultEndScanNum = as.numeric(NA),
    injectionTime = 100,
    filterString = data$ms_event_info,
    spectrumId = seqNum |> as.character(),
    centroided = TRUE,
    ionMobilityDriftTime = as.numeric(NA),
    isolationWindowTargetMZ = data$precursor_mz,
    isolationWindowLowerOffset = 0.6,
    isolationWindowUpperOffset = 0.6,
    scanWindowLowerLimit = ceiling(lowMZ / 50) * 50,
    scanWindowUpperLimit = floor(highMZ / 50) * 50
  ) |>
    dplyr::select(dplyr::all_of(colsneeded)) |>
    as.data.frame()

  tryCatch(
    {
      mzR::writeMSData(
        object = data$peaks,
        header = xml_header,
        file = path)

      cli::cli_progress_done()

    },
    error=function(cond) {
      cli::cli_abort("ERROR: {cond}")
    },
    warning=function(cond) {
      cli::cli_alert_warning("WARNING: {cond}")
    }
  )

}
