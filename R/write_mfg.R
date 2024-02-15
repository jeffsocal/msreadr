#' Write an MGF spectrum file
#'
#' @description
#' `write_mgf()` write the contents of an ms2spectrum data object to an MGF
#' formmated file.
#'
#' @param data
#' An ms2spectra data object
#'
#' @param path
#' A character string of the path to the MGF formatted file
#'
#' @return silent on success, an abort message on fail
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- path_to_example() |> read_spectra()
#'
#' data |> write_mgf("path_to_file.mgf")
#' }
#'
write_mgf <- function(
    data = NULL,
    path = NULL
){

  check_ms2spectra(data)
  if(!'peaks' %in% names(data)) { cli::cli_abort("No spectrum peaks in data object ...") }

  cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
  if(is.null(path)) {cli::cli_abort(c("x" = "path is empty"))}
  if(!grepl("\\.mgf", path)) {
    file_ext <- stringr::str_extract(basename(path), "\\.[a-zA-Z]+$")
    cli::cli_abort(c("x" = "expected a {.emph .mgf} file, got {.emph {file_ext}}"))
  }

  n_peaks <- length(data$peaks)
  cli::cli_progress_bar(glue::glue("Writing MGF file {basename(path)}"),
                        total = n_peaks, clear = FALSE)

  tryCatch(
    {
      for(i in 1:n_peaks){

        cli::cli_progress_update()

        out_string <- ""
        append <- FALSE
        if(i > 1) {append <- TRUE}

        out_string <- c(out_string, "BEGIN IONS")
        out_string <- c(out_string, glue::glue("TITLE={data$ms_event_info[i]}"))
        out_string <- c(out_string, glue::glue("SCANS={data$ms_event[i]}"))
        out_string <- c(out_string, glue::glue("RTINSECONDS={data$precursor_rt[i]}"))
        out_string <- c(out_string, glue::glue("PEPMASS={data$precursor_mz[i]} {data$precursor_intensity[i]}"))
        out_string <- c(out_string, glue::glue("CHARGE=+{data$precursor_z[i]}"))
        for(ii in 1:dim(data$peaks[[i]])[1]){
          out_string <- c(out_string, glue::glue("{data$peaks[[i]][ii,1]}\t{round(data$peaks[[i]][ii,2])}"))
        }
        out_string <- c(out_string, "END IONS")
        out_string <- c(out_string, "")

        if(i %% 100 == 0){
          out_string |> vroom::vroom_write_lines(path, append=append)
          out_string <- ""
        }
      }
      if(length(out_string) != 0) {
        out_string |> vroom::vroom_write_lines(path, append=append)
      }

    },
    error=function(cond) {
      cli::cli_abort("ERROR: {cond}")
    },
    warning=function(cond) {
      cli::cli_alert_warning("WARNING: {cond}")
    }
  )
  cli::cli_alert_info('... done')
}
