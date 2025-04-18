#' A helper function for the print definition of a ms2spectra object
#'
#' @param x
#' An ms2spectra data object
#'
#' @param ...
#' Unused legacy
#'
#' @param mz
#' The mz value (Th) to extract
#'
#' @param mz_tolerance
#' The tolerance +/- the mz value (Th) to extract
#'
#' @exportS3Method
#'
#' @import ggplot2
#'
plot.ms2spectra <- function(
    x = NULL,
    ...,
    type = c('IC','3D'),
    mz = NULL,
    mz_tolerance = 0.1
){

  # visible bindings
  TIC <- NULL
  XIC <- NULL
  INT <- NULL
  ms_event_time <- NULL
  ms_event_info <- NULL

  check_ms2spectra(x)
  if(!"ms1" %in% names(x)){
    cli::cli_abort("data does not contain ms1 survey scans")
  }

  if(type =='IC'){
    title <- yval <- "TIC"
    if(is.null(mz)){
      tbl <- x |> xic() |> dplyr::rename(INT = TIC)
    } else {
      yval <- "XIC"
      title <- paste(yval, mz, "+/-", mz_tolerance)
      tbl <- x |> xic(mz, mz_tolerance) |> dplyr::rename(INT = XIC)
    }

    tbl |>
      ggplot2::ggplot(ggplot2::aes(ms_event_time, INT)) +
      ggplot2::geom_step(ggplot2::aes(color = ms_event_info)) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_brewer(palette = 'Set1') +
      ggplot2::xlab('MS Event Time (sec)') +
      ggplot2::ylab(paste(yval, 'Ion Intensity')) +
      ggplot2::theme(
        legend.position = 'top',
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::labs(title = title)
  } else {
    title <- "Feature Map"

    x$ms1 |>
      dplyr::mutate(peaks = peaks |> purrr::map2(ms_event_time, function(x, t){x |> dplyr::mutate(ms_event_time = t)})) |>
      dplyr::select(peaks) |>
      tidyr::unnest(peaks) |>
      dplyr::bind_rows() |>
      ggplot2::ggplot(ggplot2::aes(mz, ms_event_time, color = log10(intensity))) +
      ggplot2::geom_point()


  }

}


