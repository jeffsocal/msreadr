#' A helper function for the print definition of a msNspectra object
#'
#' @param x
#' An msNspectra data object
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
plot.msNspectra <- function(
    x = NULL,
    ...,
    type = c('IC','3D'),
    mz = NULL,
    mz_tolerance = 0.1,
    bins = 32
){

  # visible bindings
  TIC <- NULL
  XIC <- NULL
  INT <- NULL
  ms_event_time <- NULL
  ms_event_info <- NULL

  check_msNspectra(x)
  if(!"ms1" %in% names(x)){
    cli::cli_abort("data does not contain ms1 survey scans")
  }

  type <- rlang::arg_match(type)
  if (!bins %in% 1:4096){
    cli::cli_abort("bins should be a numeric between 1 and 4096, not {bins}")
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

    bin <- function(x, n = 52){round(x / (diff(range(x)) / n)) * (diff(range(x)) / n)}

    title <- "Feature Map"

    x$ms1 |>
      dplyr::mutate(peaks = peaks |> purrr::map2(ms_event_time, function(x, t){x |> dplyr::mutate(ms_event_time = t)})) |>
      dplyr::select(peaks) |>
      tidyr::unnest(peaks) |>
      dplyr::bind_rows() |>
      dplyr::mutate(mz = mz |> bin(n = bins)) |>
      dplyr::mutate(ms_event_time = ms_event_time |> bin(n = bins)) |>
      dplyr::group_by(mz, ms_event_time) |>
      dplyr::summarise(intensity = intensity |> sum()) |>
      ggplot2::ggplot(ggplot2::aes(mz, ms_event_time, fill = intensity)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(trans = 'log10') +
      ggplot2::theme(
        legend.position = 'none',
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::labs(title = title) +
      theme_classic()


  }

}


