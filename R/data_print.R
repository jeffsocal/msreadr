#' A helper function for the print definition of a ms2spectra object
#'
#' @param x
#' An ms2spectra data object
#'
#' @param ...
#' Unused legacy
#'
#' @exportS3Method
#'
print.ms2spectra <- function(
    x,
    ...
) {

  check_ms2spectra(x)

  if(x |> length() == 13) {
    x.size <- as.numeric(utils::object.size(x))
    cli::cli_h2(cli::style_bold("{.emph MS SPECTRA data object}"))
    println("Memory", glue::glue("{prettyunits::pretty_bytes(x.size)}"))
    println("Scans", x$precursor_mz |> length())
  } else {
    cli::cli_abort("No data in object")
  }

  ab <- x$precursor_intensity |> log10() |> round(1) |> range()
  rt <- x$precursor_rt |> round(2) |> range()
  mz <- x$precursor_mz |> round(4) |> range()
  z <- unique(x$precursor_z)

  println("Precursor")
  println("  Intensity", glue::glue("{ab[1]} - {ab[2]} (log10)"))
  println("  LC time", glue::glue("{rt[1]} - {rt[2]} (sec)"))
  println("  M/Z", glue::glue("{mz[1]} - {mz[2]}"))
  println("  Z", glue::glue("{z}"))

  println("")
  invisible(x)

}

#' Helper function for printing messages
#'
#' @param name string
#' @param message string
#' @param pad_length string
#'
#' @return console print line
#'
println <- function(name = '',
                    message = '',
                    pad_length = 20) {
  cat(stringr::str_pad(name, pad_length, 'right'), message, "\n")
}
