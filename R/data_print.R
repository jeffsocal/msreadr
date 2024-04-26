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

  parts <- names(x)

  if(x |> length() > 1 & x[[2]] |> nrow() > 0) {
    x.size <- as.numeric(utils::object.size(x))
    cli::cli_h2(cli::style_bold("{.emph MS SPECTRA data object}"))
    println("Memory", glue::glue("{prettyunits::pretty_bytes(x.size)}"))

    for(part in intersect(parts, c("ms1", "ms2"))){

      y <- x[[part]]
      if(length(y) == 0) { next }

      println(glue::glue("{toupper(part)} Scans"), y |> nrow())
      rt <- y$ms_event_time |> round(2) |> range()
      println("  LC time", glue::glue("{rt[1]} - {rt[2]} (sec)"))
      if(part == 'ms1'){
        info <- y$ms_event_info |> unique()
        for(i in 1:min(3, length(info))){
          println(paste0("  ~ ", info[i]))
        }
      }

      if(part == 'ms2'){
        mz <- y$precursor_mz |> round(2) |> range()
        println("  precursors", glue::glue("{mz[1]} - {mz[2]} (mz)"))
      }

    }
  } else {
    cli::cli_abort("No data in object")
  }

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
