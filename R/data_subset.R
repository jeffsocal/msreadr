#' Create a data subset
#'
#' @description
#' `subset()` is the main function for sub-setting spectra data from a ms2spectra
#' data-object based on a regular expression and targeted annotation. This function
#' will return a smaller ms2spectra data-object.
#'
#' @param x
#' An ms2spectra data object
#'
#' @param ...
#' A three part expression (eg. x == a)
#'
#' @param .verbose
#' A boolean to print messages
#'
#' @exportS3Method
#'
#' @examples
#' # creates a subset of just of spectra
#' data <- path_to_example() |> read_spectra()
#'
#' data |> print()
#'
#' data |> subset(ms_event == 3)
#'
#' data |> subset(precursor_mz > 400)
#'
subset.ms2spectra <- function(
    x = NULL,
    ...,
    .verbose = TRUE
){

  # visible bindings
  ms_event <- NULL

  check_ms2spectra(x)
  str_quo <- msreadr_quo(...)
  if(is.null(str_quo)) { return(x) }

  variable <- str_quo[['variable']]
  get_setname <- function(x, variable){
    for(set in intersect(names(x), c("ms1", "ms2"))){
      if(variable %in% names(x[[set]])){
        return(set)
      }
    }
    cli::cli_abort("{.emph {variable}} not a valid variable")
  }

  data_set <- get_setname(x, variable)
  x[[data_set]] <- subset_helper(x[[data_set]], str_quo, .verbose)

  scan_range <- x[[data_set]]$ms_event |> range()
  other_set <- setdiff(intersect(c('ms1','ms2'), names(x)), data_set)

  if(length(other_set) == 1){
    x[[other_set]] <- x[[other_set]] |>
      dplyr::filter(ms_event >= (scan_range[1] - 5), ms_event <= (scan_range[2] + 5))
  }

  cli::cli_progress_done()
  return(ms2spectra(x))
}

#' Helper function to subset a data frame
#'
#' @param ... a quo
#'
#' @return a list object
#'
msreadr_quo <- function(...) {

  rlang_quo <- rlang::quo(...)
  quo_obj <- rlang::quo_text(rlang_quo)
  quo_obj <- sub("/", " / ", quo_obj)

  quo_obj <- sub("\\s+", " ", quo_obj)
  if(quo_obj == "") {return(NULL)}
  quo_str <- stringr::str_split(quo_obj, " ")[[1]]
  if(length(quo_str) < 3) {
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
    cli::cli_abort("improper expression from {.emph {quo_obj}}")
  }
  if(length(quo_str) > 3) {
    quo_str[3] <- paste(quo_str[3:length(quo_str)], collapse = " ")
    w <- 4:length(quo_str)
    quo_str <- quo_str[-w]
  }
  quo_str <- gsub('\"', '', quo_str)
  quo_str <- as.list(quo_str)
  quo_str[4] <- FALSE
  names(quo_str) <- c('variable','operator','value','inverse')

  # logic for "not"
  if(grepl("^\\!", quo_str[1])) {
    if(grepl("^\\%", quo_str[2])) { quo_str[4] <- TRUE }
    else { cli::cli_abort("undefined method for `!` at start of expression") }
    quo_str[1] <- sub("^\\!", "", quo_str[1])
  }

  # logic to turn characters back to a numeric
  if(grepl("[^0-9\\.]", quo_str[3])) {
    # keep as string
  } else { quo_str[3] <- as.numeric(quo_str[3]) }

  return(quo_str)
}

#' Helper function to get a name from the ...
#'
#' @param ... a quo
#'
#' @return a character string
#'
msreadr_quo_name <- function(...){

  str_quo <- msreadr_quo(...)
  if(is.null(str_quo)) { return(NULL) }
  return(paste(str_quo[['variable']], str_quo[['value']], sep="-"))
}

#' Helper function for subsetting
#'
#' @param a a dplyr tibble column reference
#' @param b a dplyr tibble column reference
#'
#' @return a character string
#'
#' @export
#'
`%like%` <- function(a, b) {
  grepl(b, a, ignore.case = T)
}

#' Helper function to do the subsetting ...
#'
#' @param tbl ... the data object
#' @param str_quo ... a quo
#' @param .verbose ... T/F to print progress
#'
#' @return a character string
#'
subset_helper <- function(
    tbl = NULL,
    str_quo = NULL,
    .verbose = TRUE
){

  variable <- str_quo[['variable']]
  operator <- str_quo[['operator']]
  value <- str_quo[['value']]
  inverse <- str_quo[['inverse']]
  inverse_str <- ''

  if(inverse == TRUE) { inverse_str <- '!' }

  if(!variable %in% names(tbl)){
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
    cli::cli_abort("tbl does not contain the field {.emph {variable}}. Use one of {.emph {names(tbl)}}")
  }


  operator <- rlang::arg_match(operator, c("<","<=",">",">=","==","!=","%like%"))
  if(is.null(tbl) || is.null(variable) || is.null(value)) {return(tbl)}

  w <- c()
  if(operator == "<"){ w <- which(tbl[[variable]] < value) }
  else if(operator == ">"){ w <- which(tbl[[variable]] > value) }
  else if(operator == "<="){ w <- which(tbl[[variable]] <= value) }
  else if(operator == ">="){ w <- which(tbl[[variable]] >= value) }
  else if(operator == "=="){ w <- which(tbl[[variable]] == value) }
  else if(operator == "!="){ w <- which(tbl[[variable]] != value) }

  if(length(w) > 0){
    if(.verbose == TRUE) {
      cli::cli_div(theme = list(span.emph = list(color = "#ff4500"), span.info = list(color = "blue")))
      cli::cli_progress_step("Subsetting data: {.emph {inverse_str}{variable}} {.info {operator}} {.emph {value}}")
    }

    cli::cli_alert_info(".. found {length(w)} spectra")
    # can't recombine here as ms2 has a peak list
    tbl_all <- tbl |> lapply(function(x, w){x[w]}, w)
    tbl_peaks <- tbl_all$peaks
    tbl_all$peaks <- NULL
    tbl_all <- tbl_all |> dplyr::bind_cols()
    tbl_all$peaks <- tbl_peaks
    tbl <- tbl_all
  } else {
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500"), span.info = list(color = "blue")))
    cli::cli_alert_warning("data not subsetted for {.emph {inverse_str}{variable}} {.info {operator}} {.emph {value}}")
  }

  return(tbl)
}


