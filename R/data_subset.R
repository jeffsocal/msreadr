#' Create a data subset
#'
#' @description
#' `subset()` is the main function for sub-setting spectra data from a ms2spectra
#' data-object based on a regular expression and targeted annotation. This function
#' will return a smaller ms2spectra data-object.
#'
#' @param data ms2spectra data object
#' @param ... a three part expression (eg. x == a)
#' @param .verbose a boolean
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' library(msreadr_quo)
#'
#' # creates a subset of just Ribosomes, based on the string in the annotation
#' # protein_description
#' hela_proteins %>%
#'    subset(description %like% "Ribosome") %>%
#'    summary()
#'
#' # creates a subset without Ribosomes
#' hela_proteins %>%
#'    subset(!description %like% "Ribosome") %>%
#'    summary()
#'
subset <- function(
    data = NULL,
    ...,
    rm.mbr = TRUE,
    .verbose = TRUE
){

  # visible bindings
  sample_id <- NULL

  check_ms2spectra(data)
  str_quo <- msreadr_quo(...)
  if(is.null(str_quo)) { return(data) }

  variable <- str_quo[['variable']]
  operator <- str_quo[['operator']]
  value <- str_quo[['value']]
  inverse <- str_quo[['inverse']]
  inverse_str <- ''
  if(inverse == TRUE) { inverse_str <- '!' }

  if(!variable %in% names(data)){
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500")))
    cli::cli_abort("data does not contain the field {.emph {variable}}. Use one of {.emph {names(data)}}")
  }

  if(.verbose == TRUE) {
    cli::cli_text("")
    cli::cli_div(theme = list(span.emph = list(color = "#ff4500"), span.info = list(color = "blue")))
    cli::cli_progress_step("Subsetting data: {.emph {inverse_str}{variable}} {.info {operator}} {.emph {value}}")
  }

  operator <- rlang::arg_match(operator, c("<","<=",">",">=","==","!=","%like%"))
  if(is.null(data) || is.null(variable) || is.null(value)) {return(data)}

  w <- c()
  if(operator == "<"){ w <- which(data[[variable]] < value) }
  else if(operator == ">"){ w <- which(data[[variable]] > value) }
  else if(operator == "<="){ w <- which(data[[variable]] <= value) }
  else if(operator == ">="){ w <- which(data[[variable]] >= value) }
  else if(operator == "=="){ w <- which(data[[variable]] == value) }
  else if(operator == "!="){ w <- which(data[[variable]] != value) }

  if(length(w) > 0){
    cli::cli_alert_info(".. found {length(w)} spectra")
    data <- data |> lapply(function(x, w){x[w]}, w)
  }

  cli::cli_progress_done()
  return(ms2spectra(data))
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

# #' @export
# `%!like%` <- function(a, b) {
#   !grepl(b, a, ignore.case = T)
# }
