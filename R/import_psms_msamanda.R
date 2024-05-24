#' Read a MS Amanda proteomic search results file
#'
#' @description
#' `import_msamanda()` is the helper function to import tandem ms search results
#' into a standardized data table.
#'
#' @param path
#' String path to file for importing
#'
#' @param cpus
#' The number of cpus to use for importing
#'
#' @return a tibble
#'
import_msamanda <- function(
    path,
    cpus = 1
){

  # visible bindings
  `Scan Number` <- NULL
  Title <- NULL
  `Nr of matched peaks` <- NULL
  `number of considered fragment ions` <- NULL
  `Amanda Score` <- NULL
  Rank <- NULL
  Sequence <- NULL
  `Protein Accessions` <- NULL
  str_clean <- NULL
  psm_sequence <- NULL
  Modifications <- NULL
  mass_proton <- NULL
  peptide_mass <- NULL

  if(!file.exists(path)){ cli::cli_abort(".. file {basename(path)} does not exist!") }

  # Scan Number is the experiment scan level (eg ms1, ms2 included)
  out <- path |>
    readr::read_tsv(
      skip=1,
      show_col_types = FALSE
    ) |>
    dplyr::mutate(
      ms_event = ifelse(`Scan Number` == 0, Title, `Scan Number`),
      psm_dp = `Nr of matched peaks` / `number of considered fragment ions`,
      Sequence = Sequence |> stringr::str_to_upper()) |>
    dplyr::rename(
      psm_score = `Amanda Score`,
      psm_rank = Rank,
      psm_sequence = Sequence,
      psm_modifications = Modifications,
      psm_protein = `Protein Accessions`
    ) |>
    filter(psm_rank == 1)

  # normalize peptide
  out <- out |> dplyr::select(!c('Filename','Scan Number'))

  out$psm_peptide <- 1:nrow(out) |> lapply(ms_amanda_peptide, out) |> unlist()
  # compute psm mass
  proton_mass <- mspredictr::mass_proton()
  out$psm_mh <- out$psm_peptide |> lapply(mspredictr::peptide_mass) |> unlist() + proton_mass

  return(out)
}

#' Format MS Amanda peptide strings
#'
#' @param sequence
#' A peptide string
#'
#' @param modifications
#' A string of modifications
#'
#' @return string
#'
ms_amanda_peptide <- function(
    i = NULL,
    tbl = NULL
){

  sequence <- tbl$psm_sequence[i]
  modifications <- tbl$psm_modifications[i]
  if(is.na(modifications)){ return(sequence) }

  sequence <- sequence |> stringr::str_extract_all('[A-Z]') |> unlist(use.names = FALSE)
  modifications <- modifications |> stringr::str_split(";") |> unlist(use.names = FALSE)

  masses <- modifications |> stringr::str_extract("[0-9]+\\.[0-9]+") |> as.numeric()
  local <- modifications |> stringr::str_extract("(?<=[A-Z])[0-9]+") |> as.numeric()

  for(n in local){
    sequence[n] <- paste0("[", sequence[n], mspredictr::num_trunc(masses[which(n == local)],2), "]")
  }
  out <- paste(sequence, collapse = '')
  return(out)
}
