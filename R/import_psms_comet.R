#' Read a Comet proteomic search results file
#'
#' @description
#' `import_comet()` is the helper function to import tandem ms search results
#' into a standardized data table.
#'
#' @param path
#' String path to file for importing
#'
#' @param psm_score
#' the specific score value to import, either xcorr or e-value
#'
#' @return a tibble
#'
import_comet <- function(
    path,
    comet_score = 'xcorr'
){

  # visible bindings
  `e-value` <- NULL
  calc_neutral_mass <- NULL
  ions_matched <- NULL
  ions_total <- NULL
  num <- NULL
  modified_peptide <- NULL
  plain_peptide <- NULL
  protein <- NULL
  str_peptide <- NULL
  xcorr <- NULL

  if(!file.exists(path)){ cli::cli_abort(".. file {basename(path)} does not exist!") }
  comet_score <- rlang::arg_match(comet_score, c('xcorr', 'e-value'))

  proton_mass <- mspredictr::mass_proton()

  # scan is experiment scan level (eg ms1, ms2 included)
  out <- path |>
    readr::read_tsv(
    skip=1,
    show_col_types = FALSE
  )

  if(comet_score == 'xcorr'){
    out <- out |> dplyr::mutate(psm_score = xcorr)
  } else {
    out <- out |> dplyr::mutate(psm_score = -log10(`e-value`))
  }

  out <- out |>
    dplyr::mutate(
      # 1Th correction to get [M+H]+
      psm_mh = calc_neutral_mass + proton_mass,
      psm_dp = ions_matched / ions_total
    ) |>
    dplyr::rename(
      ms_event = scan,
      psm_rank = num,
      psm_peptide = modified_peptide,
      psm_sequence = plain_peptide,
      psm_protein = protein
    )

  out$psm_peptide <- out$psm_peptide |> lapply(mspredictr::str_peptide) |> unlist()

  return(out)
}
