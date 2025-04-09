#' Read a Sage proteomic search results file
#'
#' @description
#' `import_sage()` is the helper function to import tandem ms search results
#' into a standardized data table.
#'
#' @param path
#' String path to file for importing
#'
#' @return a tibble
#'
import_sage <- function(
    path
){

  # visible bindings
  scannr <- NULL
  sage_discriminant_score <- NULL
  peptide <- NULL
  proteins <- NULL
  ms_event <- NULL
  calcmass <- NULL
  matched_peaks <- NULL
  peptide_len <- NULL
  charge <- NULL
  filename <- NULL
  str_peptide <- NULL
  str_sequence <- NULL

  if(!file.exists(path)){ cli::cli_abort(".. file {basename(path)} does not exist!") }

  proton_mass <- mspredictr::mass_proton()

  out <- path |> readr::read_tsv(show_col_types = FALSE) |>
    dplyr::rename(
      ms_event = scannr,
      psm_rank = rank,
      psm_score = sage_discriminant_score,
      psm_peptide = peptide,
      psm_protein = proteins
    ) |>
    dplyr::mutate(ms_event = stringr::str_remove(ms_event, ".+scan\\=") |> as.numeric(),
                  # 1Th correction to get [M+H]+
                  psm_mh = calcmass + proton_mass,
                  psm_dp = matched_peaks / (peptide_len * 2 * charge)) |>
    dplyr::select(dplyr::matches('file|ms_event|origin|decoy|psm')) |>
    dplyr::select(!filename)

  out$psm_peptide <- out$psm_peptide |> lapply(mspredictr::str_peptide) |> unlist()
  out$psm_sequence <- out$psm_peptide |> lapply(mspredictr::str_sequence) |> unlist()

  return(out)
}
