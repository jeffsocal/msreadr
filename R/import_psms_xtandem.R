#' Read a X!Tandem proteomic search results file
#'
#' @description
#' `import_xtandem()` is the helper function to import tandem ms search results
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
import_xtandem <- function(
    path,
    cpus = 1
){

  # visible bindings
  id <- NULL
  spectrum_num <- NULL
  psm_sequence <- NULL
  label <- NULL
  psm_protein <- NULL
  expect <- NULL
  mh <- NULL
  y_ions <- NULL
  b_ions <- NULL
  psm_score <- NULL
  ms_event <- NULL
  hyperscore <- NULL

  if(!file.exists(path)){ cli::cli_abort(".. file {basename(path)} does not exist!") }

  d <- path |> xml2::read_html()
  h <- d |> xml2::xml_find_all('.//group[@id]', flatten = FALSE)

  xml_dataframe <- function(x){x |> unlist() |> t() |> as.data.frame()}

  l_peptides <- h |> xml2::xml_find_all('.//peptide/domain')

  tbl_seq <- list()
  n <- 0
  for(l_pep in l_peptides){
    n <- n + 1

    tbl_seq[[n]] <- l_pep |>
      xml2::xml_attrs() |>
      unlist() |> t() |>
      as.data.frame() |>
      tidyr::separate(id, into = c('ms_event', 'a' , 'b')) |>
      dplyr::mutate(ms_event = ms_event |> as.numeric()) |>
      dplyr::rename(psm_sequence = seq) |>
      dplyr::mutate(psm_peptide = psm_sequence)

    tmp_mod <- l_pep |>
      xml2::xml_children() |>
      xml2::xml_attrs()

    if(length(tmp_mod) > 0){
      tbl_seq[[n]]$psm_peptide <- tbl_seq[[n]]$psm_peptide |> xtandem_peptide(tmp_mod, tbl_seq[[n]]$start)
    }

  }

  tbl_peptides <- tbl_seq |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  tbl_proteins <- h |>
    xml2::xml_attrs() |>
    lapply(xml_dataframe) |>
    dplyr::bind_rows() |>
    dplyr::select(ms_event = id, psm_protein = label) |>
    dplyr::mutate(psm_protein = stringr::str_remove(psm_protein, "\\s.+")) |>
    dplyr::mutate(ms_event = ms_event |> as.numeric()) |>
    tibble::as_tibble()


  tbl <- tbl_proteins |>
    dplyr::full_join(tbl_peptides, by = "ms_event") |>
    dplyr::mutate(#psm_score = expect |> as.numeric() |> log10() * -1,
                  psm_score = hyperscore |> as.numeric(),
                  psm_mh = as.numeric(mh),
                  psm_dp = as.numeric(y_ions) + as.numeric(b_ions) / stringr::str_length(psm_sequence)) |>
    dplyr::group_by(ms_event) |>
    dplyr::arrange(dplyr::desc(psm_score)) |>
    dplyr::mutate(psm_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(origin = 'xtandem') |>
    dplyr::select(dplyr::matches('file|ms_event|origin|decoy|psm'))

  return(tbl)
}


#' Format X!Tandem peptide strings
#'
#' @param sequence
#' A peptide string
#'
#' @param modifications
#' A string of modifications
#'
#' @param val_start
#' A numeric value on where to start locating the modification on the peptide string
#'
#' @return string
#'
xtandem_peptide <- function(
    sequence = NULL,
    modifications = NULL,
    val_start = 1
){

  sequence <- sequence |> stringr::str_extract_all('[A-Z]') |> unlist()

  val_masses <- modifications |> lapply(function(x){x[3] |> as.numeric()}) |> unlist() |> as.numeric()
  val_locate <- modifications |> lapply(function(x){x[2]}) |> unlist() |> as.numeric() - as.numeric(val_start) + 1

  out <- ''
  for(i in 1:length(sequence)){
    if(i %in% val_locate){
      # xtandem can place multiple modifications on the same residue
      res <- paste0("[", sequence[i], mspredictr::num_trunc(sum(val_masses[which(i == val_locate)]),2), "]")
      sequence[i] <- res
    }
    out <- paste0(out, sequence[i])
  }
  return(out)
}
