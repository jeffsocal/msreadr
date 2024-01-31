#' Helper function for displaying path to data
#'
#' @export
#'
path_to_example <- function(
){
  list.files(system.file("extdata", "", package = "msreadr"), full.names = T, pattern = "\\.mzML")[1]
}
