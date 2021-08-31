#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lga
#' @return
#' @author Nicholas Tierney
#' @export
remove_lga_tails <- function(x) {
  str_trim(str_remove_all(x, "\\(A\\)|\\(C\\)|\\(NSW\\)"))
}
