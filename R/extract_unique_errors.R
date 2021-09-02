#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_error
#' @return
#' @author Nicholas Tierney
#' @export
extract_unique_errors <- function(mobility_gam_which_error) {

  mobility_gam_which_error %>% 
    pull(error_message) %>% 
    unique()

}
