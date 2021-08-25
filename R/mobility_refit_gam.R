#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_error
#' @return
#' @author Nicholas Tierney
#' @export
mobility_refit_gam <- function(mobility_gam_which_error) {

  mobility_gam_which_error %>% 
    mutate(error_message = map_chr(error, glue_collapse)) %>% 
    arrange(error_message) %>% 
    select(lga,
           datastream,
           data,
           error_message)
    

}
