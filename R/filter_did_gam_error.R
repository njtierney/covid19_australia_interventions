#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_nest_model_fit
#' @return
#' @author Nicholas Tierney
#' @export
filter_did_gam_error <- function(mobility_nest_model_fit) {

  mobility_nest_model_fit %>% filter(did_error) 
    

}
