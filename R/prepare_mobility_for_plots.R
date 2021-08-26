#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Nicholas Tierney
#' @export
prepare_mobility_for_plots <- function(mobility_fitted_nsw_concordance) {

  mobility_fitted_nsw_concordance %>% 
    filter(
      !is.na(datastream)
    ) %>% 
    group_by(LGA_NAME16) %>% 
    nest()

}
