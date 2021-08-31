#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param reff_trend_vaccination_plots
#' @return
#' @author Nicholas Tierney
#' @export
add_reff_trend_vac_plots <- function(reff_trend_vaccination_plots) {
  
  reff_trend_vaccination_plot %>% 
    mutate(plots = map(data, gg_reff_trend_vaccination_plots),
           path = glue("outputs/nsw/NSW_{lga}_reff.png"))
  
}
