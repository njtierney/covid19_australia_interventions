#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param reff_trend_vaccination_plots
#' @return
#' @author Nicholas Tierney
#' @export
add_reff_trend_vac_plots <-
  function(reff_trend_vaccination_prep_plots) {
    
    # make sure directory exists:
    dir_create(here("outputs/nsw/figs/reff"))
    
    reff_trend_vaccination_prep_plots %>%
      group_by(lga) %>%
      nest() %>%
      mutate(
        plots = map2(.x = data,
                     .y = lga,
                     .f = gg_reff_trend_vaccination_plots),
        path = here(glue("outputs/nsw/figs/reff/NSW_{lga}_reff.png"))
      )
    
  }
