#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_for_plotting
#' @return
#' @author Nicholas Tierney
#' @export
gg_mobility_fitted_trend_all_lga <- function(mobility_fitted_nsw_concordance) {
  
  mobility_for_plotting <- prepare_mobility_for_plots(
    mobility_fitted_nsw_concordance
  )

  mobility_for_plotting %>%
    ungroup() %>% 
    mutate(plot = map2(
      .x = data,
      .y = LGA_NAME16,
      .f = ~gg_mobility_fitted_trend(
        mobility_fitted_nsw_concordance = .x,
        lga_name = .y
        ),
      last_date = max(mobility_fitted_nsw_concordance$date)
    )
    )

}
