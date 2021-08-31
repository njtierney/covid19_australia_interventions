#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param reff_trend_vaccination
#' @return
#' @author Nicholas Tierney
#' @export
add_reff_trend_vac_prep_plots <- function(reff_trend_vaccination) {

  reff_trend_vaccination %>%
    # filter(lga == this_lga) %>%
    group_by(lga, scenario, coverage_scenario) %>% 
    nest() %>% 
    mutate(
      scenario = factor(
        scenario,
        levels = c(
          "baseline",
          "670K extra doses"
        )
      ),
      coverage_scenario = factor(
        coverage_scenario,
        levels = c(
          "max 70% coverage",
          "max 80% coverage", 
          "max 90% coverage",
          "max 100% coverage"
        )
      )
    ) 
  

}
