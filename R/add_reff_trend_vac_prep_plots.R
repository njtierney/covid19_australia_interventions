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
      scenario = factor(scenario),
      scenario = fct_relevel(scenario, "baseline"),
      coverage_scenario = factor(coverage_scenario),
      coverage_scenario = fct_relevel(
        coverage_scenario,
        c(
          # relevel that to be at the bototm
          "max 70% coverage",
          "max 80% coverage",
          "max 90% coverage",
          "max 100% coverage"
        )
      )
    ) %>% 
    unnest(cols = c(data))
  
  
}
