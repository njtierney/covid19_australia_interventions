#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param interventions
#' @param all_dates
#' @return
#' @author Nicholas Tierney
#' @export
expand_dates_add_intervention_stage <- function(data, all_dates) {
  
  data %>% 
    group_by(intervention_id, state) %>%
    do(
      tibble(
        date = all_dates,
        intervention_effect = as.numeric(all_dates >= .$date)
      )
    ) %>%
    group_by(state, date) %>%
    summarise(
      intervention_stage = sum(intervention_effect),
      .groups = "drop"
    ) %>%
    mutate(
      intervention_stage = factor(intervention_stage)
    ) 

}
