#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
intervention_with_number <- function() {
  interventions(end_dates = TRUE) %>%
    arrange(date) %>%
    group_by(state) %>%
    mutate(intervention_id = as.character(glue("intervention_{row_number()}"))) %>%
    ungroup() %>%
    arrange(state)
  
}
