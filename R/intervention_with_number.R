#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
intervention_with_number <- function() {
  intervention_w_number <- 
  interventions(end_dates = TRUE) %>%
    arrange(date) %>%
    group_by(state) %>%
    mutate(intervention_id = as.character(glue("intervention_{row_number()}"))) %>%
    ungroup() %>%
    arrange(state) %>% 
    group_by(state) 
  
  intervention_w_number %>% 
    arrange(date) %>% 
    group_by(state) %>% 
    slice(1) %>% 
    mutate(date = date - 1,
           intervention_id = "intervention_0") %>% 
    ungroup() %>% 
    bind_rows(intervention_w_number) %>% 
    arrange(state, date)
  
}
