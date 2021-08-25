#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_nsw
#' @return
#' @author Nicholas Tierney
#' @export
add_holidays_interventions <- function(mobility_nsw,
                                       n_weeks_ahead = 6) {
  
  
  mobility_school_public <- mobility_nsw %>% 
    add_public_holidays() %>% 
    add_school_holidays() %>% 
    add_intervention_expanded(start_date = min(.$date),
                              end_date = max(.$date))
    
  #   mutate(first_date = min(date),
  #          last_date = min(date),
  #          min_date = first_date,
  #          max_date = last_date + 7 * n_weeks_ahead) %>% 
  #   group_by(lga, datastream) %>% 
  #   mutate(
  #     # all_dates = list(seq(min_date, max_date, by = 1)),
  #     min_data_date = min(date),
  #     max_data_date = max(date)) 
  # 
  # summary(mobility_school_public$date)
  # 
  # unique(mobility_school_public$max_data_date)
  # 
  # interventions_nsw <- interventions(end_dates = TRUE) %>%
  #     filter(state == "NSW")
  # 
  # mobility_school_public_example <- mobility_school_public %>% 
  #   group_nest() %>% 
  #   slice(1) %>% 
  #   pull(data) %>% 
  #   pluck(1)
  # 
  # interventions(end_dates = TRUE)
  # 
  # mobility_school_public_example
  # 
  # all_dates <- seq(min(mobility_nsw$date), max(mobility_nsw$date), by = 1)
  # 
  # mobility_school_public_example %>% 
  #   select(date) %>% 
  #   mutate()
  # 
  # interventions_nsw %>% 
  #   filter(date <=max(mobility_school_public_example$date),
  #          date %in% mobility_school_public_example$date) %>% 
  #   mutate(
  #     intervention_id = paste0(
  #       "intervention_",
  #       match(date, unique(date))
  #     )
  #   ) %>% 
  #   group_by(intervention_id, state) %>%
  #   do(
  #     tibble(
  #       date = all_dates,
  #       intervention_effect = as.numeric(all_dates >= .$date)
  #     )
  #   ) %>%
  #   group_by(state, date) %>%
  #   summarise(
  #     intervention_stage = sum(intervention_effect),
  #     .groups = "drop"
  #   ) %>%
  #   mutate(
  #     intervention_stage = factor(intervention_stage)
  #   ) 
  
    

}
