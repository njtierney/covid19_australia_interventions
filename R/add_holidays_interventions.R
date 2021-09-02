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
  
  mobility_nsw %>% 
    add_public_holidays() %>% 
    add_school_holidays() %>% 
    # perform a nest so we can join the intervention with data
    # where each row is a state and a date
    group_by(state, date) %>% 
    nest() %>% 
    # then we can join the intervention data with this date/state rows
    full_join(intervention_with_number(),
              by = c("date", "state")) %>% 
    # we can then repopulate the intervention id
    group_by(state) %>% 
    ungroup() %>% 
    unnest(cols = data)  %>% 
    arrange(lga, date) %>% 
    fill(intervention_id, .direction = "downup") 
    group_by(date, state, intervention_id) %>% 
    nest() %>% 
    # THIS NEEDS A QUICK CODE REVIEW
    mutate(
      intervention_stage = as_factor(
        parse_number(intervention_id),
        ),
      .after = intervention_id
    ) %>% 
    # tidy up the holiday data
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      is_a_school_holiday = !is.na(school_holiday),
      holiday = factor(holiday),
      dow = lubridate::wday(date, label = TRUE),
      dow = as.character(dow)
    ) %>% 
    filter(!is.na(trend)) %>% 
    group_by(lga) %>% 
    mutate(
      date_num = as.numeric(date - first(date)),
      .after = date
    )  %>% 
    ungroup()

}
