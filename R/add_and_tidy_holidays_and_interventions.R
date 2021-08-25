#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility
#' @param intervention_steps
#' @param min_date
#' @return
#' @author Nicholas Tierney
#' @export
add_and_tidy_holidays_and_interventions <- function(mobility,
                                                    min_date,
                                                    all_dates) {
  
  intervention_steps <- create_intervention_steps(
    mobility, 
    all_dates
  )

  mobility %>%
    add_public_holidays() %>% 
    add_school_holidays() %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      is_a_school_holiday = !is.na(school_holiday),
      holiday = factor(holiday),
      date_num = as.numeric(date - min_date),
      dow = lubridate::wday(date, label = TRUE),
      dow = as.character(dow),
      intervention_stage = factor(
        intervention_stage,
        levels = unique(intervention_stage)
      )
    ) %>%
    filter(!is.na(trend))

}
