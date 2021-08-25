#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
#' @param all_dates
#' @param mobility
#' @return
#' @author Nicholas Tierney
#' @export
predict_gam_date_avg_weekday_effect <- function(df, 
                                                all_dates,
                                                mobility,
                                                min_date) {
  

  min_data_date <- min(mobility$date)
  max_data_date <- max(mobility$date)
  
  # create intervention step-change covariates
  intervention_steps <- create_intervention_steps(
    mobility, 
    all_dates
  )
  
  expand_grid(
    state_long = unique(df$state_long),
    dow = unique(df$dow),
    date = all_dates,
  ) %>%
    mutate(
      lga = df$lga[1],
      datastream = mobility$datastream[1],
      state = abbreviate_states(state_long)
    ) %>% 
    add_public_holidays() %>% 
    add_school_holidays() %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      # remove any named holidays not in the training data
      holiday = case_when(
        holiday %in% unique(df$holiday) ~ holiday,
        TRUE ~ "none"
      ),
      holiday = factor(holiday),
      is_a_school_holiday = !is.na(school_holiday),
      date_num = as.numeric(date - min_date),
      # clamp the smooth part of the prediction at both ends
      date_num = pmax(date_num, min_data_date - min_date),
      date_num = pmin(date_num, max_data_date - min_date)
    )

}
