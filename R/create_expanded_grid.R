#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_nest_model_fit
#' @return
#' @author Nicholas Tierney
#' @export
create_expanded_grid <- function(mobility_gam_tidy_preds) {
  
  # compute mean and standard deviation of Gaussian observation model for fitted data
  # predict each date, *averaging over the weekday effect for each date*
  pred_df <- predict_gam_date_avg_weekday_effect(
    df = df,
    all_dates = all_dates,
    mobility = mobility,
    min_date = min_date
  )  
 
  # unnested_data <- mobility_gam_with_refit %>%
  #   unnest(cols = data)
  
  mobility_gam_tidy_preds %>% 
    slice(1) %>% 
    unnest(cols = data_gam_preds) 
  
  expand_dates_and_stuff <- function(data, n_weeks = 1){
    
    unique_state_long <- unique(data$state_long)
    unique_dow <- unique(data$dow)
    min_date <- min(data$date)
    max_date <- max(data$date)
    all_dates <- seq(min_date, max_date + (7 * n_weeks), by = 1)
    
    expanded_data <- expand_grid(
      state_long = unique_state_long,
      dow = unique_dow,
      date = all_dates,
    ) %>%
      mutate(
        date_num = as.numeric(date - min_date),
        # clamp the smooth part of the prediction at both ends
        # date_num = pmax(date_num, min_data_date - min_date),
        # date_num = pmin(date_num, max_data_date - min_date)
      )
  }
  
  mobility_gam_tidy_preds %>% 
    slice(1) %>% 
    mutate(data_expanded = map(data_gam_preds, expand_dates_and_stuff))
  
  
#   %>%
#     mutate(
#       lga = df$lga[1],
#       datastream = mobility$datastream[1],
#       state = abbreviate_states(state_long)
#     )
# 
  
  
  # columns are:
    # state_long
    # dow
    # date
    # lga
    # datastream
    # state
    # holiday
    # school_holiday
    # intervention_stage
        # is_a_holiday = holiday != "none",
        # holiday = replace_na(holiday, "none"),
        # remove any named holidays not in the training data
        # holiday = case_when(
        #   holiday %in% unique(df$holiday) ~ holiday,
        #   TRUE ~ "none"
        # ),
        # #   holiday = factor(holiday),
    # is_a_school_holiday = !is.na(school_holiday),
    # date_num = as.numeric(date - min_date),
    # # clamp the smooth part of the prediction at both ends
    # date_num = pmax(date_num, min_data_date - min_date),
    # date_num = pmin(date_num, max_data_date - min_date)
  
}
