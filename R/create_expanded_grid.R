#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_nest_model_fit
#' @return
#' @author Nicholas Tierney
#' @export
create_expanded_grid <- function(mobility_nest_model_fit) {
 
  unnested_data <- mobility_nest_model_fit %>% 
    unnest(cols = data) 
  
  unique_state_long <- unique(unnested_data$state_long)
  unique_dow <- unique(unnested_data$dow)
  min_date <- min(unnested_data$date)
  max_date <- max(unnested_data$date)
  all_dates <- seq(min_date, max_date, by = 1)
  
  expanded_data <- expand_grid(
    state_long = unique_state_long,
    dow = unique_dow,
    date = all_dates,
  ) 
  
  unnested_data
}
  
  # date_num = as.numeric(date - min_date),
  # # clamp the smooth part of the prediction at both ends
  # date_num = pmax(date_num, min_data_date - min_date)
  # date_num = pmin(date_num, max_data_date - min_date)
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
  
# }
