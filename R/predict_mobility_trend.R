# given a dataframe of mobility data subsetted to a particular mobility metric,
# fit a generalised additive model for the trend and return a dataframe with the
# modelled mobility trend for all dates between min_date and max_date
predict_mobility_trend <- function(
  mobility,
  min_date = min(mobility$date),
  max_date = max(mobility$date)
  # min_date = first_date,
  # max_date = last_date + 7 * n_weeks_ahead
) {
  
  all_dates <- seq(min_date, max_date, by = 1)
  min_data_date = min(mobility$date)
  max_data_date = max(mobility$date)
  
  # empty row to return on error (NULL apparently won't work)
  empty_df <- create_empty_df(mobility, all_dates)
  
  # need to deal with factor levels not in the original fit. I.e. when no date
  # is observed with that intervention. Remove any interventions not matching
  # the date and states in the data
  
  # # drop any stages for which there is no data (cannot be estimated)
  if (interventions_has_0_rows(mobility)) {
    return(empty_df)
  }
  
  df <- add_and_tidy_holidays_and_interventions(
    mobility = mobility,
    min_date = min_date,
    all_dates = all_dates
  )
  
  m <- tryCatch(
    
    fit_gam_mobility(df),
    
    error = function(e) {
      NULL
    }
    
  )
  
  # return an empty row if there was an error
  if (is.null(m)) {
    return(empty_df)
  }
  
  # compute mean and standard deviation of Gaussian observation model for fitted data
  df_fitted <- tidy_gam_mobility_predictions(model = m, data = df)
  
  # predict each date, *averaging over the weekday effect for each date*
  pred_df <- predict_gam_date_avg_weekday_effect(
    df = df,
    all_dates = all_dates,
    mobility = mobility,
    min_date = min_date
  )  
  
  # predict trends under these conditions, and average over day of the week
  pred_df <- pred_df %>%
    mutate(
      predicted_trend = predict(m, newdata = pred_df)
    ) %>%
    group_by(
      state_long, state, lga, datastream, date
    ) %>%
    summarise(
      predicted_trend = mean(predicted_trend),
      .groups = "drop"
    ) %>%
    group_by(
      state, lga, datastream
    ) %>%
    # smooth fitted curve over days of the week and holidays
    mutate(
      predicted_trend = slider::slide_dbl(
        predicted_trend,
        gaussian_smooth,
        na.rm = TRUE,
        sd = 2.8,
        .before = 5,
        .after = 5
      )
    ) %>%
    ungroup() %>%
    left_join(
      df_fitted %>%
        select(
          state, state_long, lga, date,
          datastream,
          trend,
          fitted_trend,
          fitted_trend_lower,
          fitted_trend_upper
        ),
      by = c("state", "state_long", "lga", "datastream", "date")
    )
  
  pred_df
  
}