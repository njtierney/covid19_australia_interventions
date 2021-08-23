# given a dataframe of mobility data subsetted to a particular mobility metric,
# fit a generalised additive model for the trend and return a dataframe with the
# modelled mobility trend for all dates between min_date and max_date
predict_mobility_trend <- function(
  mobility,
  min_date = min(mobility$date),
  max_date = max(mobility$date)
) {
  
  all_dates <- seq(min_date, max_date, by = 1)
  min_data_date = min(mobility$date)
  max_data_date = max(mobility$date)
  
  # empty row to return on error (NULL apparently won't work)
  empty_df <- tibble(
    state_long = mobility$state_long[1],
    state = mobility$state[1],
    lga = mobility$lga[1],
    date = all_dates,
    predicted_trend = NA,
    datastream = mobility$datastream[1],
    fitted_trend = NA,
    fitted_trend_lower = NA,
    fitted_trend_upper = NA
  ) %>%
    left_join(
      mobility,
      by = c("lga", "date", "datastream", "state_long", "state")
    ) %>%
    relocate(
      trend,
      .after = "datastream"
    )
  
  public_holidays <- holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    ) %>%
    rename(
      holiday = name
    )
  
  school_holidays <- school_holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    )
  
  # need to deal with factor levels not in the original fit. I.e. when no date
  # is observed with that intervention. Remove any interventions not matching
  # the date and states in the data
  
  # # drop any stages for which there is no data (cannot be estimated)
  
  
  # create intervention step-change covariates
  interventions <- interventions(end_dates = TRUE) %>%
    filter(
      date <= max_data_date,
      date %in% mobility$date
    )
  
  if (nrow(interventions) == 0) {
    return(empty_df)
  }
  
  intervention_steps <- interventions %>%
    mutate(
      intervention_id = paste0(
        "intervention_",
        match(date, unique(date))
      )
    ) %>%
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
  
  df <- mobility %>%
    left_join(
      public_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      school_holidays,
      by = c("state", "date")
    ) %>%
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
  
  library(mgcv)
  
  m <- tryCatch(
    gam(trend ~
          
          # smooth variations in mobility
          s(date_num, k = 50) +
          
          # step changes around intervention impositions
          intervention_stage +
          
          # random effect on holidays (different for each holiday, but shrunk
          # to an average holiday effect which used to predict into future)
          is_a_holiday +
          s(holiday, bs = "re") +
          
          # constant effect for school holidays
          is_a_school_holiday +
          
          # day of the week effect
          dow,
        
        select = TRUE,
        gamma = 2,
        data = df),
    
    error = function(e) {
      NULL
    }
    
  )
  
  # return an empty row if there was an error
  if (is.null(m)) {
    return(empty_df)
  }
  
  # compute mean and standard deviation of Gaussian observation model for fitted data
  fit <- predict(m, se.fit = TRUE)
  fit$sd <- sqrt(var(residuals(m)) + fit$se.fit ^ 2)
  
  df_fitted <- df %>%
    # predict with fitted model (and get 90% CIs)
    mutate(
      fitted_trend = fit$fit,
      fitted_trend_upper = fit$fit + fit$sd * qnorm(0.025),
      fitted_trend_lower = fit$fit + fit$sd * qnorm(0.975),
    )
  
  # predict each date, *averaging over the weekday effect for each date*
  pred_df <- expand_grid(
    state_long = unique(df$state_long),
    dow = unique(df$dow),
    date = all_dates,
  ) %>%
    mutate(
      lga = df$lga[1],
      datastream = mobility$datastream[1],
      state = abbreviate_states(state_long)
    ) %>% 
    left_join(
      public_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      school_holidays,
      by = c("state", "date")
    ) %>%
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