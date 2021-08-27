average_daily_doses <- function (
  air_current,
  latest_data_date = max(air_current$date),
  previous_days_average = 0:27
) {
  
  air_current %>%
    filter(
      date %in% (latest_data_date - previous_days_average)
    ) %>%
    group_by(
      lga, age_air_80,
    ) %>%
    summarise(
      across(
        starts_with("dose"),
        ~ (max(.) - min(.)) / n()
      ),
      .groups = "drop"
    )
  
}
