# scenario for a bolus of extra Pfizer dose 1s delivered to a given age group
# and set of lgas over a given time period, with rate based on previous observed
# rates
extra_pfizer <- function (
  air_current,
  dose_1_dates = max(air_current$date) + 1:21,
  n_extra_pfizer = 670000,
  target_ages_air = c("15-29", "30-39"),
  target_lgas = lgas_of_concern,
  previous_days_average = 0:27,
  dose_interval = 8 * 7
) {
  
  air_current %>%
    # get average daily doses
    average_daily_doses(
      previous_days_average = previous_days_average
    ) %>%
    # filter to only LGAs of concern, and age groups of concern
    filter(
      lga %in% target_lgas,
      age_air_80 %in% target_ages_air
    ) %>%
    # mask out types of dose not required
    mutate(
      across(
        c(starts_with("dose_2"), ends_with("AstraZeneca")),
        ~ . * 0
      )
    ) %>%
    # add on range of dates on which to overload doses
    full_join(
      expand_grid(
        lga = target_lgas,
        date = dose_1_dates
      ),
      by = "lga"
    ) %>%
    # normalise and multiply by number of doses
    mutate(
      dose_1_Pfizer = n_extra_pfizer * dose_1_Pfizer / sum(dose_1_Pfizer)
    ) %>%
    # add on dose 2s the required number of days into the future
    add_pfizer_dose_2s(
      dose_interval = dose_interval
    )
  
}
