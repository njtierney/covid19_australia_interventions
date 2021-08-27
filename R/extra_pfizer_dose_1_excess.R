# compute excess Pfizer dose 1s from the bolus under this scenario
extra_pfizer_dose_1_excess <- function (
  extra_pfizer,
  air
) {
  extra_pfizer %>%
    filter(dose_1_Pfizer > 0) %>%
    arrange(lga, age_air_80, date) %>%
    group_by(lga, age_air_80) %>%
    mutate(
      across(
        starts_with("dose"),
        cumsum
      )
    ) %>%
    ungroup() %>%
    full_join(
      air %>%
        filter(
          lga %in% extra_pfizer$lga,
          age_air_80 %in% extra_pfizer$age_air_80,
          date %in% extra_pfizer$date
        ) %>%
        select(
          c(date, lga, age_air_80, coverage_scenario, ends_with("extra"))
        ),
      by = c("date", "lga", "age_air_80")
    ) %>%
    mutate(
      dose_1_Pfizer_scenario_extra = pmin(dose_1_Pfizer_extra, dose_1_Pfizer)
    ) %>%
    select(
      lga, age_air_80, coverage_scenario, date, dose_1_Pfizer_scenario_extra
    ) %>%
    # collapse across age groups and LGAs
    group_by(date, coverage_scenario) %>%
    summarise(
      dose_1_Pfizer_scenario_extra = sum(dose_1_Pfizer_scenario_extra),
      .groups = "drop"
    )
  
}
