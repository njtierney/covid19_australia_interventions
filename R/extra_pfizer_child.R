# give out extra doses to all 12-15 year olds in the stated lgas
extra_pfizer_child <- function (
  air_current,
  dose_1_dates,
  n_extra_pfizer,
  target_adult_ages_air = c("15-29", "30-39"),
  target_lgas = lgas_of_concern,
  previous_days_average,
  n_child_doses = lga_child_pop(lgas)$`12-15`
) {
  
  lga_child_pop <- child_pop(target_lgas)
  
  # give out 140K dose 1s to 12-15 year olds in the LGAs of concern, proportionally to population
  child_doses <- extra_pfizer(
    air_current = air_current,
    dose_1_dates = dose_1_dates,
    n_extra_pfizer = 0,
    target_ages_air = c("0-14", "15-29"),
    target_lgas = target_lgas,
    previous_days_average = previous_days_average,
    dose_interval = Inf
  ) %>%
    left_join(
      pop_air,
      by = c(age_air_80 = "age_air", lga = "LGA_NAME19")
    ) %>%
    select(
      -LGA_CODE19
    ) %>%
    # adjust populations to get the number of 12-15 year olds in each age bin,
    # then compute fraction of 12-15 population in each AIR age band and LGA
    mutate(
      population = case_when(
        age_air_80 == "0-14" ~ population * lga_child_pop$fraction_12_15_in_0_14,
        age_air_80 == "15-29" ~ population * lga_child_pop$fraction_12_15_in_15_29
      ),
      fraction = population / sum(population)
    ) %>%
    # allocate Pfizer dose 1s to these LGAs
    mutate(
      dose_1_AstraZeneca = 0,
      dose_1_Pfizer = fraction * n_child_doses,
      dose_2_AstraZeneca = 0,
      dose_2_Pfizer = 0,
    ) %>%
    select(
      -population,
      -fraction
    ) %>%
    add_pfizer_dose_2s(
      dose_interval = 8 * 7
    )
  
  # give out remaining dose 1s to 16-39 year olds in LGAs of concern
  adult_doses <- extra_pfizer(
    air_current = air_current,
    dose_1_dates = dose_1_dates,
    n_extra_pfizer = pmax(0, n_extra_pfizer - n_child_doses),
    target_ages_air = target_adult_ages_air,
    target_lgas = target_lgas,
    previous_days_average = previous_days_average,
    dose_interval = 8 * 7
  )
  
  # join these, and sum across ages since there are two different 15-29
  # allocations. Note that the daily number of doses in 15-29 is an allocation
  # based on the population of 15 year olds, plus an allocation of additional
  # doses for the 15-29 age category as a whole, based on the rate of vaccination
  # in this age band. If only 16+ were vaccinated previously, this would be
  # identical to doing allocation on 15 year olds (based on population) and on
  # 16-29 year olds (based on previous rate). In reality, a small number of
  # priority 15 year olds will already have been vaccinated so this will lead to a
  # slightly too-fast allocation in those LGAs. But this should be a very small
  # bias, irrelevant at the scale of all 12 LGAs combined, and irrelevant by the
  # time the population is saturated.
  bind_rows(
    child_doses,
    adult_doses,
  ) %>%
    group_by(
      lga, age_air_80, date
    ) %>%
    summarise(
      across(
        starts_with("dose"),
        sum
      ),
      .groups = "drop"
    )
}
