# turn the forecast into a function, and add on a method to add extra vaccinations in some LGAs/weeks
forecast_vaccination <- function(
  # current vaccination coverage timeseries
  air_current,
  # which previous days to average over in computing daily vaccination rate
  # (default previous 4 weeks)
  previous_days_average = 0:27,
  # end of simulations
  max_date = as.Date("2021-09-30"),
  # proportion of population accepting vaccines
  max_coverages = c(0.7, 0.8, 0.9, 1),
  # optional file of additional doses
  extra_doses = NULL,
  # whether or not under 15s are being vaccinated (affects maximum population
  # for 10-14 and 15-19 age groups)
  vaccinating_12_15 = TRUE,
  # the name of this scenario
  scenario_name = "baseline"
) {
  
  latest_data_date <- max(air_current$date)
  
  # what fractions of the 0-14 and 15-29 age groups are eligible for vaccination?
  if (vaccinating_12_15) {
    fraction_0_14_eligible <- pop_disagg$fraction_0_14_eligible_child
    fraction_15_29_eligible <- 1
  } else {
    fraction_0_14_eligible <- 0
    fraction_15_29_eligible <- pop_disagg$fraction_15_29_eligible_adult
  }
  
  # correct the populations in air current to account for eligibility of different age groups
  air_current <- air_current %>%
    mutate(
      # correct age group populations for eligibility of ages within them, and
      # the rate of vaccine acceptance
      eligibility_correction = case_when(
        # if we are vaccinating
        age_air_80 == "0-14" ~ fraction_0_14_eligible,
        age_air_80 == "15-29" ~ fraction_15_29_eligible,
        TRUE ~ 1
      ),
      eligible_population = population * eligibility_correction
    ) %>%
    select(
      -eligibility_correction
    )
  
  # compute daily average numbers of doses in each age group and lga over the past weeks
  # start with an average, then try a random effects model (shrinkage & extrapolation will help for small populations)
  dailies <- average_daily_doses(
    air_current = air_current,
    latest_data_date = latest_data_date,
    previous_days_average = previous_days_average
  )
  
  # cumulative number of doses as a the most recent time point
  starting <- air_current %>%
    filter(
      date == latest_data_date
    ) %>%
    rename_with(
      .fn = function(x) paste0("starting_", x),
      .cols = starts_with("dose")
    ) %>%
    select(
      -date
    )
  
  # compute maximum coverages by age and LGA (allowing them to exceed the
  # eligible population coverages in the observed data)
  max_coverage <- air_current %>%
    filter(
      date == max(date)
    ) %>%
    # compute coverage as at the latest date for each age and LGA (capped at 100%)
    mutate(
      any_doses = dose_1_Pfizer + dose_1_AstraZeneca,
      observed_max_coverage = any_doses / eligible_population,
      observed_max_coverage = pmin(1, observed_max_coverage),
      # some ages have zero eligible population, so set observed max coverage to 0
      observed_max_coverage = replace_na(observed_max_coverage, 0),
    ) %>%
    select(
      lga, age_air_80, observed_max_coverage
    ) %>%
    # add on the assumed maximum coverage (later, for one of a number of scenarios)
    full_join(
      expand_grid(
        lga = unique(air_current$lga),
        age_air_80 = unique(air_current$age_air_80),
        hypothetical_max_coverage = max_coverages
      ),
      by = c("lga", "age_air_80")
    ) %>%
    # compute the maximum of the two for capping vaccination
    mutate(
      max_coverage = pmax(observed_max_coverage, hypothetical_max_coverage)
    ) %>%
    select(
      -observed_max_coverage
    )
  
  # extrapolate the number of cumulative doses into the future, without capping coverage
  
  # future dates for each LGA and age
  future_doses <- expand_grid(
    lga = unique(air_current$lga),
    age_air_80 = unique(air_current$age_air_80),
    date = seq(latest_data_date + 1, max_date, by = 1)
  ) %>%
    # add on daily vaccination rates (previous average)
    left_join(
      dailies,
      by = c("lga", "age_air_80")
    )
  
  # optionally add on some extra doses to represent an alternate vaccination
  # scenario
  if (!is.null(extra_doses)) {
    
    future_doses <- future_doses %>%
      bind_rows(
        extra_doses
      ) %>%
      group_by(
        lga, age_air_80, date,# eligible_population
      ) %>%
      summarise(
        across(
          starts_with("dose"),
          sum
        ),
        .groups = "drop"
      ) %>%
      filter(
        date <= max_date
      )
    
  }
  
  # compute the cumulative sum to get total future doses by each day
  air_forecast <- future_doses %>%
    arrange(lga, age_air_80, date) %>%
    group_by(
      lga,
      age_air_80,
      # eligible_population
    ) %>%
    mutate(
      across(
        starts_with("dose"),
        cumsum
      ),
      across(
        starts_with("dose"),
        round
      )
    ) %>%
    # add on current observed total number of doses
    left_join(
      starting,
      by = c("lga", "age_air_80")
    ) %>%
    mutate(
      dose_1_AstraZeneca = dose_1_AstraZeneca + starting_dose_1_AstraZeneca,
      dose_1_Pfizer = dose_1_Pfizer + starting_dose_1_Pfizer,
      dose_2_AstraZeneca = dose_2_AstraZeneca + starting_dose_2_AstraZeneca,
      dose_2_Pfizer = dose_2_Pfizer + starting_dose_2_Pfizer
    ) %>%
    select(
      -starts_with("starting")
    ) %>%
    # add on a forecast flag
    mutate(
      forecast = TRUE
    )
  
  # add the forecast to the current air data to get the full time series
  air_saturated <- air_current %>%
    mutate(
      forecast = FALSE
    ) %>%
    bind_rows(
      air_forecast
    ) %>%
    # add on the max coverages (with different thresholds)
    full_join(
      max_coverage,
      by = c("lga", "age_air_80")
    ) %>%
    arrange(date, lga, age_air_80) %>%
    mutate(
      accepting_population = eligible_population * max_coverage
    ) %>%
    # where the number of vaccinations exceeds the maximum population coverage, cap it
    mutate(
      # compute extra doses
      dose_1_extra = pmax(0, (dose_1_AstraZeneca + dose_1_Pfizer) - accepting_population),
      dose_2_extra = pmax(0, (dose_2_AstraZeneca + dose_2_Pfizer) - accepting_population)
    )
  
  # compute fraction of doses that are Pfizer - *as at the date the
  # population was saturated* otherwise it will keep adjusting
  # retrospectively
  air_final_fraction_dose_1 <- air_saturated %>%
    filter(
      dose_1_extra == 0
    ) %>%
    group_by(lga, age_air_80, hypothetical_max_coverage) %>%
    filter(
      date == max(date)
    ) %>%
    ungroup() %>%
    mutate(
      dose_1_Pfizer_fraction = dose_1_Pfizer / (dose_1_AstraZeneca + dose_1_Pfizer),
    ) %>%
    select(
      lga, age_air_80, hypothetical_max_coverage, dose_1_Pfizer_fraction 
    )
  
  air_final_fraction_dose_2 <- air_saturated %>%
    filter(
      dose_2_extra == 0
    ) %>%
    group_by(lga, age_air_80, hypothetical_max_coverage) %>%
    filter(
      date == max(date)
    ) %>%
    ungroup() %>%
    mutate(
      dose_2_Pfizer_fraction = dose_2_Pfizer / (dose_2_AstraZeneca + dose_2_Pfizer)
    ) %>%
    select(
      lga, age_air_80, hypothetical_max_coverage, dose_2_Pfizer_fraction 
    )
  
  # cap these extra doses, keeping the allocation between AZ and Pfizer constant after saturation   
  air <- air_saturated %>%
    left_join(
      air_final_fraction_dose_1,
      by = c("lga", "age_air_80", "hypothetical_max_coverage")
    ) %>%
    left_join(
      air_final_fraction_dose_2,
      by = c("lga", "age_air_80", "hypothetical_max_coverage")
    ) %>%
    mutate(
      
      # fill in any missing fractions (those that have not yet reached saturation)
      dose_1_Pfizer_fraction = replace_na(dose_1_Pfizer_fraction, 0),
      dose_2_Pfizer_fraction = replace_na(dose_2_Pfizer_fraction, 0),
      
      # compute the number of Pfizer doses at saturation
      dose_1_Pfizer_maximum = (accepting_population * dose_1_Pfizer_fraction),
      dose_1_AstraZeneca_maximum = (accepting_population * (1 - dose_1_Pfizer_fraction)),
      dose_2_Pfizer_maximum = (accepting_population * dose_2_Pfizer_fraction),
      dose_2_AstraZeneca_maximum = (accepting_population * (1 - dose_2_Pfizer_fraction)),
      
      # compute the numbers of excess doses
      dose_1_Pfizer_extra = pmax(0, dose_1_Pfizer - dose_1_Pfizer_maximum),
      dose_1_AstraZeneca_extra = pmax(0, dose_1_AstraZeneca - dose_1_AstraZeneca_maximum),
      dose_2_Pfizer_extra = pmax(0, dose_2_Pfizer - dose_2_Pfizer_maximum),
      dose_2_AstraZeneca_extra = pmax(0, dose_2_AstraZeneca - dose_2_AstraZeneca_maximum),
      
      # remove the excess doses
      dose_1_Pfizer = dose_1_Pfizer - dose_1_Pfizer_extra,
      dose_1_AstraZeneca = dose_1_AstraZeneca - dose_1_AstraZeneca_extra,
      dose_2_Pfizer = dose_2_Pfizer - dose_2_Pfizer_extra,
      dose_2_AstraZeneca = dose_2_AstraZeneca - dose_2_AstraZeneca_extra
      
    )
  
  air %>%
    mutate(scenario = scenario_name) %>%
    select(
      -ends_with("maximum")
    )
  
}
