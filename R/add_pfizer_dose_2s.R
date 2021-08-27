# given daily dose 1 allocations, assign their dose 2 allocations 'dose_interval' days into the future
add_pfizer_dose_2s <- function(
  dose_1s,
  dose_interval = 8 * 7
) {
  
  if (is.finite(dose_interval)) {
    # add 670K more dose 2s delivered some weeks after this to get the full scenario
    dose_2s <- dose_1s %>%
      mutate(
        dose_2_Pfizer = dose_1_Pfizer,
        dose_1_Pfizer = 0,
        date = date + dose_interval
      )
  } else{
    dose_2s <- NULL
  }
  
  bind_rows(
    dose_1s,
    dose_2s
  )
  
}
