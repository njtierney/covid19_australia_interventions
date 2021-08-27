# compute the number of 12-15 year olds in the stated LGAs, and the fraction
# of them in each of the two AIR age bins
child_pop <- function(lgas) {
  lga_child_pop <- pop_air %>%
    filter(
      age_air %in% c("0-14", "15-29"),
      LGA_NAME19 %in% lgas
    ) %>%
    group_by(age_air) %>%
    summarise(
      population = sum(population)
    ) %>%
    pivot_wider(
      names_from = age_air,
      values_from = population
    ) %>%
    mutate(
      # compute the number of 12-15 year olds
      `12-14` = `0-14` * pop_disagg$fraction_0_14_eligible_child,
      `15` = `15-29` * pop_disagg$fraction_15_29_eligible_child,
      `12-15` = `12-14` + `15`,
      # compute the fraction of each bin that are eligible
      fraction_12_15_in_0_14 = `12-14` / `0-14`,
      fraction_12_15_in_15_29 = `15` / `15-29`,
    )
}
