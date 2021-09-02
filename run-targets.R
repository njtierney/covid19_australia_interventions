library(targets)

tar_make(
  c(reff_trend_csv,
    vaccine_effect,
    reff_trend_vaccination,
    reff_trend_vaccination_csv,
    reff_trend_vaccination_prep_plots,
    reff_trend_vaccination_plots,
    write_reff_trend_vaccination_plots)
)
