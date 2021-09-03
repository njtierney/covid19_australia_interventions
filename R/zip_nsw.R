#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param write_reff_trend_vaccination_plots
#' @param reff_trend_vaccination_csv
#' @param reff_trend_csv
#' @param macro_distancing_trends_lga_csv
#' @param write_mobility_fitted_ggplot
#' @return
#' @author Nicholas Tierney
#' @export
zip_nsw <- function(write_reff_trend_vaccination_plots,
                    reff_trend_vaccination_csv,
                    reff_trend_csv,
                    macro_distancing_trends_lga_csv,
                    write_mobility_fitted_ggplot) {
  
  zip(
    zipfile = here("outputs/nsw/nsw-zip.zip"),
    files = c(write_reff_trend_vaccination_plots,
              reff_trend_vaccination_csv,
              reff_trend_csv,
              macro_distancing_trends_lga_csv,
              write_mobility_fitted_ggplot),
    root = here(),
    mode = "cherry-pick"
  )
  
  here("outputs/nsw/nsw-zip.zip")
  
}
