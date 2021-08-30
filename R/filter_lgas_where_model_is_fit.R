#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param google_mobility_fitted_nsw
#' @return
#' @author Nicholas Tierney
#' @export
filter_lgas_where_model_is_fit <- function(mobility_fitted_nsw) {

  lgas_to_keep <- mobility_fitted_nsw %>%
    filter(!is.na(datastream)) %>%
    select(lga, date, datastream, predicted_trend) %>%
    group_by(lga, date) %>%
    pivot_wider(
      names_from = datastream,
      values_from = predicted_trend
    ) %>%
    filter(
      !is.na(`Google: time at parks`),
      !is.na(`Google: time at residential`),
      !is.na(`Google: time at retail and recreation`),
      !is.na(`Google: time at transit stations`),
      !is.na(`Google: time at workplaces`)
    ) %>%
    pull(lga) %>%
    unique()
  
  
  datastreams_to_keep <- c(
    "Google: time at parks",
    "Google: time at residential",
    "Google: time at retail and recreation",
    "Google: time at transit stations",
    "Google: time at workplaces"
  )
  
  mobility_fitted_nsw %>%
    filter(
      datastream %in% datastreams_to_keep,
      lga %in% lgas_to_keep
    ) %>%
    arrange(lga, date)
  

}
