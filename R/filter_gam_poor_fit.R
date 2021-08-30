#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_error
#' @return
#' @author Nicholas Tierney
#' @export
filter_gam_poor_fit <- function(mobility_gam_which_error) {

  # 
  # lgas_to_keep <- mobility_gam_which_error %>%
  #   filter(!is.na(datastream)) %>%
  #   unnest(cols = data) %>% 
  #   select(lga, date, datastream, predicted_trend) %>%
  #   group_by(lga, date) %>%
  #   pivot_wider(
  #     names_from = datastream,
  #     values_from = predicted_trend
  #   ) %>%
  #   filter(
  #     !is.na(`Google: time at parks`),
  #     !is.na(`Google: time at residential`),
  #     !is.na(`Google: time at retail and recreation`),
  #     !is.na(`Google: time at transit stations`),
  #     !is.na(`Google: time at workplaces`)
  #   ) %>%
  #   pull(lga) %>%
  #   unique()
  # 
  # 
  # datastreams_to_keep <- c(
  #   "Google: time at parks",
  #   "Google: time at residential",
  #   "Google: time at retail and recreation",
  #   "Google: time at transit stations",
  #   "Google: time at workplaces"
  # )
  # 
  # mobility_fitted_nsw %>%
  #   filter(
  #     datastream %in% datastreams_to_keep,
  #     lga %in% lgas_to_keep
  #   ) %>%
  #   arrange(lga, date)
  # 

}
