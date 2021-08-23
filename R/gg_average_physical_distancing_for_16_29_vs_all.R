#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_survey_nsw_summarised_lga_concern_all_and_16_29
#' @return
#' @author Nicholas Tierney
#' @export
gg_average_physical_distancing_for_16_29_vs_all <- function(data) {

  gg_contact_survey_physically_distancing(
    data = data,
    colour = which_popn,
    facet = lga_of_concern
  ) +
    labs(subtitle = "Comparing a subset of age groups (16-29) to all (16-100+), and LGA of concern\nPoint size corresponds to sample size",
         colour = "Age Group")

}
