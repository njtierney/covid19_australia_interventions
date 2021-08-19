#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_survey_nsw_summarised_lga_concern_all_and_16_29
#' @return
#' @author Nicholas Tierney
#' @export
gg_average_physical_distancing_for_16_29_vs_all <- function(contact_survey_nsw_summarised_lga_concern_all_and_16_29) {

  gg_contact_survey(
    data = contact_survey_nsw_summarised_lga_concern_all_and_16_29,
    y = phys_dist_always_weight,
    colour = which_popn,
    facet = lga_of_concern
  ) +
    labs(title = "Weighted proportion of people always physical distancing",
         subtitle = "Comparing a subset of age groups (16-29) to all (16-100+), and LGA of concern\nPoint size corresponds to sample size",
         x = "Date (aggregated weekly)",
         y = "Proportion Physically Distanced",
         colour = "Age Group") +
    ylim(c(0,1))

}
