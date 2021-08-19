#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_survey_nsw_summarised_lga_concern_all_and_16_29
#' @return
#' @author Nicholas Tierney
#' @export
gg_average_contact_rates_for_16_29_vs_all <- function(contact_survey_nsw_summarised_lga_concern_all_and_16_29) {

  gg_contact_survey(
    data = contact_survey_nsw_summarised_lga_concern_all_and_16_29,
    y = avg_contact_num_weight,
    colour = which_popn,
    facet = lga_of_concern
  ) +
    labs(title = "Weighted average number of contacts",
         subtitle = "Comparing a subset of age groups (16-29) to all (16-100+), and LGA of concern\nPoint size corresponds to sample size",
         x = "Date (aggregated weekly)",
         y = "Weighted avg. # contacts",
         colour = "Age Group") 

}
