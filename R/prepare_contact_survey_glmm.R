#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_survey_nsw
#' @return
#' @author Nicholas Tierney
#' @export
prepare_contact_survey_glmm <- function(contact_survey_nsw) {
  contact_survey_nsw %>%
    # relocate(postcode, phys_distance, lga, weight, .before = wave) %>%
    select(postcode,
           date_week,
           prop_phys_dist_always,
           lga,
           lga_of_concern,
           weight) %>%
    # filter from July
    filter(month(date_week) >= 7)
  
}
