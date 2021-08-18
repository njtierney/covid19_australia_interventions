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
    select(postcode, date_week, phys_distance, lga, lga_of_concern, weight) %>%
    group_by(date_week, lga) %>% 
    mutate(n_respondents = n(),
           n_always_distancing = sum(phys_distance == "Always"),
           n_not_distancing = n_respondents - n_always_distancing) %>% 
    # remove duplicates
    distinct() %>% 
    ungroup()

}
