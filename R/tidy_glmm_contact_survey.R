#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_survey_fitted_glmm
#' @param contact_survey_glmm_prepped
#' @return
#' @author Nicholas Tierney
#' @export
augment_glmm_contact_survey <- function(contact_survey_fitted_glmm,
                                     contact_survey_glmm_prepped) {
  
  
  augment(x = contact_survey_fitted_glmm,
          newdata = contact_survey_glmm_prepped)
  # tibble(
  #   lga = sort(unique(contact_survey_glmm_prepped$lga))
  # ) %>%
  #   mutate(
  #     proportion_distancing = predict(contact_survey_fitted_glmm, 
  #                                     newdata = ., 
  #                                     type = "response")
  #   )
  
  

}
