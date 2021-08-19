#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_survey_glmm_prepped
#' @return
#' @author Nicholas Tierney
#' @export
fit_glmm_contact_survey <- function(contact_survey_glmm_prepped) {

  
  model_survey_fit <- glmer(
    prop_phys_dist_always ~ 1 + (1|lga),
    family = binomial,
    weights = weight,
    data = contact_survey_glmm_prepped
  )
  
  model_survey_fit
  
}
