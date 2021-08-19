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
augment_glmm_contact_survey <- function(model,
                                     data) {
  
  tibble(
    lga = sort(unique(data$lga))
  ) %>%
    mutate(
      proportion_distancing = predict(model, 
                                      newdata = ., 
                                      type = "response")
    )


}
