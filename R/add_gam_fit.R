#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_holidays_interventions
#' @param safe_fit_gam_mobility
#' @return
#' @author Nicholas Tierney
#' @export
add_gam_fit <-
  function(mobility_holidays_interventions, s_k) {
    safe_fit_gam_mobility <- safely(fit_gam_mobility)
    
    gam_model_fit <- mobility_holidays_interventions %>%
      group_by(lga, datastream) %>%
      nest() %>%
      mutate(model_fit = future_map(data, safe_fit_gam_mobility, s_k = s_k))
    
    gam_model_fit %>%
      ungroup() %>%
      extract_model_result_error(model_fit)
    
  }
