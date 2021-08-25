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
  function(mobility_holidays_interventions) {
    safe_fit_gam_mobility <- safely(fit_gam_mobility)
    
    gam_model_fit <- mobility_holidays_interventions %>%
      group_by(lga, datastream) %>%
      nest() %>%
      mutate(model_fit = future_map(data, safe_fit_gam_mobility))
    
    gam_model_fit %>%
      ungroup() %>%
      mutate(
        model = map(model_fit, pluck, "result"),
        error = map(model_fit, pluck, "error"),
        did_error = map_lgl(error, negate(is.null)),
        did_fit = map_lgl(model, negate(is.null))
      ) %>%
      select(-model_fit)
    
  }
