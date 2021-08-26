#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_fit
#' @return
#' @author Nicholas Tierney
#' @export
add_fitted_upper_lower <- function(mobility_gam_which_fit) {
  mobility_nest_model_fit_add_pred <- mobility_gam_which_fit %>%
    mutate(
      tidy_predictions = map(model, predict, se.fit = TRUE),
      tidy_predictions = map2(.x = tidy_predictions,
                              .y = model,
                              function(.x, .y) {
                                tibble(
                                  fit = .x$fit,
                                  se_fit = .x$se.fit,
                                  sd = sqrt(var(residuals(.y))) + .x$se.fit ^ 2,
                                  fitted_trend = fit,
                                  fitted_trend_upper = fit + sd * qnorm(0.025),
                                  fitted_trend_lower = fit + sd * qnorm(0.975)
                                )
                              })
    )
  
  mobility_nest_model_fit_add_pred
  
}
