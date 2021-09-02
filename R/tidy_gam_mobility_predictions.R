#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model
#' @param data
#' @return
#' @author Nicholas Tierney
#' @export
tidy_gam_mobility_predictions <- function(model, data) {

  fit <- predict(model, se.fit = TRUE)
  fit$sd <- sqrt(var(residuals(model)) + fit$se.fit ^ 2)
  
  df_fitted <- data %>%
    # predict with fitted model (and get 90% CIs)
    mutate(
      fitted_trend = fit$fit,
      fitted_trend_upper = fit$fit + fit$sd * qnorm(0.025),
      fitted_trend_lower = fit$fit + fit$sd * qnorm(0.975),
    )
  
  df_fitted 

}
