#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param location_change_trends
#' @param macro_model
#' @return
#' @author Nicholas Tierney
#' @export
model_macro_distancing_trends_lga <- function(location_change_trends,
                                              macro_model) {

  
  # replace location_change_trends with the new data
  macro_model$data$location_change_trends <- location_change_trends
  
  macro_predictions <- macrodistancing_model(macro_model$data, macro_model$params)
  OC_t_state <- macro_predictions$mean_daily_contacts
  pred_sim <- calculate(c(OC_t_state), values = macro_model$draws, nsim = 1000)[[1]][, , 1]
  quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
  colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")
  
  # predicted trends for downstream modelling
  pred_trend <- macro_model$data$location_change_trends %>%
    select(date, state) %>%
    # add predictions
    mutate(mean = colMeans(pred_sim)) %>%
    bind_cols(as_tibble(quants))
  
  pred_trend

}
