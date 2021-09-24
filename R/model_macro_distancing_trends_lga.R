#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param macrodistancing_model_sims
#' @return
#' @author Nicholas Tierney
#' @export
model_macro_distancing_trends_lga <- function(macrodistancing_model_sims) {


  sim <- macrodistancing_model_sims$sim
  location_change_trends <- macrodistancing_model_sims$location_change_trends
  
  pred_sim <- sim[[1]][, , 1]
  quants <- t(apply(pred_sim, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
  colnames(quants) <- c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")
  
  # predicted trends for downstream modelling
  pred_trend <- location_change_trends %>%
    select(date, state) %>%
    # add predictions
    mutate(mean = colMeans(pred_sim)) %>%
    bind_cols(as_tibble(quants))
  
  pred_trend

  
}
