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
run_macrodistancing_model_sims <- function(location_change_trends, macro_model) {

  # replace location_change_trends with the new data
  macro_model$data$location_change_trends <- location_change_trends
  
  macro_predictions <- macrodistancing_model(macro_model$data, macro_model$params)
  OC_t_state <- macro_predictions$mean_daily_contacts
  
  sdlog <- macro_model$out$sdlog
  
  sim <- calculate(c(OC_t_state), sdlog, values = macro_model$draws, nsim = 1000)
  
  return(
    list(
      sim = sim,
      location_change_trends = macro_model$data$location_change_trends
    )
  )

}
