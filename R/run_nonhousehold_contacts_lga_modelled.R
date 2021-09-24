#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param macrodistancing_model_sims
#' @return
#' @author Nicholas Tierney
#' @export
run_nonhousehold_contacts_lga_modelled <- function(macrodistancing_model_sims) {

  sim <- macrodistancing_model_sims$sim
  location_change_trends <- macrodistancing_model_sims$location_change_trends
  
  # write the meanlog, sdlog, and predicted contacts out for Nic R
  pred_sim <- sim[[1]][, , 1]
  sdlog_sim <- sim$sdlog[, , 1]
  meanlog_sim <- log(pred_sim) - (sdlog_sim ^ 2) / 2
  
  nonhousehold_contacts_lga_modelled <- location_change_trends %>%
    select(date, state) %>%
    # add predictions
    mutate(
      mean_contacts = colMeans(pred_sim),
      meanlog_contacts = colMeans(meanlog_sim),
      sdlog_contacts = mean(sdlog_sim),
      meanlog_contacts_sd = apply(meanlog_sim, 2,sd),
      sdlog_contacts_sd = sd(sdlog_sim)
    ) 
  
  nonhousehold_contacts_lga_modelled

}
