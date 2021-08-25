#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility
#' @param max_data_date
#' @param all_dates
#' @return
#' @author Nicholas Tierney
#' @export
create_intervention_steps <- function(mobility, all_dates) {

  max_data_date <- max(mobility$date)
  interventions <- create_interventions_df(mobility, max_data_date)
  
  expand_dates_add_intervention_stage(
    data = interventions, 
    all_dates = all_dates
  )

}
