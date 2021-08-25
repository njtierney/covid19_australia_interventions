#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
interventions_has_0_rows <- function(mobility) {

  max_data_date <- max(mobility$date)
  interventions_df <- interventions(end_dates = TRUE) %>%
    filter(
      date <= max_data_date,
      date %in% mobility$date
    ) 
  
  nrow(interventions_df) == 0
 
}
