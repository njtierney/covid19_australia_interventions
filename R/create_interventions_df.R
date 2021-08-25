#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility
#' @param max_data_date
#' @return
#' @author Nicholas Tierney
#' @export
create_interventions_df <- function(mobility, max_data_date) {

  interventions(end_dates = TRUE) %>%
    filter(
      date <= max_data_date,
      date %in% mobility$date
    ) %>%
    mutate(
      intervention_id = paste0(
        "intervention_",
        match(date, unique(date))
      )
    )

}
