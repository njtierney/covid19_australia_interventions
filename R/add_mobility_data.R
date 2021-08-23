#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param google_mobility
#' @return
#' @author Nicholas Tierney
#' @export
add_mobility_data <- function(google_mobility,
                              n_weeks_ahead = 6) {
  
  first_date <- min(google_mobility$date)
  last_date <- max(google_mobility$date)

  google_mobility %>%
    group_by(lga, datastream) %>%
    do(
      predict_mobility_trend(
        .,
        min_date = first_date,
        max_date = last_date + 7 * n_weeks_ahead
      )
    ) %>%
    ungroup()
  

}
