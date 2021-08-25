#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility
#' @return
#' @author Nicholas Tierney
#' @export
create_empty_df <- function(mobility, all_dates) {

  tibble(
    state_long = mobility$state_long[1],
    state = mobility$state[1],
    lga = mobility$lga[1],
    date = all_dates,
    predicted_trend = NA,
    datastream = mobility$datastream[1],
    fitted_trend = NA,
    fitted_trend_lower = NA,
    fitted_trend_upper = NA
  ) %>%
    left_join(
      mobility,
      by = c("lga", "date", "datastream", "state_long", "state")
    ) %>%
    relocate(
      trend,
      .after = "datastream"
    )

}
