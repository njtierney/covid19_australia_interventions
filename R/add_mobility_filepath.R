#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df_mobility_fitted_trend_plots
#' @return
#' @author Nicholas Tierney
#' @export
add_mobility_filepath <- function(df_mobility_fitted_trend_plots) {
  dir_create("outputs/nsw/figs/mobility")
  
  last_date <-
    max(unnest(df_mobility_fitted_trend_plots, cols = data)$date)
  
  df_mobility_fitted_trend_plots %>%
    mutate(path = glue(
      here(
        "outputs/nsw/figs/mobility/NSW_{LGA_NAME16}_datastream_model_fit_{last_date}.png"
      )
    ))
  
}
