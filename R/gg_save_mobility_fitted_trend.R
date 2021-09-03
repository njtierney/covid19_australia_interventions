#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot_mobility_trend
#' @return
#' @author Nicholas Tierney
#' @export
gg_save_mobility_fitted_trend <- function(df_mobility_fitted_trend_plot_paths,
                                          dpi = 150) {
  
  walk2(
    .x = df_mobility_fitted_trend_plot_paths$path,
    .y = df_mobility_fitted_trend_plot_paths$plot,
    .f = ~ggsave(
      filename = .x,
      plot = .y,
      width = 1500 / dpi,
      height = 1000 / dpi,
      dpi = dpi,
      scale = 1.2,
      bg = "white"
    )
  )
  
  df_mobility_fitted_trend_plot_paths$path

}
