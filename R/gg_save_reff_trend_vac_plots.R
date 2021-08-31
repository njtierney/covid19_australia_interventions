#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param reff_trend_vaccination_plots
#' @return
#' @author Nicholas Tierney
#' @export
gg_save_reff_trend_vac_plots <- function(reff_trend_vaccination_plots) {
  
    walk2(
      .x = reff_trend_vaccination_plots$path,
      .y = reff_trend_vaccination_plots$plot,
      .f = ~ggsave(
        filename = .x,
        plot = .y,
        width = 10,
        height = 5,
        # dpi = dpi,
        # scale = 1.2,
        bg = "white"
      )
    )
  
  reff_trend_vaccination_plots$path
  
}
