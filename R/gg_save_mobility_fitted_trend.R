#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot_mobility_trend
#' @return
#' @author Nicholas Tierney
#' @export
gg_save_mobility_fitted_trend <- function(plot_mobility_trend,
                                          dpi = 150,
                                          lga_of_interest,
                                          last_date) {

  ggsave(
    filename = glue(
      "outputs/nsw/NSW_{this_lga}_datastream_model_fit_{last_date}.png"
      ),
    width = 1500 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )

}
