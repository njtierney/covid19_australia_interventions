#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
covid_pal <- function() {
  # colours for plotting
  green <- brewer.pal(8, "Set2")[1]
  list(
    blue = "steelblue3",
    purple = "#C3A0E8",
    green = green,
    yellow = brewer.pal(8, "Set2")[6],
    blue_green = colorRampPalette(c("blue", green))(10)[8],
    yellow_green = colorRampPalette(c("yellow", green))(10)[8],
    orange = brewer.pal(8, "Set2")[2],
    pink = brewer.pal(8, "Set2")[4],
    fifo = "#A8EB12"
  )
}
