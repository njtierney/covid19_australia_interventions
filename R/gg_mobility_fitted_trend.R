#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_fitted
#' @return
#' @author Nicholas Tierney
#' @export
gg_mobility_fitted_trend <- function(
  mobility_fitted_nsw_concordance,
  last_date = max(mobility_fitted_nsw_concordance$date),
  lga_name
  ){

  # all_lgas <- na.omit(unique(mobility_fitted$lga))
  
    ggplot(data = mobility_fitted_nsw_concordance) +
    aes(date, fitted_trend) +
    geom_hline(
      yintercept = 0,
      colour = "grey80",
      linetype = 3
    ) +
    geom_vline(
      aes(xintercept = date),
      data = interventions() %>%
        filter(state == "NSW"),
      colour = "grey80"
    ) +
    geom_vline(
      aes(xintercept = last_date),
      colour = "grey80",
      linetype = 2
    ) +
    facet_wrap(
      ~datastream,
      ncol = 3,
      scales = "free"
    ) +
    geom_ribbon(
      aes(
        ymin = fitted_trend_lower,
        ymax = fitted_trend_upper
      ),
      fill = grey(0.9),
      colour = grey(0.8),
      size = 0.1
    ) +
    # fitted trend
    geom_line(
      aes(date, fitted_trend),
      colour = "gray40"
    ) +
    geom_point(
      aes(date, trend),
      size = 0.2,
      col = "purple"
    ) +
    coord_cartesian(
      xlim = c(as.Date("2020-03-01"), last_date) # + 7 * n_weeks_ahead)
    ) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b",
      limits = range(mobility_fitted_nsw_concordance$date)
    ) +
    labs(
      title = "Percentage change in selected mobility datastreams",
      subtitle = glue(
        "{lga_name}\n",
        "Up until {format(last_date, format = '%B %d')}, ",
        "{format(last_date, format = '%Y')}"
      ),
      x = "",
      y = ""
    ) +
    theme_cowplot() +
    panel_border(remove = TRUE) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      axis.title.y.right = element_text(vjust = 0.5, angle = 90),
      panel.spacing = unit(1.2, "lines")
    )
}
