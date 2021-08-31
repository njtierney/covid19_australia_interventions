#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Nicholas Tierney
#' @export
gg_reff_trend_vaccination_plots <- function(data) {

  data %>%
    ggplot(
      aes(
        x = date,
        y = mean,
        linetype = forecast
      )
    ) +
    geom_ribbon(
      aes(
        ymax = ci_90_hi,
        ymin = ci_90_lo
      ),
      fill = green,
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(
        ymax = ci_50_hi,
        ymin = ci_50_lo
      ),
      fill = green,
      alpha = 0.2
    ) +
    geom_line(
      aes(y = ci_90_lo),
      colour = green,
      alpha = 0.8
    ) + 
    geom_line(
      aes(y = ci_90_hi),
      colour = green,
      alpha = 0.8
    ) +
    geom_hline(
      yintercept = 1,
      linetype = 2,
      colour = grey(0.5)
    ) +
    geom_vline(
      aes(xintercept = date),
      data = interventions() %>%
        filter(state == "NSW"),
      colour = "grey75"
    ) +
    geom_vline(
      data = prop_variant_dates(),
      aes(xintercept = date),
      colour = "firebrick1",
      linetype = 5
    ) +
    facet_grid(
      scenario ~ coverage_scenario,
      switch = "y"
    ) +
    ylab("Transmission potential") +
    xlab("") +
    ggtitle(this_lga) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%e/%m"
    ) +
    # coord_cartesian(xlim = c(as.Date("2021-01-27"), last_date)) +
    theme_cowplot() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      # strip.placement = "outside"
    )

}
