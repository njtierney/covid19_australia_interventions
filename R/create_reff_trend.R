#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param location_change_trends
#' @param fitted_reff_model
#' @return
#' @author Nicholas Tierney
#' @export
create_reff_trend <- function(location_change_trends,
                               macro_distancing_trends_lga,
                               fitted_reff_model,
                               macro_model,
                               gi_cdf) {
  
  
  all_dates <- unique(location_change_trends$date)
  n_dates <- length(all_dates)

  ga <- fitted_reff_model$greta_arrays
  nsw_idx <- which(fitted_reff_model$data$states == "NSW")

  # get index between dated for fitted model and for prediction
  start_idx <-
    as.numeric(min(all_dates) - min(fitted_reff_model$data$dates$mobility))
  extra_dates <-
    as.numeric(max(all_dates) - max(fitted_reff_model$data$dates$mobility))
  date_idx <- pmin(start_idx + seq_len(n_dates), n_dates)

  # time in household by LGA
  h_t <- location_change_trends %>%
    select(state, date, home) %>%
    pivot_wider(names_from = state, values_from = home) %>%
    select(-date) %>%
    as.matrix()

  de <- ga$distancing_effect
  infectious_days <- infectious_period(gi_cdf)
  HD_t <- de$HD_0 * h_t

  # non-household contact rates by LGA
  OC_t_lga <- trends_date_state(trends = macro_distancing_trends_lga,
                                dates = all_dates
  )
  # get the probability of not transmitting per unit time, for Delta
  # p_star <- de$p_star[nrow(de$p_star), nsw_idx]

  p_star_nsw <-
    extend(de$p_star[, nsw_idx], n_rows = nrow(de$p_star) + extra_dates)
  p_star_nsw <- p_star_nsw[(start_idx + 1):length(p_star_nsw)]
  p_star_lga <-
    sweep(zeros(nrow(OC_t_lga), ncol(OC_t_lga)), 1, p_star_nsw, FUN = "+")

  # get the microdistancing effect, lining up dates
  gamma_t_nsw <-
    extend(de$gamma_t_state[, nsw_idx], n_rows = nrow(de$gamma_t_state) + extra_dates)
  gamma_t_nsw <- gamma_t_nsw[(start_idx + 1):length(gamma_t_nsw)]
  gamma_t_lga <-
    sweep(zeros(nrow(OC_t_lga), ncol(OC_t_lga)), 1, gamma_t_nsw, FUN = "+")

  household_infections <- de$HC_0 * (1 - p_star_lga^HD_t)
  non_household_infections <-
    OC_t_lga * gamma_t_lga * infectious_days * (1 - p_star_lga^de$OD_0)
  infections_distancing <-
    household_infections + non_household_infections

  surveillance_effect <-
    ga$surveillance_reff_local_reduction[date_idx, nsw_idx]
  extra_isolation_effect <-
    ga$extra_isolation_local_reduction[date_idx, nsw_idx]

  infections <-
    sweep(infections_distancing,
      1,
      surveillance_effect * extra_isolation_effect,
      FUN = "*"
    )

  reff_sims <-
    calculate(c(infections), values = fitted_reff_model$draws, nsim = 1000)[[1]][, , 1]
  quants <-
    t(apply(reff_sims, 2, quantile, c(0.05, 0.25, 0.75, 0.95)))
  colnames(quants) <-
    c("ci_90_lo", "ci_50_lo", "ci_50_hi", "ci_90_hi")

  reff_trend <- macro_model$data$location_change_trends %>%
    select(date, state) %>%
    # add predictions
    mutate(mean = colMeans(reff_sims)) %>%
    bind_cols(as_tibble(quants)) %>%
    rename(lga = state)
  
  reff_trend
}
