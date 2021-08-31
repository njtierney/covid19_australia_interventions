#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param reff_effect
#' @param vaccine_effect
#' @return
#' @author Nicholas Tierney
#' @export
create_reff_trend_vaccination <- function(reff_trend, vaccine_effect) {

  lga_lookup <- tibble::tribble(
    ~lga_reff, ~lga_vaccine,
    "Canterbury", "Canterbury-Bankstown",
    "Bankstown", "Canterbury-Bankstown",
    "MidCoast", "Mid-Coast",
    "Strathfield Municipal", "Strathfield",
    "Sutherland", "Sutherland Shire",
    "The Hills", "The Hills Shire",
    "Woolahra Municipal", "Woolahra"
  )
  
  reff_trend_vaccination <- reff_trend %>%
    left_join(
      lga_lookup,
      by = c("lga" = "lga_reff")
    ) %>%
    mutate(
      lga_vaccine = case_when(
        is.na(lga_vaccine) ~ lga,
        TRUE ~ lga_vaccine
      )
    ) %>%
    filter(
      lga_vaccine %in% vaccine_effect$lga
    ) %>%
    inner_join(
      vaccine_effect,
      by = c("lga_vaccine" = "lga", "date")
    ) %>%
    mutate(
      across(
        c(mean, starts_with("ci")),
        ~ . * vaccination_transmission_multiplier
      )
    ) %>%
    select(
      -starts_with("vaccination")
    )
  
  reff_trend_vaccination

}
