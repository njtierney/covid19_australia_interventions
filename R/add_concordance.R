#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_fitted_nsw
#' @param mobility_data_lga_table
#' @param abs_lga_google_concordance
#' @return
#' @author Nicholas Tierney
#' @export
add_concordance <-
  function(mobility_fitted_nsw,
           abs_lga_google_concordance) {
    mobility_fitted_nsw %>%
      left_join(
        abs_lga_google_concordance %>%
          select(-intersect_area,
                 -lga_area_abs),
        by = c("lga" = "google_lga")
      ) %>%
      relocate(area,
               LGA_NAME16,
               .before = lga) %>%
      relocate(weight,
               .before = predicted_trend) %>%
      # should happen earlier in concordance
      mutate(weight = as.numeric(weight)) %>% 
      group_by(LGA_NAME16, datastream, date) %>%
      summarise(across(
        c(
          predicted_trend,
          trend,
          fitted_trend,
          fitted_trend_lower,
          fitted_trend_upper
        ),
        .fns = ~weighted.mean(x = .x,
                              w = weight,
                              na.rm = TRUE))) %>% 
      ungroup()
    
  }
