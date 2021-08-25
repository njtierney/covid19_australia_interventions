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
      left_join(abs_lga_google_concordance %>% 
                  select(-intersect_area,
                         -lga_area_abs),
                by = "lga") %>% 
      relocate(area,
               LGA_NAME16,
               .before = lga) %>% 
      relocate(weight,
               .before = predicted_trend)
    
    
  }
