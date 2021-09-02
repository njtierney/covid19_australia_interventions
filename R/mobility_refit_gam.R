#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_error
#' @return
#' @author Nicholas Tierney
#' @export
mobility_refit_gam <- function(mobility_gam_which_error,
                               mobility_gam_error_summary) {
  
  safe_re_fit_gam_mobility <- safely(re_fit_gam_mobility)
  
  # only fit those that don't have one level in the data
  gam_model_fit <- mobility_gam_error_summary %>% 
    select(lga:contains_one_level) %>% 
    filter(!contains_one_level) %>% 
    left_join(mobility_gam_which_error,
              by = c("lga", "datastream", "error_message")) %>% 
    select(-error_message) %>% 
    mutate(nrow = map_int(data, nrow)) %>% 
    arrange(-nrow) %>% 
    mutate(model_fit = future_map(data, safe_re_fit_gam_mobility)) %>% 
    ungroup() %>%
    extract_model_result_error(model_fit) %>% 
    filter(did_fit)
  
  gam_model_fit
  
}
