#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_error
#' @return
#' @author Nicholas Tierney
#' @export
summarise_error_reasons <- function(mobility_gam_which_error) {

  
  mobility_gam_which_error %>% 
    mutate(nrow = map_int(data, nrow)) %>% 
    unnest(cols = data) %>% 
    group_by(lga, datastream, error_message, nrow) %>% 
    summarise(
      across(
        .cols = c(date_num,
                  trend,
                  intervention_stage,
                  is_a_holiday,
                  holiday,
                  is_a_school_holiday,
                  dow),
        .fns = list(
          n_distinct = n_distinct
          ),
        .names = "{.fn}_{.col}"
      )
    ) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(contains_one_level = any(
      across(where(is.integer))
      == 1),
      .after = error_message) %>% 
    ungroup() %>% 
    arrange(contains_one_level)

}
