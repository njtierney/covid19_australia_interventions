#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_with_refit
#' @return
#' @author Nicholas Tierney
#' @export
add_gam_predictions <- function(mobility_gam_with_refit) {

  mobility_gam_with_refit %>% 
    mutate(data_gam_preds = map2(
      .x = model,
      .y = data,
      .f = tidy_gam_mobility_predictions
    )) %>% 
    select(-data)

}
