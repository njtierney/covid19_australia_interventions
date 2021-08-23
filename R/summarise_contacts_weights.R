#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param groups
#' @return
#' @author Nicholas Tierney
#' @export
summarise_contacts_weights <- function(data, ...) {

  data %>% 
    group_by(date_week,
             ...) %>% 
    summarise(
      n_resp_weight = sum(n()*weight),
      phys_dist_always_weight = weighted.mean(x = prop_phys_dist_always,
                                              w = weight),
      phys_not_distancing_weight = (1 -  phys_dist_always_weight),
      across(
        .cols = contains("contact"),
        .fns = ~weighted.mean(x = .x, w = weight),
        .names = "avg_{.col}_weight"
      )
    )

}
