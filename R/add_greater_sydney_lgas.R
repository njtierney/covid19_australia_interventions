#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param vec_greater_sydney_lgas
#' @return
#' @author Nicholas Tierney
#' @export
add_greater_sydney_lgas <- function(data, vec_greater_sydney_lgas) {

  data %>% 
    mutate(
      greater_sydney_lga = if_else(
        condition = str_detect(lga, vec_greater_sydney_lgas),
        true = "greater_sydney",
        false = "not_greater_sydney"
      ),
      .after = lga
    ) 
  

}
