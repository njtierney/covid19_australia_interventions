#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param start_date
#' @param end_date
#' @return
#' @author Nicholas Tierney
#' @export
add_intervention_expanded <- function(data,
                                      start_date,
                                      end_date) {
  
  # this should create a dataset about 4,768 big
  # also we need to add a "intervention stage" column
  # which is a factor, of 0, then 1...2... etc for each lockdown
  intervention_long <- intervention_expanded(
    start_date,
    end_date
  )

  data %>%
    left_join(
      intervention_long,
      by = c("state", "date")
    )
}
