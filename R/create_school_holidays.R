#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
create_school_holidays <- function() {

  school_holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    )

}
