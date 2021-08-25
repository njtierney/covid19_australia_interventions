#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
create_public_holidays <- function() {

  holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    ) %>%
    rename(
      holiday = name
    )

}
