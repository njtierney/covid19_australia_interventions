#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
read_vaccine_effect <- function(file_path) {

  
  # load vaccination effect estimates
  vaccine_effect <- read_csv(
    file = file_path,
    col_types = cols(
      lga = col_character(),
      date = col_date(format = ""),
      forecast = col_logical(),
      scenario = col_character(),
      coverage_scenario = col_character(),
      vaccination_transmission_multiplier = col_double(),
      vaccination_transmission_reduction_percent = col_double()
    )
  ) %>%
    mutate(
      lga = str_remove(lga, " \\(A\\)"),
      lga = str_remove(lga, " \\(C\\)"),
      lga = str_remove(lga, " \\(NSW\\)"),
    )
  
  vaccine_effect

}
