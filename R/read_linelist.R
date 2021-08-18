#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param path
#' @return
#' @author Nicholas Tierney
#' @export
read_linelist <- function(path = "~/not_synced/COVID-19 UoM 18Aug2021 1405.xlsx") {

  read_excel(path = path,
             .name_repair = make_clean_names)  %>%
    mutate(
      true_onset_date = excel_numeric_to_date(parse_number(true_onset_date)),
      specimen_date = excel_numeric_to_date(parse_number(specimen_date)),
      age_at_onset = parse_number(age_at_onset)
    )
  
}
