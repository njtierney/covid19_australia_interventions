#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
read_google_mobility_data <- function() {
  url <-
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  data <- read_csv(
    url,
    col_types = cols(
      country_region_code = col_character(),
      country_region = col_character(),
      sub_region_1 = col_character(),
      sub_region_2 = col_character(),
      date = col_date(format = "%Y-%m-%d"),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      grocery_and_pharmacy_percent_change_from_baseline = col_double(),
      parks_percent_change_from_baseline = col_double(),
      transit_stations_percent_change_from_baseline = col_double(),
      workplaces_percent_change_from_baseline = col_double(),
      residential_percent_change_from_baseline = col_double(),
      census_fips_code = col_character()
    )
  )
  data %>% filter(country_region == "Australia")
}
