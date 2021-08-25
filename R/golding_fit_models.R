# get mobility change models for LGAs
golding_fit_models <- function(){
url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
data <- readr::read_csv(
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

lga_data <- data %>%
  filter(
    country_region == "Australia" & sub_region_1 == "New South Wales" & !is.na(sub_region_2)
  ) %>%
  tidyr::pivot_longer(
    ends_with("_percent_change_from_baseline"),
    names_to = "category",
    values_to = "trend"
  ) %>%
  dplyr::select(
    lga = sub_region_2,
    category = category,
    date = date,
    trend = trend
  ) %>%
  mutate(
    category = str_remove_all(category, "_percent_change_from_baseline"),
    category = str_replace_all(category, "_", " ")
  ) %>%
  # tidy up LGA names
  mutate(
    lga = str_remove(lga, "The Council of the City of "),
    lga = str_remove(lga, "The Council of the Municipality of "),
    lga = str_remove(lga, "The Council of the Shire of "),
    lga = str_remove(lga, "Council of the City of "),
    lga = str_remove(lga, "City of "),
    lga = str_remove(lga, "\\sCity|\\sShire"),
    lga = str_remove(lga, "\\sCouncil"),
  ) %>%
  mutate(
    datastream = str_c("Google: time at ", category)
  ) %>%
  dplyr::select(-category)

n_weeks_ahead <- 6
first_date <- min(lga_data$date)
last_date <- max(lga_data$date)

mobility_fitted <- lga_data %>%
  filter(
    !is.na(lga) & !is.na(trend)
  ) %>%
  mutate(
    state_long = "New South Wales",
    state = "NSW"
  ) %>%
  group_by(lga, datastream) %>%
  do(
    golding_predict_mobility_trend(
      .,
      min_date = first_date,
      max_date = last_date + 7 * n_weeks_ahead
    )
  ) %>%
  ungroup()

# keep only the LGAs where we managed to fit a model (others have too-small
# sample sizes for Google to provide data on the metrics we care about)
lgas_to_keep <- mobility_fitted %>%
  filter(!is.na(datastream)) %>%
  select(lga, date, datastream, predicted_trend) %>%
  group_by(lga, date) %>%
  pivot_wider(
    names_from = datastream,
    values_from = predicted_trend
  ) %>%
  filter(
    !is.na(`Google: time at parks`),
    !is.na(`Google: time at residential`),
    !is.na(`Google: time at retail and recreation`),
    !is.na(`Google: time at transit stations`),
    !is.na(`Google: time at workplaces`)
  ) %>%
  pull(lga) %>%
  unique()

# the datastreams we need to predict from the contact rate model
datastreams_to_keep <- c(
  "Google: time at parks",
  "Google: time at residential",
  "Google: time at retail and recreation",
  "Google: time at transit stations",
  "Google: time at workplaces"
)

mobility_fitted_raw <- mobility_fitted
mobility_fitted <- mobility_fitted %>%
  filter(
    datastream %in% datastreams_to_keep,
    lga %in% lgas_to_keep
  ) %>%
  arrange(lga, date)

mobility_fitted
}
