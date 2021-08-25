#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_data
#' @return
#' @author Nicholas Tierney
#' @export
create_lga_lookup <- function(mobility_data) {

  mobility_data %>%
    filter(
      country_region == "Australia" & sub_region_1 == "New South Wales" & !is.na(sub_region_2)
    ) %>%
    pivot_longer(
      ends_with("_percent_change_from_baseline"),
      names_to = "category",
      values_to = "trend"
    ) %>% 
    select(
      lga = sub_region_2,
      category = category,
      date = date,
      trend = trend
    ) %>%
    mutate(
      category = str_remove_all(category, "_percent_change_from_baseline"),
      category = str_replace_all(category, "_", " "),
      original_lga = lga,
      lga = str_remove(lga, "The Council of the City of "),
      lga = str_remove(lga, "The Council of the Municipality of "),
      lga = str_remove(lga, "The Council of the Shire of "),
      lga = str_remove(lga, "Council of the City of "),
      lga = str_remove(lga, "City of "),
      lga = str_remove(lga, "\\sCity|\\sShire"),
      lga = str_remove(lga, "\\sCouncil"),
    ) %>%
    select(lga,
           original_lga) %>% 
    distinct() %>% 
    arrange(lga)

}
