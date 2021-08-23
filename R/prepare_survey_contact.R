#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pmin_contact_num
#' @param vec_lga_of_concern
#' @return
#' @author Nicholas Tierney
#' @export
prepare_survey_contact <- function(data,
                                   pmin_contact_num = 100,
                                   vec_lga_of_concern,
                                   vec_greater_sydney_lgas) {
  data %>%
    add_date_week() %>%
    filter(
      year == 2021,
      age >= 16
    ) %>%
    mutate(
      contact_num = pmin(pmin_contact_num, contact_num),
      prop_phys_dist_always = phys_distance == "Always"
    ) %>%
    add_lga_of_concern(vec_lgas_of_concern = vec_lga_of_concern) %>%
    add_greater_sydney_lgas(vec_greater_sydney_lgas = vec_greater_sydney_lgas) %>%
    mutate(
      age_16_39 = if_else(
        condition = between(age, 16, 39),
        true = "16-39",
        false = "40+"
      ),
      age_16_29 = if_else(
        condition = between(age, 16, 29),
        true = "16-29",
        false = "30+"
      ),
      .before = lga
    )
}
