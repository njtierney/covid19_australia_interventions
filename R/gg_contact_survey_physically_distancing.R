#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param colour
#' @param facet
#' @return
#' @author Nicholas Tierney
#' @export
gg_contact_survey_physically_distancing <- function(data,
                                                    colour, 
                                                    facet,
                                                    new_labels = NULL) {

  gg_contact_survey(
    data = data,
    y = phys_not_distancing_weight,
    colour = {{ colour }},
    facet = {{ facet }},
    new_labels = new_labels
  ) +
    labs(title = "Weighted percentage of people not always adhering to 1.5m rule",
         x = "Date (aggregated weekly)",
         y = "% people not always adhering to 1.5m rule") +
    scale_y_continuous(labels = label_percent(),
                       limits = c(0, 1))

}
