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
gg_contact_survey_contact_rates <- function(data, 
                                            colour,
                                            facet,
                                            new_labels = NULL) {

  gg_contact_survey(
    data = data,
    y = avg_contact_num_weight,
    colour = {{ colour }},
    facet = {{ facet }},
    new_labels = new_labels
  ) +
    labs(
      title = "Weighted average number of contacts",
      x = "Date (aggregated weekly)",
      y = "Weighted avg. # contacts",
    )

}
