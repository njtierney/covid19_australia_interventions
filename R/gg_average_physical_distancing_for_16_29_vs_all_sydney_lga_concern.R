#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Nicholas Tierney
#' @export
gg_average_physical_distancing_for_16_29_vs_all_sydney_lga_concern <-
  function(data,
           new_labels) {
    gg_contact_survey_physically_distancing(
      data = data,
      colour = which_popn,
      facet = lga_type,
      new_labels = new_labels
    ) +
      labs(
        subtitle = "Comparing a subset of age groups (16-29) to all (16-100+)\nAnd greater sydney LGAs and LGA of concern\nPoint size corresponds to sample size",
        colour = "Age Group"
      )
  }
