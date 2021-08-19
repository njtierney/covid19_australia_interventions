#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contact_survey_glmm_aug
#' @return
#' @author Nicholas Tierney
#' @export
gg_phys_distance_all_popn <- function(data,
                                      y) {
  
  # cbind(n_always_distancing, n_not_distancing)
  ggplot(data,
         aes(x = date_week,
             y = {{y}},
             size = weight)) +
    geom_point(alpha = 0.75) +
    scale_colour_brewer(palette = "Dark2") +
    facet_wrap(facets = vars(lga_of_concern)) +
    theme(legend.position = "bottom") +
    scale_size(guide = "none") 

}
