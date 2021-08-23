#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param y
#' @param colour
#' @param facet
#' @return
#' @author Nicholas Tierney
#' @export
gg_contact_survey <-
  function(data = data,
           y = avg_contact_num_weight,
           colour = lga_of_concern,
           size = n_resp_weight,
           facet = lga_of_concern,
           new_labels = NULL) {
    
p <- ggplot(data,
           aes(
             x = date_week,
             y = {{ y }},
             size = {{ size }},
             colour = {{ colour }}
           )) +
      geom_smooth(
        alpha = 0.25,
        se = FALSE,
        size = 0.25,
        alpha = 0.25
      ) +
      geom_point(alpha = 0.75) +
      scale_colour_brewer(palette = "Dark2") +
      theme(legend.position = "bottom") +
      scale_size(guide = "none")

if (is.null(new_labels)){
  p <- p + facet_wrap(facets = vars({{ facet }}))
} else if (!is.null(new_labels)){
  p <- p + facet_wrap(facets = vars({{ facet }}),
                      labeller = new_labels)
}

return(p)

    
  }
