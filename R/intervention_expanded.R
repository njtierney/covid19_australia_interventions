#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @param nameme2
#' @return
#' @author Nicholas Tierney
#' @export
intervention_expanded <- function(start_date,
                                  end_date) {
  intervention_with_number() %>%
    group_by(state) %>%
    complete(date = seq(start_date,
                               end_date,
                               by = "day")) %>%
    fill(intervention_id, .direction = "down") %>%
    ungroup() %>%
    mutate(intervention_id = coalesce(intervention_id, "intervention_0"))

}
