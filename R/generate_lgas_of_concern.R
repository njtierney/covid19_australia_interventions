#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
generate_lgas_of_concern <- function() {

  paste0(c("Bayside",
           "Blacktown",
           "Burwood",
           "Campbelltown",
           "Canterbury-Bankstown",
           "Cumberland",
           "Fairfield",
           "Georges River",
           "Liverpool",
           "Parramatta",
           "Strathfield",
           "Penrith"),
         collapse = "|")

}
