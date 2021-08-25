#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
add_public_holidays <- function(data) {

  public_holidays <- create_public_holidays()
  
    left_join(
      data, 
      public_holidays,
      by = c("state", "date")
    )

}
