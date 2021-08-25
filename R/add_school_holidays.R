#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
add_school_holidays <- function(data) {
  
  school_holidays <- create_school_holidays()

  left_join(
    data,
    school_holidays,
    by = c("state", "date")
  )

}
