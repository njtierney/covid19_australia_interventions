#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ...
#' @return
#' @author Nicholas Tierney
#' @export
zip_nsw <- function(...) {
  
  zip(
    zipfile = here("outputs/nsw/nsw-zip.zip"),
    files = c(...),
    root = here(),
    mode = "cherry-pick"
  )
  
  here("outputs/nsw/nsw-zip.zip")
  
}
