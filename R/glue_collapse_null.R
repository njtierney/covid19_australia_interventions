#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @return
#' @author Nicholas Tierney
#' @export
glue_collapse_null <- function(x, ...){
    if (is.null(x)){
      return("")
    } else (
      glue_collapse(x, ...)
    )
  }
