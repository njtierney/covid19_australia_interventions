#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
remove_n_distinct_name <- function(x){
  set_names(x, str_remove(names(x), "n_distinct_"))
}
