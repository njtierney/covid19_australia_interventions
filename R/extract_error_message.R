#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
extract_error_message <- function(data, x) {

   data %>% 
    mutate(error_message = map_chr({{ x }}, glue_collapse_null))

}
