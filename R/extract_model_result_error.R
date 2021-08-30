#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param model_fit
#' @return
#' @author Nicholas Tierney
#' @export
extract_model_result_error <- function(df, model_fit) {

  df %>% 
  mutate(
    model = map(model_fit, pluck, "result"),
    error = map(model_fit, pluck, "error"),
    did_error = map_lgl(error, negate(is.null)),
    did_fit = map_lgl(model, negate(is.null))
  ) %>% 
    extract_error_message(error) %>% 
    select(-model_fit) 

}
