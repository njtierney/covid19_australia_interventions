#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nicholas Tierney
#' @export
make_true_into_expression <- function(x){
  x %>% 
    as_vector() %>% 
    keep(isTRUE) %>% 
    names() %>% 
    str_replace_all(pattern = "date_num", "s(date_num)") %>% 
    str_replace_all(pattern = "^holiday$", "s(holiday, bs = 're')") %>% 
    rlang::syms() %>%
    purrr::reduce(
      .x = .,
      .f = ~ rlang::expr(!!.x + !!.y)
    )
}