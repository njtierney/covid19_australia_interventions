#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
#' @return
#' @author Nicholas Tierney
#' @export
gam_rerun <- function(df) {

  is_term_in <- mobility_gam_error_summary %>% 
    select(-lga,
           -datastream,
           -error_message,
           -contains_one_level,
           -nrow) %>% 
    remove_n_distinct_name() %>% 
    select(-trend) %>% 
    map_dfr(.f = function(x) x != 1)
  
  is_term_in %>% 
    slice(1) %>% 
    make_true_into_expression()
  
  safe_make_true_into_expression <- safely(make_true_into_expression)
  
  term_expr <- is_term_in %>% 
    rowwise() %>% 
    group_split() %>% 
    map(safe_make_true_into_expression) 
    
  is_term_in %>% 
    mutate(formula = term_expr,
           result = map(term_expr, pluck, "result"),
           error = map(term_expr, pluck, "error")) 
    
  # summarise_error_reasons(df)
  
    gam_formula <- function(formula, df){
      gam(
        formula = formula,
        select = TRUE,
        gamma = 2,
        data = df
      )
    }

}
