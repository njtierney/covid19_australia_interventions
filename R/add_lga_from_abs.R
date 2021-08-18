#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_w_postcodes
#' @return
#' @author Nicholas Tierney
#' @export
add_lga_from_abs <- function(data_w_postcodes, 
                             abs_lga_file =
                               "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx",
                             postcode_col) {

  postcodes <- data_w_postcodes %>% 
    pull({{postcode_col}}) %>% 
    unique()
  
  weights_tbl <- create_abs_lga_weights(
    file = abs_lga_file,
    postcode = postcodes
  )
  
  added_weights <- data_w_postcodes %>% 
    left_join(weights_tbl,
              by = "postcode")
  
  added_weights

}
