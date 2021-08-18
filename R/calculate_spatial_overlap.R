#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param shape_1
#' @param shape_2
#' @return
#' @author Nicholas Tierney
#' @export
calculate_spatial_overlap <- function(google_shape,
                                      abs_shape) {
  
  
  abs_shape <- abs_shape %>% filter(STE_NAME16 == "New South Wales")
  google_shape <- google_shape %>% filter(state == "New South Wales")
  
  intersect_pct <- st_intersection(google_shape, abs_shape) %>% 
    mutate(intersect_area = st_area(.))
  
  intersect_pct
  
  
}
