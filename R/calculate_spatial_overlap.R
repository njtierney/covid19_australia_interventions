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
                                      abs_shape,
                                      mobility_data_lga_table) {
  
  
  intersect_pct <- st_intersection(google_shape, abs_shape) %>% 
    mutate(intersect_area = st_area(.)) %>% 
    select(area, LGA_NAME16, intersect_area) %>% 
    st_drop_geometry()
  
  # Create a fresh area variable for counties
  abs_shape_areas <- abs_shape %>% 
    mutate(lga_area_abs = st_area(.)) %>% 
    select(lga_area_abs, LGA_NAME16) %>% 
    st_drop_geometry()
  
  concordance <- intersect_pct %>% 
    left_join(abs_shape_areas, 
              by = "LGA_NAME16") %>% 
    mutate(weight = intersect_area / lga_area_abs)
  
  
  concordance %>% 
    left_join(mobility_data_lga_table,
              by = c("area" = "google_original_lga")) %>% 
    relocate(google_lga, .before = area) %>% 
    group_by(LGA_NAME16) %>% 
    # not 100% overlap, so we need to normalise the weights
    mutate(weight = weight / sum(weight))
  
}
