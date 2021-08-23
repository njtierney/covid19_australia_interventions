#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @return
#' @author Nicholas Tierney
#' @export
extract_greater_sydney_lgas <- function() {

  url <- "https://www.nsw.gov.au/covid-19/health-and-wellbeing/disability-resources/local-councils-greater-sydney"
  
  greater_sydney_html <- url %>% bow() %>% scrape()
  
  greater_sydney_lgas <- greater_sydney_html %>% 
    html_nodes(".nsw-col.page__main") %>% 
    html_text2() %>% 
    str_split("\n") %>% 
    pluck(1) %>% 
    purrr::discard(.p = ~nchar(.x) == 0)
  
  greater_sydney_lgas
    
}
