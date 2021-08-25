#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_error
#' @return
#' @author Nicholas Tierney
#' @export
mobility_refit_gam <- function(mobility_gam_which_error) {

  test_data <- mobility_gam_which_error %>%
    mutate(data_nrow = map_int(data, nrow)) %>% 
    arrange(-data_nrow) %>% 
    slice(3) %>%
    unnest(cols = data)
  
  # Bega Valley - Google: time at residential
  # only has intervention data for interventions 2 and 4
  # is_a_holiday, is_a_school_holiday, and holiday are the same
  # but once removing the effect of s(holiday, bs = "re")
  # and changing k = 41, this model then fit?
  
  # Goulburn Mulwaree - Google: time at parks
  # changing k = 29 got the model to fit
  
  
  # bega valley - "Google: time at grocery and pharmacy"
  # k = 27 worked
  # 
  # unique(test_data$error_message)
  # unique(test_data$datastream)
  # 
  # gam(trend ~
  #       
  #       # smooth variations in mobility
  #       s(date_num, k = 27) +
  #       
  #       # step changes around intervention impositions
  #       intervention_stage +
  #       
  #       # random effect on holidays (different for each holiday, but shrunk
  #       # to an average holiday effect which used to predict into future)
  #       is_a_holiday +
  #       s(holiday, bs = "re") +
  #       
  #       # constant effect for school holidays
  #       is_a_school_holiday +
  #       
  #       # day of the week effect
  #       dow,
  #     
  #     select = TRUE,
  #     gamma = 2,
  #     data = test_data)
  

}
