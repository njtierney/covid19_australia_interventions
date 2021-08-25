#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param df
#' @return
#' @author Nicholas Tierney
#' @export
fit_gam_mobility <- function(df) {

  gam(trend ~
        
        # smooth variations in mobility
        # s(date_num, k = 50) +
        s(date_num, k = 48) +
        
        # step changes around intervention impositions
        intervention_stage +
        
        # random effect on holidays (different for each holiday, but shrunk
        # to an average holiday effect which used to predict into future)
        is_a_holiday +
        s(holiday, bs = "re") +
        
        # constant effect for school holidays
        is_a_school_holiday +
        
        # day of the week effect
        dow,
      
      select = TRUE,
      gamma = 2,
      data = df)

}
