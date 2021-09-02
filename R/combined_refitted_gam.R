#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_gam_which_fit
#' @param mobility_gam_which_error
#' @return
#' @author Nicholas Tierney
#' @export
combined_refitted_gam <- function(mobility_gam_which_fit,
                                  mobility_gam_refit_error) {

  bind_rows(fit = mobility_gam_which_fit,
            refit = mobility_gam_refit_error,
            .id = "was_refit") %>% 
    select(-contains_one_level,
           -nrow)
  
}
