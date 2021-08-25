#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_fitted_nsw
#' @param mobility_fitted_nsw_model_is_fit
#' @return
#' @author Nicholas Tierney
#' @export
identify_lgas_model_not_fit <- function(mobility_fitted,
                                        mobility_fitted_lgas_model_is_fit) {

  setdiff(
  unique(mobility_fitted$lga),
  unique(mobility_fitted_lgas_model_is_fit$lga)
  )

}
