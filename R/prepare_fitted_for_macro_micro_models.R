#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_fitted_nsw_concordance
#' @return
#' @author Nicholas Tierney
#' @export
prepare_fitted_for_macro_micro_models <- function(
  mobility_fitted_nsw_concordance
  ) {
    mobility_fitted_nsw_concordance %>%
      filter(grepl("^Google: ", datastream)) %>%
      mutate(
        change = 1 + (predicted_trend / 100),
        lga_datastream = paste(LGA_NAME16, datastream)
      ) %>%
      select(lga_datastream,
             state = state_long,
             datastream,
             change,
             date) %>% 
    mutate(location = case_when(
      datastream == "Google: time at residential" ~ "home",
      datastream == "Google: time at transit stations" ~ "transit",
      datastream == "Google: time at parks" ~ "public",
      datastream == "Google: time at workplaces" ~ "work",
      datastream == "Google: time at retail and recreation" ~ "retail",
      TRUE ~ "other"
    )) %>%
    filter(location != "other") %>%
    mutate(
      lga = str_remove(
        lga_datastream,
        paste0("\\s", datastream)
      )
    ) %>%
    select(-lga_datastream, -datastream, -state) %>% 
    arrange(
      lga,
      date
    ) %>%
    pivot_wider(
      names_from = location,
      values_from = change
    ) %>%
    relocate(
      lga,
      .before = everything()
    ) %>%
    mutate_at(
      vars(public, home, retail, transit, work),
      ~replace_na(., 1)
    ) %>%
    # location indicator needs to be called state for the prediction function to work
    rename(state = lga)
  
    
  }
