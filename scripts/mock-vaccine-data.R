source("./packages.R")
bottom_age <- seq(from = 10,
                  to = 99,
                  by = c(5))

top_age <- seq(from = 14,
               to = 99,
               by = c(5))

age_group_vec <- c(glue("{bottom_age}-{top_age}"), "100+")

vacc_data <- expand_grid(
  vaccine_type = c("az", "pfizer"),
  age_group = age_group_vec,
  week = c("1-20",
           21:25),
  state_territory = c("invalid",
                      "act",
                      "nsw",
                      "nt",
                      "qld",
                      "sa",
                      "tas",
                      "vic",
                      "wa"),
  dose = c("dose_1",
           "dose_2")
) %>% 
  mutate(count = rpois(n(), lambda = 245))

vacc_data