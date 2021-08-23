message("loading R packages")
suppressPackageStartupMessages(source("./packages.R"))
source("./conflicts.R")
## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(

  tar_file(
    doh_path,
    path("~/not_synced/survey_data/")
  ),
  tar_file(
    abs_lga_postcodes_path,
    path("~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx")
  ),
  # read in all surveys
  contact_survey = read_doh_add_weights(
    doh_path = doh_path,
    abs_lga_postcodes_path = abs_lga_postcodes_path
  ),
  greater_sydney_lgas = extract_greater_sydney_lgas(),
  lga_of_concern = lgas_of_concern(),
  greater_sydney_without_lga_of_concern = setdiff(greater_sydney_lgas, lga_of_concern),
  contact_survey_nsw = prepare_survey_nsw(
    data = contact_survey,
    pmin_contact_num = 20,
    vec_lgas_concern = paste_or(lga_of_concern),
    vec_greater_sydney_lgas = paste_or(greater_sydney_without_lga_of_concern)
  ),
  contact_survey_nsw_summarised_lga_concern =
    summarise_contacts_weights(
      data = contact_survey_nsw,
      lga_of_concern
    ),
  contact_survey_nsw_summarised_greater_syd =
    summarise_contacts_weights(
      data = contact_survey_nsw,
      greater_sydney_lga
    ),
  contact_survey_nsw_summarised_lga_concern_age_groups =
    summarise_contacts_weights(
      data = contact_survey_nsw,
      lga_of_concern, 
      age_groups
    ),
  contact_survey_nsw_summarised_lga_concern_age_16_39 =
    summarise_contacts_weights(
      data = contact_survey_nsw,
      lga_of_concern, 
      age_16_39
    ),
  contact_survey_nsw_summarised_lga_concern_age_16_29 =
    summarise_contacts_weights(
      data = contact_survey_nsw,
      lga_of_concern, age_16_29
    ),
  contact_survey_nsw_summarised_lga_concern_16_39_only = contact_survey_nsw %>%
    filter(age_16_39 == "16-39") %>%
    summarise_contacts_weights(lga_of_concern),
  
  contact_survey_nsw_summarised_lga_concern_16_29_only = contact_survey_nsw %>%
    filter(age_16_29 == "16-29") %>%
    summarise_contacts_weights(lga_of_concern),
  
  contact_survey_nsw_summarised_greater_syd_16_29_only = contact_survey_nsw %>%
    filter(age_16_29 == "16-29") %>%
    summarise_contacts_weights(greater_sydney_lga),
  
  # updating these plots with the left-hand panel showing LGAs that are not in the 'of concern' list, but are in the greater Sydney list (rather than all LGAs no in the 'of concern' list) ?

  contact_survey_nsw_summarised_lga_concern_all_and_16_39 = bind_rows(
    all_popn = contact_survey_nsw_summarised_lga_concern,
    only_16_39 = contact_survey_nsw_summarised_lga_concern_16_39_only,
    .id = "which_popn"
  ),
  contact_survey_nsw_summarised_lga_concern_all_and_16_29 = bind_rows(
    all_popn = contact_survey_nsw_summarised_lga_concern,
    only_16_29 = contact_survey_nsw_summarised_lga_concern_16_29_only,
    .id = "which_popn"
  ),
  
  contact_survey_nsw_summarised_greater_syd_all_and_16_29 = bind_rows(
    all_popn = contact_survey_nsw_summarised_greater_syd,
    only_16_29 = contact_survey_nsw_summarised_greater_syd_16_29_only,
    .id = "which_popn"
  ),
  
  contact_survey_nsw_summarised_lga_concern_vs_syd_and_16_29_vs_all = bind_rows(
    {
      contact_survey_nsw_summarised_lga_concern_all_and_16_29 %>% 
        filter(lga_of_concern == "lga_of_concern")  %>% 
        rename(lga_type = lga_of_concern)
      },
    {
      contact_survey_nsw_summarised_greater_syd_all_and_16_29 %>% 
        filter(greater_sydney_lga == "greater_sydney") %>% 
        rename(lga_type = greater_sydney_lga)
      }
  ),

  # average contact rates over time by 16-29 vs all ages
  plot_average_contact_rates_for_16_29_vs_all =
    gg_average_contact_rates_for_16_29_vs_all(
      contact_survey_nsw_summarised_lga_concern_all_and_16_29
    ),
  # physical distancing over time by 16-29 vs all ages
  plot_average_physical_distancing_for_16_29_vs_all =
    gg_average_physical_distancing_for_16_29_vs_all(
      contact_survey_nsw_summarised_lga_concern_all_and_16_29
    ),
  
  # for contact_survey_nsw_summarised_lga_concern_vs_syd_and_16_29_vs_all
  
  lga_facet_names = as_labeller(
    c(
      `greater_sydney` = "Greater Sydney LGAs NOT of concern",
      `lga_of_concern` =  "LGAs of concern"
      )
    ),
  
  # average contact rates over time by 16-29 vs all ages
  plot_average_contact_rates_for_16_29_vs_all_sydney_lga_concern =
    gg_average_contact_rates_for_16_29_vs_all_sydney_lga_concern(
      contact_survey_nsw_summarised_lga_concern_vs_syd_and_16_29_vs_all,
      lga_facet_names
    ),
  # physical distancing over time by 16-29 vs all ages
  plot_average_physical_distancing_for_16_29_vs_all_sydney_lga_concern =
    gg_average_physical_distancing_for_16_29_vs_all_sydney_lga_concern(
      contact_survey_nsw_summarised_lga_concern_vs_syd_and_16_29_vs_all,
      lga_facet_names
    ),
  
  
  tar_render(rmd_nsw_contact_survey, "doc/nsw-contact-survey.Rmd"),
  contact_survey_glmm_prepped = prepare_contact_survey_glmm(contact_survey_nsw),
  contact_survey_fitted_glmm =
    fit_glmm_contact_survey(contact_survey_glmm_prepped),
  contact_survey_glmm_aug = augment_glmm_contact_survey(
    contact_survey_fitted_glmm,
    contact_survey_glmm_prepped
  ),
  contact_survey_prop_phys_dist_nsw_lga = write_csv(
    contact_survey_glmm_aug,
    file = "data/contact_survey_prop_phys_dist_nsw_lga.csv"
  ),
  plot_phys_distance_glmmm_all_popn =
    gg_phys_distance_all_popn(
      data = contact_survey_glmm_aug,
      y = .fitted
    ),

  # getting the
  australia_linelist = read_linelist(),
  linelist_lga = add_lga_from_abs(
    data_w_postcodes = australia_linelist,
    abs_lga_file =
      "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx",
    postcode_col = postcode
  ),
  tar_render(nsw_linelist, "doc/nsw-linelist.Rmd"),
  tar_file(google_shape_path, "~/not_synced/shapefiles/google/google.gpkg"),
  google_shape = read_sf() %>%
    filter(state == "New South Wales"),
  abs_shape = read_sf("~/not_synced/shapefiles/abs_lga/LGA_2016_AUST.shp") %>%
    filter(STE_NAME16 == "New South Wales"),
  abs_lga_weighted_by_google_lga_overlap = calculate_spatial_overlap(
    google_shape,
    abs_shape
  ),
  tar_file(weighted_abs_lga_csv, {
    write_csv(
      x = abs_lga_weighted_by_google_lga_overlap,
      file = "data/abs_lga_weighted_by_google_lga_overlap.csv"
    )
    "data/abs_lga_weighted_by_google_lga_overlap.csv"
  }),

  # modelling the mobility metrics by LGA
  # https://github.com/goldingn/covid19_australia_interventions/blob/nsw_tp/R/nsw_tp.R

  google_mobility_data = read_google_mobility_data(),
  google_mobility_nsw = tidy_mobility_nsw(google_mobility_data),
  first_date = min(google_mobility_nsw$date),
  last_date = max(google_mobility_nsw$date),
  google_mobility_fitted_nsw = add_mobility_data(google_mobility_nsw),
  
  # keep only the LGAs where we managed to fit a model (others have too-small
  # sample sizes for Google to provide data on the metrics we care about)
  
  google_mobility_fitted_nsw_model_is_fit = 
    filter_lgas_where_model_is_fit(google_mobility_fitted_nsw),
  
  lgas_to_fit = 
    na.omit(unique(google_mobility_fitted_nsw_model_is_fit$lga)),
  
  plot_mobility_fitted_trend = 
    gg_mobility_fitted_trend(google_mobility_fitted_nsw_model_is_fit,
                             lga_of_interest = lgas_to_fit,
                             last_date),
  
  write_mobility_fitted_ggplot = 
    gg_save_mobility_fitted_trend(plot_mobility_trend,
                                  lga_of_interest = lgas_to_fit,
                                  last_date = last_date),


  # which uses this function: `predict_mobility_trend`
  # (note it has been modified from the branch you were working on)
  # For each metric and each Google LGA, I'm fitting a GAM.
  # But sometimes it crashes because there's not enough data
  # Need those LGAs, so a solution that will run
  # tweaks model to make it run

  # model fitting over all LGAs to share google movement information
  # using urban/rural distinctions of ABS
  # plus a fixed effect over urban/rural + lockdowns (by LGA)
  # need a dataset of lockdowns by LGA

  # LGA activity mobility from google
  # vaccination forecasting coverage
  # we take outputs of that to plug into Gerry's model
  # gerry said he needs to get "Timeseries of vaccination effect"

  # taking the vaccination scenario plots
)
