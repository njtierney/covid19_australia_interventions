message("loading R packages")
suppressPackageStartupMessages(source(here::here("packages.R")))
message("Loading greta and tensorflow objects")
source(here("greta-tf-objs.R"))
# source("./conflicts.R")
## Load your R files
lapply(list.files(here("R"), full.names = TRUE), source)

tar_plan(

  tar_file(
    doh_path,
    here("data/survey_data/")
  ),
  tar_file(
    abs_lga_postcodes_path,
    here("data/CA_POSTCODE_2018_LGA_2018.xlsx")
  ),
  # read in all surveys
  contact_survey = read_doh_add_weights(
    doh_path = doh_path,
    abs_lga_postcodes_path = abs_lga_postcodes_path
  ),
  greater_sydney_lgas = extract_greater_sydney_lgas(),
  lga_of_concern = lgas_of_concern(),
  greater_sydney_without_lga_of_concern = 
    setdiff(greater_sydney_lgas, lga_of_concern),
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

  australia_linelist = read_linelist(),
  tar_file(abs_lga_path, here("data/CA_POSTCODE_2018_LGA_2018.xlsx")),
  linelist_lga = add_lga_from_abs(
    data_w_postcodes = australia_linelist,
    abs_lga_file = abs_lga_path,
  ),
  tar_render(nsw_linelist, "doc/nsw-linelist.Rmd"),
  tar_file(google_shape_path, here("data/shapefiles/google/google.gpkg")),
  google_shape = read_sf(google_shape_path) %>%
    filter(state == "New South Wales"),
  tar_file(abs_shape_path, here("data/shapefiles/abs_lga/LGA_2016_AUST.shp")),
  abs_shape = read_sf(abs_shape_path) %>% 
    filter(STE_NAME16 == "New South Wales"),
  abs_lga_google_concordance = calculate_spatial_overlap(
    google_shape,
    abs_shape,
    mobility_data_lga_table
  ),
  tar_file(abs_lga_google_concordance_csv, {
    write_csv(
      x = abs_lga_google_concordance,
      file = here("data/abs_lga_google_concordance.csv")
    )
    here("data/abs_lga_google_concordance.csv")
  }),

  # modelling the mobility metrics by LGA
  # https://github.com/goldingn/covid19_australia_interventions/blob/nsw_tp/R/nsw_tp.R

  mobility_data = read_google_mobility_data(),
  mobility_data_lga_table = create_lga_lookup(mobility_data),
  mobility_nsw = tidy_mobility_nsw(mobility_data),
  mobility_fitted_nsw = add_mobility_data(mobility_nsw),
  
  # keep only the LGAs where we managed to fit a model (others have too-small
  # sample sizes for Google to provide data on the metrics we care about)
  
  mobility_fitted_nsw_model_is_fit = 
    filter_lgas_where_model_is_fit(mobility_fitted_nsw),
  
  mobility_fitted_nsw_concordance = add_concordance(
      mobility_fitted_nsw_model_is_fit,
      abs_lga_google_concordance
      ),
  
  mobility_for_plotting = prepare_mobility_for_plots(
    mobility_fitted_nsw_concordance
  ),
  
  df_mobility_fitted_trend_plots =  gg_mobility_fitted_trend_all_lga(
    mobility_fitted_nsw_concordance
    ),
  
  df_mobility_fitted_trend_plot_paths = add_mobility_filepath(
    df_mobility_fitted_trend_plots
    ),
  
  
  tar_file(write_mobility_fitted_ggplot, {
    gg_save_mobility_fitted_trend(
      df_mobility_fitted_trend_plot_paths
    )
    df_mobility_fitted_trend_plot_paths$path
  }
  ),
  
  mobility_fitted_nsw_model_id_lgas_not_fit = identify_lgas_model_not_fit(
    mobility_fitted_nsw,
    mobility_fitted_nsw_model_is_fit
  ),
  
  mobility_nsw_lgas_not_fit = mobility_nsw %>% 
    filter(lga %in% mobility_fitted_nsw_model_id_lgas_not_fit),
  
  lgas_to_fit = 
    na.omit(unique(mobility_fitted_nsw_model_is_fit$lga)),
  
  # work on a new approach to fitting these models
  mobility_holidays_interventions = add_holidays_interventions(mobility_nsw),
  mobility_nest_model_fit = add_gam_fit(mobility_holidays_interventions,
                                        s_k = 48),
  mobility_gam_which_fit = filter_did_gam_fit(mobility_nest_model_fit),
  mobility_gam_which_error = filter_did_gam_error(mobility_nest_model_fit),
  mobility_gam_refit_error = mobility_refit_gam(mobility_gam_which_error),
  mobility_gam_refit_error_id_poor_fit = 
    filter_gam_poor_fit(mobility_gam_which_error),
  # need to check some of the implementation details for using the expand grid
  # part of this.
  mobility_grid = create_expanded_grid(mobility_nest_model_fit),
  mobility_gam_added_preds = add_fitted_upper_lower(mobility_gam_which_fit),
  # which uses this function: `predict_mobility_trend`
  # (note it has been modified from the branch you were working on)

  # save predictions in correct format for macro and mobility models
  # AKA "location_change_trends" object in "nsw-tp.R"
  location_change_trends = 
    prepare_fitted_for_macro_micro_models(mobility_fitted_nsw_concordance),
  
  
  # load fitted macrodistancing model
  tar_file(macro_model_path, here("outputs/nsw/fitted_macro_model.RDS")),
  
  macro_model = read_rds(macro_model_path),
  
  macro_distancing_trends_lga = 
    model_macro_distancing_trends_lga(
      location_change_trends,
      macro_model
    ),
  
  tar_file(macro_distancing_trends_lga_csv, {
    write_csv(
      x = macro_distancing_trends_lga,
      file = here("outputs/nsw/nonhousehold_contacts_lga_modelled.csv")
    )
    here("outputs/nsw/nonhousehold_contacts_lga_modelled.csv")
  }),
  
  
  # combine these with NSW data for other components to get TP for each LGA
  
  # load fitted reff model
  tar_file(fitted_reff_model_path,
           here("outputs/nsw/fitted_reff_model.RDS")),
  
  fitted_reff_model = read_rds(fitted_reff_model_path),
  
  tar_file(nishiura_samples_path,
           here("data/parameters/nishiura_samples.csv")),
  
  nishiura_samples = get_nishiura_samples(nishiura_samples_path),
  
  gi_cdf = nishiura_cdf(nishiura_samples),
  
  reff_trend = create_reff_trend(location_change_trends,
                                   macro_distancing_trends_lga,
                                   fitted_reff_model,
                                   macro_model,
                                   gi_cdf),
  
  tar_file(reff_trend_csv, {
    write_csv_return_path(
      x = reff_trend,
      file = here("outputs/nsw/tp_trends_no_vacc.csv")
    )
  }),
  
  tar_file(vaccine_effect_path, 
           here("outputs/nsw/nsw_lgas_vaccination_effect.csv")),
  
  vaccine_effect = read_vaccine_effect(vaccine_effect_path),
  
  reff_trend_vaccination = create_reff_trend_vaccination(
    reff_trend,
    vaccine_effect
  ),
  
  tar_file(reff_trend_vaccination_csv, {
    write_csv_return_path(
      x = reff_trend_vaccination,
      file = here("outputs/nsw/tp_trends_no_vacc.csv")
    )
  }),
  
  reff_trend_vaccination_prep_plots = add_reff_trend_vac_prep_plots(
    reff_trend_vaccination
  ),
  
  reff_trend_vaccination_plots = add_reff_trend_vac_plots(
    reff_trend_vaccination_prep_plots
  ),

  write_reff_trend_vaccination_plots = write_reff_trend_vac_plots(
    reff_trend_vaccination_plots
  ),
  
  
  

  # LGA activity mobility from google
  # vaccination forecasting coverage
  # we take outputs of that to plug into Gerry's model
  # gerry said he needs to get "Timeseries of vaccination effect"

  # taking the vaccination scenario plots
)
