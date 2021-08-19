source("./packages.R")
source("./conflicts.R")
## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  # read in all surveys
  contact_survey = read_doh_add_weights(
    doh_path = "~/not_synced/survey_data/",
    abs_lga_postcodes_path = "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx"
  ),
  contact_survey_nsw = prepare_survey_nsw(
    data = contact_survey,
    pmin_contact_num = 20,
    vec_lgas_concern = generate_lgas_of_concern()
  ),
  
  contact_survey_nsw_summarised_lga_concern = contact_survey_nsw %>%
    group_by(date_week, lga_of_concern) %>%
    summarise_contacts_weights(),

  contact_survey_nsw_summarised_lga_concern_age_groups = contact_survey_nsw %>%
    group_by(date_week, lga_of_concern, age_groups) %>%
    summarise_contacts_weights(),
  
  contact_survey_nsw_summarised_lga_concern_age_16_39 = contact_survey_nsw %>%
    group_by(date_week, lga_of_concern, age_16_39) %>%
    summarise_contacts_weights(),
  
  contact_survey_nsw_summarised_lga_concern_age_16_29 = contact_survey_nsw %>%
    group_by(date_week, lga_of_concern, age_16_29) %>%
    summarise_contacts_weights(),
  
  contact_survey_nsw_summarised_lga_concern_16_39_only = contact_survey_nsw %>%
    filter(age_16_39 == "16-39") %>% 
    group_by(date_week, lga_of_concern) %>%
    summarise_contacts_weights(),
  
  contact_survey_nsw_summarised_lga_concern_16_29_only = contact_survey_nsw %>%
    filter(age_16_29 == "16-29") %>% 
    group_by(date_week, lga_of_concern) %>%
    summarise_contacts_weights(),
  
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
  
  # phy_distance - as "always"
  # by LGA
  # weighted mean of phy_distance == "always", for the LGAs
  # for 16-29 vs all age groups
  # something something GAM hierarchical
  
  # 16-29yos and 30+ yos
  # and also compare the 16-29yos vs the whole population

  # contact rates over time by 16-29, and all ages, 
  # and then the same again for the physical distancing question would be great.
  
  tar_render(rmd_nsw_contact_survey, "doc/nsw-contacy-survey.Rmd"),
  
  contact_survey_glmm_prepped = prepare_contact_survey_glmm(contact_survey_nsw),
  contact_survey_fitted_glmm = fit_glmm_contact_survey(contact_survey_glmm_prepped),
  contact_survey_glmm_aug = augment_glmm_contact_survey(
    contact_survey_fitted_glmm,
    contact_survey_glmm_prepped
  ),
  
  contact_survey_prop_phys_dist_nsw_lga = write_csv(
    contact_survey_glmm_aug,
    file = "data/contact_survey_prop_phys_dist_nsw_lga.csv"
  ),
  
  plot_phys_distance_glmmm_all_popn = 
    gg_phys_distance_all_popn(data = contact_survey_glmm_aug,
                              y = .fitted),
  
  
  # getting the 
  australia_linelist = read_linelist(),
  linelist_lga = add_lga_from_abs(
    data_w_postcodes = australia_linelist,
    abs_lga_file =
      "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx",
    postcode_col = postcode
  ),
  tar_render(nsw_linelist, "doc/nsw-linelist.Rmd"),
  
  google_shape = read_sf("~/not_synced/shapefiles/google/google.gpkg") %>% 
    filter(state == "New South Wales"),
  abs_shape = read_sf("~/not_synced/shapefiles/abs_lga/LGA_2016_AUST.shp") %>% 
    filter(STE_NAME16 == "New South Wales"),
  
  # planar CRS
  # find out which CRS each is
  # make them the same
  
  weighted_abs_lga = calculate_spatial_overlap(
    google_shape,
    abs_shape
  ),
  
  # model fitting over all LGAs to share google movement information
  # using urban/rural distinctions of ABS
  # plus a fixed effect over urban/rural + lockdowns (by LGA)
  # need a dataset of lockdowns by LGA
  
  # LGA activity mobility from google
  # use st_overlap to get the weights of how the google LGAs map to
  # the ABS LGA data so 
  # need to find an ABS defn
  # vaccination forecasting coverage
  # we take outputs of that to plug into Gerry's model
  # need to ask Gerry what he needs
  #  - need to get "Timeseries of vaccination effect"

)
