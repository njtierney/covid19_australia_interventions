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
    pmin_contact_num = 100,
    vec_lgas_concern = generate_lgas_of_concern()
  ),
  contact_survey_nsw_summarised_lga_concern = contact_survey_nsw %>%
    group_by(date_week, lga_of_concern) %>%
    summarise_contacts_weights(),

  contact_survey_nsw_summarised_lga_concern_age_groups = contact_survey_nsw %>%
    group_by(date_week, lga_of_concern, age_groups) %>%
    summarise_contacts_weights(),
  contact_survey_nsw_summarised_lga_concern_youth_older = contact_survey_nsw %>%
    group_by(date_week, lga_of_concern, youth_older) %>%
    summarise_contacts_weights(),
  contact_survey_glmm_prepped = prepare_contact_survey_glmm(contact_survey_nsw),
  contact_survey_fitted_glmm = fit_glmm_contact_survey(contact_survey_glmm_prepped),
  contact_survey_glmm_aug = augment_glmm_contact_survey(
    contact_survey_fitted_glmm,
    contact_survey_glmm_prepped
  ),
  plot_phys_distance_glmmm_all_popn = gg_phys_distance_glmmm_all_popn(contact_survey_glmm_aug),
  
  
  tar_render(nsw_lgas, "doc/nsw-lgas.Rmd"),
  
  australia_linelist = read_linelist(),
  linelist_lga = add_lga_from_abs(
    data_w_postcodes = australia_linelist,
    abs_lga_file =
      "~/not_synced/CA_POSTCODE_2018_LGA_2018.xlsx",
    postcode_col = postcode
  ),
  tar_render(nsw_linelist, "doc/nsw-linelist.Rmd"),
  
  google_shape = read_sf("~/not_synced/shapefiles/google/google.gpkg"),
  abs_shape = read_sf("~/not_synced/shapefiles/abs_lga/LGA_2016_AUST.shp"),
  weighted_abs_lga = calculate_spatial_overlap(
    google_shape,
    abs_shape
  ),
  

  # average contact rates over time by 16-29
  # average contact rates over time for ages,
  # physical distancing over time by 16-29
  # physical distancing over time for ages,
)
