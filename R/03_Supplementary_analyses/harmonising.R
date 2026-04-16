#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_with_chronologies <-
  RUtilpol::get_latest_file(
    file_name = "data_with_chronologies",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Data/Processed/Data_with_chronologies"
    )
  ) %>% 
  dplyr::mutate(
    harmonisation_region = dplyr::case_when(
      .default = harmonisation_region,
      harmonisation_region == "New Zealand" ~ "IndoPacific",
      harmonisation_region == "IndoPacific_Islands" ~ "IndoPacific",
      harmonisation_region == "Papua_New_Guinea" ~ "IndoPacific",
      harmonisation_region == "Tasmania_and_islands" ~ "IndoPacific",
      harmonisation_region == "Australia_Southeast" ~ "IndoPacific",
      harmonisation_region == "Australia_North" ~ "IndoPacific",
    )
  )



# test the presence of data
RUtilpol::check_if_loaded(
  file_name = "data_with_chronologies",
  env = current_env
)


#----------------------------------------------------------#
# 2. Harmonise data -----
#----------------------------------------------------------#
data_harmonised <-
  RFossilpol::harmonise_all_regions(
    data_source = data_with_chronologies,
    harmonisation_tables = harmonisation_tables_df,
    original_name = "taxon_name",
    harm_level = "level_1", # [USER] Change the levels if needed
    exclude_taxa = "delete",
    pollen_grain_test = TRUE # [USER] Turn FALSE to hide progress
  )


#----------------------------------------------------------#
# 3. Save the data -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_harmonised,
  dir = paste0(
    data_storage_path, # [config_criteria]
    "/Data/Processed/Data_harmonised"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)

