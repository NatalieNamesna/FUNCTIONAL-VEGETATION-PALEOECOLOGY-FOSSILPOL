#------------------------------------------------------------------------------#
#
#                             The FOSSILPOL workflow
#
#                          Harmonising the pollen taxa
#
#------------------------------------------------------------------------------#

# Creating a harmonisation tables based on Birks 

#------------------------------------------------------------------------------#
# 1. Load Birks tables -----
#------------------------------------------------------------------------------#

Birks_Asia_Levant <- readr::read_csv("Data/Input/Harmonisation_tables/Birks/Asia_Levant_2023-04-06.csv")
Birks_Asia_Main <- readr::read_csv("Data/Input/Harmonisation_tables/Birks/Asia_Main_2023-04-06.csv")
Birks_Asia_Siberia <- readr::read_csv("Data/Input/Harmonisation_tables/Birks/Asia_Siberia_2023-04-06.csv")
Birks_Europe <- readr::read_csv("Data/Input/Harmonisation_tables/Birks/Europe_2023-04-06.csv")
Birks_Indopacific <- readr::read_csv("Data/Input/Harmonisation_tables/Birks/Indopacific_2023-04-06.csv")
Birks_Latin_America <- readr::read_csv("Data/Input/Harmonisation_tables/Birks/Latin_America_2023-04-06.csv")
Birks_North_America <- readr::read_csv("Data/Input/Harmonisation_tables/Birks/North_America_2023-04-06.csv")

#------------------------------------------------------------------------------#
# 2. Make taxon name unique -----
#------------------------------------------------------------------------------#

# Asia Levant
unique_Birks_Asia_Levant <- Birks_Asia_Levant %>% 
  distinct(taxon_name, .keep_all = TRUE)

# Asia Main
unique_Birks_Asia_Main <- Birks_Asia_Main %>% 
  distinct(taxon_name, .keep_all = TRUE)

# Asia Siberia
unique_Birks_Asia_Siberia <- Birks_Asia_Siberia %>% 
  distinct(taxon_name, .keep_all = TRUE)

# Europe
unique_Birks_Europe <- Birks_Europe %>% 
  distinct(taxon_name, .keep_all = TRUE)

# Indopacific
unique_Birks_Indopacific <- Birks_Indopacific %>% 
  distinct(taxon_name, .keep_all = TRUE)

# Latin America
unique_Birks_Latin_America <- Birks_Latin_America %>% 
  distinct(taxon_name, .keep_all = TRUE)

# North America
unique_Birks_North_America <- Birks_North_America %>% 
  distinct(taxon_name, .keep_all = TRUE)

#------------------------------------------------------------------------------#
# 3. Load each empty table -----
#------------------------------------------------------------------------------#

empty_Asia_Levant <- read_csv("Data/Input/Harmonisation_tables/Asia_Levant_2026-02-24.csv") %>% 
  dplyr::select(-level_1)

empty_Asia_Main <- read_csv("Data/Input/Harmonisation_tables/Asia_Main_2026-02-24.csv")%>% 
  dplyr::select(-level_1)

empty_Asia_Siberia <- read_csv("Data/Input/Harmonisation_tables/Asia_Siberia_2026-02-24.csv")%>% 
  dplyr::select(-level_1)

empty_Europe <- read_csv("Data/Input/Harmonisation_tables/Europe_2026-02-24.csv")%>% 
  dplyr::select(-level_1)

empty_Indopacific <- read_csv("Data/Input/Harmonisation_tables/IndoPacific_2026-02-24.csv")%>% 
  dplyr::select(-level_1)
                
empty_Latin_America <- read_csv("Data/Input/Harmonisation_tables/Latin_America_2026-02-24.csv")%>% 
  dplyr::select(-level_1)

empty_North_America <- read_csv("Data/Input/Harmonisation_tables/North_America_2026-02-24.csv")%>% 
  dplyr::select(-level_1)

taxa_reference_table <- readr::read_csv(
  here::here("Data/Input/Harmonisation_tables/taxa_reference_table_2026-02-27.csv")
)


#------------------------------------------------------------------------------#
# 4. Join Birks tables and empty tables by taxon_name - harm table A -----
#------------------------------------------------------------------------------#

# Asia Levant
joined_empty_Birks_tables_Asia_Levant <-
  left_join(empty_Asia_Levant, Birks_Asia_Levant, by = "taxon_name")

# Asia Main
joined_empty_Birks_tables_Asia_Main <-
  left_join(empty_Asia_Main, Birks_Asia_Main, by = "taxon_name")

# Asia Siberia
joined_empty_Birks_tables_Asia_Siberia <-
  left_join(empty_Asia_Siberia, Birks_Asia_Siberia, by = "taxon_name")

# Europe
joined_empty_Birks_tables_Europe <-
  left_join(empty_Europe, Birks_Europe, by = "taxon_name")

# Indopacific
joined_empty_Birks_tables_Indospecific <-
  left_join(empty_Indopacific, Birks_Indopacific, by = "taxon_name")

# Latin America
joined_empty_Birks_tables_Latin_America <-
  left_join(empty_Latin_America, Birks_Latin_America, by = "taxon_name")

# North America
joined_empty_Birks_tables_North_America <-
  left_join(empty_North_America, Birks_North_America, by = "taxon_name")


#------------------------------------------------------------------------------#
# 5. Make a list of taxons, which are not present in Birks -----
#------------------------------------------------------------------------------#

# Asian Levant
NA_Asia_Levant <- joined_empty_Birks_tables_Asia_Levant %>% 
  filter(is.na(level_1))

# Asia Main
NA_Asia_Main <- joined_empty_Birks_tables_Asia_Main %>% 
  filter(is.na(level_1))

# Asia Siberia
NA_Asia_Siberia <- joined_empty_Birks_tables_Asia_Siberia %>% 
  filter(is.na(level_1))

# Europe
NA_Europe <- joined_empty_Birks_tables_Europe %>% 
  filter(is.na(level_1))

# Indopacific
NA_Indopacific <- joined_empty_Birks_tables_Indospecific %>% 
  filter(is.na(level_1))

# Latin America
NA_Latin_America <- joined_empty_Birks_tables_Latin_America %>% 
  filter(is.na(level_1))

# North America
NA_North_America <- joined_empty_Birks_tables_North_America %>% 
  filter(is.na(level_1))


#------------------------------------------------------------------------------#
# 6. Use {taxospace} to get the classification for the missing taxons 
# - harm table B  -----
#------------------------------------------------------------------------------#

# install.packages("remotes")
remotes::install_github("OndrejMottl/taxospace")

# Attach the package
library(taxospace)

# Clean taxon names ----

# Asia Levant - clean names
NA_Asia_Levant_clean <- NA_Asia_Levant %>% 
  mutate(
    taxon_clean = taxon_name %>% 
      str_remove("cf_") %>% 
      str_remove("_type") %>% 
      str_remove("_undiff") %>% 
      str_remove("_broken") %>% 
      str_remove("_crumpled") %>% 
      str_remove("_degraded") %>% 
      str_remove("_corroded") %>% 
      str_remove("_obscured") %>% 
      str_remove("_sensu_lato") %>%
      str_remove("_bigger_than_37_mu_m") %>%
      str_replace_all("_", " ") %>% 
      str_replace(
        "polygonum aviculare bistorta officinalis",
        "polygonum aviculare|bistorta officinalis"
      ) %>% 
      str_replace(
        "sarcopoterium spinosum poterium sanguisorba",
        "sarcopoterium spinosum|poterium sanguisorba"
      )  
  ) %>%
  separate_rows(taxon_clean, sep = "\\|") %>% 
  # Do names with capital letters
  mutate(
    taxon_clean_cap = str_c(
      str_to_upper(str_sub(taxon_clean, 1, 1)),
      str_sub(taxon_clean, 2)
    )
  )

# Asia Main
NA_Asia_Main_clean <- NA_Asia_Main %>% 
  mutate(
    taxon_clean = taxon_name %>% 
      str_remove("_type") %>% 
      str_remove("_bigger_than_35_mu_m") %>% 
      str_remove("_smaller_than_37_mu_m") %>% 
      str_remove("bigger_than_37_mu_m") %>% 
      str_remove("smaller_than_40_mu_m") %>% 
      str_remove("smaller_than_35_mu_m") %>% 
      str_replace_all("_", " ") %>% 
      str_replace(
        "polygonum aviculare bistorta officinalis",
        "polygonum aviculare|bistorta officinalis"
      )
  ) %>% 
  separate_rows(taxon_clean, sep = "\\|") %>% 
# Do names with capital letters
  mutate(
    taxon_clean_cap = str_c(
      str_to_upper(str_sub(taxon_clean, 1, 1)),
      str_sub(taxon_clean, 2)
    )
  )

# Asia Siberia
NA_Asia_Siberia_clean <- NA_Asia_Siberia %>% 
  mutate(
    taxon_clean = taxon_name %>% 
      str_remove("_type") %>% 
      str_remove("_undiff") %>% 
      str_remove("cf_") %>% 
      str_replace_all("_", " ") %>%
      str_replace(
        "paliurus spina minus christi",
        "palirius spina christi"
      ) %>% 
      str_replace(
        "astragalus cf a alpinus",
        "astragalus alpinus"
      ) %>%
      str_replace(
        "allium cf a sibiricum",
        "allium sibiricum"
      )
    ) %>% 

# Do names with capital letters
  mutate(
    taxon_clean_cap = str_c(
      str_to_upper(str_sub(taxon_clean, 1, 1)),
      str_sub(taxon_clean, 2)
    )
  )


# Europe
NA_Europe_clean <- NA_Europe %>% 
  mutate(
    taxon_clean = taxon_name %>% 
      str_remove("_33_minus_36_mu_m") %>% 
      str_remove("_10_minus_12_mu_m") %>% 
      str_remove("_bigger_than_35_mu_m") %>% 
      str_remove("_bigger_than_37_mu_m") %>% 
      str_remove("_bigger_than_45_mu_m") %>% 
      str_remove("_excluding_secale") %>% 
      str_remove("_37_minus_39_mu_m") %>% 
      str_remove("_40_minus_44_mu_m") %>% 
      str_remove("_45_minus_49_mu_m") %>% 
      str_remove("_bigger_than_50mu_m") %>% 
      str_remove("_type_excluding_secale") %>% 
      str_remove("_type_indeterminable") %>% 
      str_remove("bigger_than_20_mu_m") %>%
      str_remove("bigger_than_40_mu_m") %>%
      str_remove("_smaller_than_20_mu_m") %>%
      str_remove("_smaller_than_40_mu_m") %>%
      str_remove("_indet") %>% 
      str_remove("_large") %>% 
      str_remove("_small") %>% 
      str_remove("_undiff") %>% 
      str_remove("_sensu_lato") %>% 
      str_remove("cf_") %>% 
      str_remove("_group") %>%
      str_remove("_type1") %>% 
      str_remove("_type2") %>% 
      str_remove("_type") %>% 
      str_remove("_1") %>%
      str_remove("_2") %>% 
      str_replace_all("_", " ")
    ) %>% 

# Do names with capital letters
  mutate(
    taxon_clean_cap = str_c(
      str_to_upper(str_sub(taxon_clean, 1, 1)),
      str_sub(taxon_clean, 2)
    )
  )

# Indopacific
NA_Indopacific_clean <- NA_Indopacific %>% 
  mutate(
    taxon_clean = taxon_name %>%
      str_remove("_type_1_granular") %>% 
      str_remove("_type_2_psilate") %>% 
      str_remove("_type_3_granular") %>% 
      str_remove("_type_4_granular") %>% 
      str_remove("_type_anodopetalum_type") %>%
      str_remove("_bigger_than_50_mu_m") %>% 
      str_remove("_smaller_than_50_mu_m") %>%
      str_remove("undiff") %>%
      str_remove("_type_a") %>% 
      str_remove("_type_b") %>%
      str_remove("_type_c") %>% 
      str_remove("_type") %>% 
      str_remove("_1") %>% 
      str_remove("cf_") %>% 
      str_replace_all("_", " ")
  ) %>% 
# Do names with capital letters
  mutate(
    taxon_clean_cap = str_c(
      str_to_upper(str_sub(taxon_clean, 1, 1)),
      str_sub(taxon_clean, 2)
    )
  )

# Latin America
NA_Latin_America_clean <- NA_Latin_America %>% 
  mutate(
    taxon_clean = taxon_name %>%
      str_remove("_type_1") %>% 
      str_remove("_type_2") %>% 
      str_remove("_type_3") %>%
      str_remove("_type_4") %>%
      str_remove("_type_5") %>%
      str_remove("_type_6") %>%
      str_remove("_type_7") %>%
      str_remove("_type_8") %>%
      str_remove("_type_9") %>%
      str_remove("_type_10") %>%
      str_remove("_clade_type_ii_cf_acacia") %>%
      str_remove("_bigger_than_35_mu_m") %>% 
      str_remove("_smaller_than_35_mu_m") %>%
      str_remove("_type_i") %>%
      str_remove("_type_ii") %>%
      str_remove("_type") %>%
      str_remove("_undiff") %>%
      str_remove("cf_") %>%
      str_replace_all("_", " ")
  ) %>% 
# Do names with capital letters
  mutate(
    taxon_clean_cap = str_c(
      str_to_upper(str_sub(taxon_clean, 1, 1)),
      str_sub(taxon_clean, 2)
    )
  )


# North America

NA_North_America_clean <- NA_North_America %>% 
  mutate(
    taxon_clean = taxon_name %>%
      str_remove("_large") %>% 
      str_remove("_small") %>% 
      str_remove("_type_1") %>% 
      str_remove("_type_2") %>% 
      str_remove("_type") %>% 
      str_remove("cf_") %>%
      str_remove("_undiff") %>% 
      str_replace_all("_", " ")
  ) %>% 
# Do names with capital letters
  mutate(
    taxon_clean_cap = str_c(
      str_to_upper(str_sub(taxon_clean, 1, 1)),
      str_sub(taxon_clean, 2)
    )
  )


# Using {taxospace} for completing missing classification ----



# Asia Levant

# Make vector with clean taxon names
taxa_vec_Asia_Levant <- NA_Asia_Levant_clean %>%
  distinct(taxon_clean_cap) %>% # get unique values
  pull(taxon_clean_cap) # extracting column, pull() makes vector / select() makes data frame

# other cleaning
taxa_vec_Asia_Levant_clean <- taxa_vec_Asia_Levant %>%
  str_replace(" [A-Z].*$", "")

# Get classification for Asia Levant
classification_Asia_Levant <-
  taxa_vec_Asia_Levant %>% 
  rlang::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ taxospace::get_classification(.x)) %>% 
  bind_rows() 

# my idea - not that good
classification_Asia_Levant %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  group_by(sel_name) %>% 
  slice_tail(n = 1) 

# get table of finest classification -> Ondra's idea - very good
get_finest_classification_Asia_Levant <-
classification_Asia_Levant %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  pivot_wider(
    names_from = rank, 
    values_from = name
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::case_when(
      genus == "NULL" ~  family,
      .default = genus
    )
  ) %>% 
  dplyr::select(sel_name, level_1)





# Asia Main

# Make vector with clean taxon names
taxa_vec_Asia_Main <- NA_Asia_Main_clean %>%
  distinct(taxon_clean_cap) %>%
  pull(taxon_clean_cap) # extracting column, pull() makes vector / select() makes data frame

# Get classification for Asia Main
classification_Asia_Main <-
  taxa_vec_Asia_Main %>% 
  rlang::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ taxospace::get_classification(.x)) %>% 
  bind_rows() 

# get table of finest classification
get_finest_classification_Asia_Main <-
  classification_Asia_Main %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  pivot_wider(
    names_from = rank, 
    values_from = name
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::case_when(
      is.na(genus) ~  family,
      .default = genus
    )
  ) %>% 
  dplyr::select(sel_name, level_1)





# Asia Siberia
taxa_vec_Asia_Siberia <- NA_Asia_Siberia_clean %>%
  distinct(taxon_clean_cap) %>%
  pull(taxon_clean_cap)

# Get classification for Asia Siberia
classification_Asia_Siberia <-
  taxa_vec_Asia_Siberia %>% 
  rlang::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ taxospace::get_classification(.x)) %>% 
  bind_rows() 

# get table of finest classification
get_finest_classification_Asia_Siberia <-
  classification_Asia_Siberia %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  pivot_wider(
    names_from = rank, 
    values_from = name
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::case_when(
      genus == "NULL" ~  family,
      .default = genus
    )
  ) %>% 
  dplyr::select(sel_name, level_1)





# Europe
taxa_vec_Europe <- NA_Europe_clean %>%
  distinct(taxon_clean_cap) %>%
  pull(taxon_clean_cap)

# Get classification for Asia Europe
classification_Europe <-
  taxa_vec_Europe %>% 
  rlang::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ taxospace::get_classification(.x)) %>% 
  bind_rows() 

# get table of finest classification
get_finest_classification_Europe <-
  classification_Europe %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  pivot_wider(
    names_from = rank, 
    values_from = name
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::case_when(
      genus == "NULL" ~  family,
      .default = genus
    )
  ) %>% 
  dplyr::select(sel_name, level_1)






# Indopacific
taxa_vec_Indopacific <- NA_Indopacific_clean %>%
  distinct(taxon_clean_cap) %>%
  pull(taxon_clean_cap)

# Get classification for Indopacific
classification_Indopacific <-
  taxa_vec_Indopacific %>% 
  rlang::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ taxospace::get_classification(.x)) %>% 
  bind_rows() 

# get table of finest classification
get_finest_classification_Indopacific <-
  classification_Indopacific %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  pivot_wider(
    names_from = rank, 
    values_from = name
  ) %>% 
  dplyr::mutate(
    level_1a = dplyr::case_when(
      genus == "NULL" ~  family,
      .default = genus
    )
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::if_else(
      level_1a == "NULL", order, level_1a
    )
  ) %>%
  dplyr::select(sel_name, level_1)





# Latin America
taxa_vec_Latin_America <- NA_Latin_America_clean %>%
  distinct(taxon_clean_cap) %>%
  pull(taxon_clean_cap)


# Get classification for Latin America
classification_Latin_America <-
  taxa_vec_Latin_America %>% 
  rlang::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ taxospace::get_classification(.x)) %>% 
  bind_rows() 

# get table of finest classification
get_finest_classification_Latin_America <-
  classification_Latin_America %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  pivot_wider(
    names_from = rank, 
    values_from = name
  ) %>% 
  dplyr::mutate(
    level_1a = dplyr::case_when(
      genus == "NULL" ~  family,
      .default = genus
    )
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::if_else(
      level_1a == "NULL", as.list(sel_name), level_1a
    )
  ) %>%
  dplyr::select(sel_name, level_1)





# North America
taxa_vec_North_America <- NA_North_America_clean %>%
  distinct(taxon_clean_cap) %>%
  pull(taxon_clean_cap)

# Get classification for North America
classification_North_America <-
  taxa_vec_North_America %>% 
  rlang::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ taxospace::get_classification(.x)) %>% 
  bind_rows() 


# get table of finest classification
get_finest_classification_North_America <-
  classification_North_America %>% 
  dplyr::select(sel_name, classification) %>% 
  tidyr::unnest(classification) %>% 
  dplyr::select(-id) %>%
  pivot_wider(
    names_from = rank, 
    values_from = name
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::case_when(
      genus == "NULL" ~  family,
      .default = genus
    )
  ) %>%
  dplyr::select(sel_name, level_1)




