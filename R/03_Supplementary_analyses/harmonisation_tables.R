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

Birks_Asia_Levant <- read.csv("Data/Input/Harmonisation_tables/Birks/Asia_Levant_2023-04-06.csv")
Birks_Asia_Main <- read.csv("Data/Input/Harmonisation_tables/Birks/Asia_Main_2023-04-06.csv")
Birks_Asia_Siberia <- read.csv("Data/Input/Harmonisation_tables/Birks/Asia_Siberia_2023-04-06.csv")
Birks_Europe <- read.csv("Data/Input/Harmonisation_tables/Birks/Europe_2023-04-06.csv")
Birks_Indopacific <- read.csv("Data/Input/Harmonisation_tables/Birks/Indopacific_2023-04-06.csv")
Birks_Latin_America <- read.csv("Data/Input/Harmonisation_tables/Birks/Latin_America_2023-04-06.csv")
Birks_North_America <- read.csv("Data/Input/Harmonisation_tables/Birks/North_America_2023-04-06.csv")

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

empty_Asia_Levant <- read.csv("Data/Input/Harmonisation_tables/Asia_Levant_2026-02-24.csv")
empty_Asia_Main <- read.csv("Data/Input/Harmonisation_tables/Asia_Main_2026-02-24.csv")
empty_Asia_Siberia <- read.csv("Data/Input/Harmonisation_tables/Asia_Siberia_2026-02-24.csv")
empty_Europe <- read.csv("Data/Input/Harmonisation_tables/Europe_2026-02-24.csv")
empty_Indopacific <- read.csv("Data/Input/Harmonisation_tables/IndoPacific_2026-02-24.csv")
empty_Latin_America <- read.csv("Data/Input/Harmonisation_tables/Latin_America_2026-02-24.csv")
empty_North_America <- read.csv("Data/Input/Harmonisation_tables/North_America_2026-02-24.csv")


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
  filter(is.na(level_1.y))

# Asia Main
NA_Asia_Main <- joined_empty_Birks_tables_Asia_Main %>% 
  filter(is.na(level_1.y))

# Asia Siberia
NA_Asia_Siberia <- joined_empty_Birks_tables_Asia_Siberia %>% 
  filter(is.na(level_1.y))

# Europe
NA_Europe <- joined_empty_Birks_tables_Europe %>% 
  filter(is.na(level_1.y))

# Indopacific
NA_Indopacific <- joined_empty_Birks_tables_Indospecific %>% 
  filter(is.na(level_1.y))

# Latin America
NA_Latin_America <- joined_empty_Birks_tables_Latin_America %>% 
  filter(is.na(level_1.y))

# North America
NA_North_America <- joined_empty_Birks_tables_North_America %>% 
  filter(is.na(level_1.y))


#------------------------------------------------------------------------------#
# 6. Use {taxospace} to get the classification for the missing taxons 
# - harm table B  -----
#------------------------------------------------------------------------------#

# install.packages("remotes")
remotes::install_github("OndrejMottl/taxospace")

# Attach the package
library(taxospace)

# Clean taxon names 

# Asia Levant 
NA_Asia_Levant_clean <- NA_Asia_Levant %>% 
  mutate(
    taxon_clean = taxon_name %>% 
      str_remove("_type") %>% 
      str_remove("_undiff") %>% 
      str_remove("_broken") %>% 
      str_remove("_crumpled") %>% 
      str_remove("_degraded") %>% 
      str_remove("_corroded") %>% 
      str_remove("_obscured") %>% 
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
  separate_rows(taxon_clean, sep = "\\|")
  



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
  separate_rows(taxon_clean, sep = "\\|")

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
  )


# 










