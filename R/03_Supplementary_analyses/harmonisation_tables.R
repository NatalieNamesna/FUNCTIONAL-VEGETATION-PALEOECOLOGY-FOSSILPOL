#----------------------------------------------------------#
#
#                 The FOSSILPOL workflow
#
#               Harmonising the pollen taxa
#
#----------------------------------------------------------#

# Creating a harmonisation tables based on Birks 

#----------------------------------------------------------#
# 1. Load Birks tables -----
#----------------------------------------------------------#

Birks_Asia_Levant <- read.csv("Data/Input/Harmonisation_tables/Birks/Asia_Levant_2023-04-06.csv")
Birks_Asia_Main <- read.csv("Data/Input/Harmonisation_tables/Birks/Asia_Main_2023-04-06.csv")
Birks_Asia_Siberia <- read.csv("Data/Input/Harmonisation_tables/Birks/Asia_Siberia_2023-04-06.csv")
Birks_Europe <- read.csv("Data/Input/Harmonisation_tables/Birks/Europe_2023-04-06.csv")
Birks_Indopacific <- read.csv("Data/Input/Harmonisation_tables/Birks/Indopacific_2023-04-06.csv")
Birks_Latin_America <- read.csv("Data/Input/Harmonisation_tables/Birks/Latin_America_2023-04-06.csv")
Birks_North_America <- read.csv("Data/Input/Harmonisation_tables/Birks/North_America_2023-04-06.csv")

#----------------------------------------------------------#
# 2. Make taxon name unique -----
#----------------------------------------------------------#

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

#----------------------------------------------------------#
# 3. Load each empty table -----
#----------------------------------------------------------#

empty_Asia_Levant <- read.csv("Data/Input/Harmonisation_tables/Asia_Levant_2026-02-24.csv")
empty_Asia_Main <- read.csv("Data/Input/Harmonisation_tables/Asia_Main_2026-02-24.csv")
empty_Asia_Siberia <- read.csv("Data/Input/Harmonisation_tables/Asia_Siberia_2026-02-24.csv")
empty_Europe <- read.csv("Data/Input/Harmonisation_tables/Europe_2026-02-24.csv")
empty_Indopacific <- read.csv("Data/Input/Harmonisation_tables/IndoPacific_2026-02-24.csv")
empty_Latin_America <- read.csv("Data/Input/Harmonisation_tables/Latin_America_2026-02-24.csv")
empty_North_America <- read.csv("Data/Input/Harmonisation_tables/North_America_2026-02-24.csv")


#----------------------------------------------------------#
# 4. Join Birks tables and empty tables by taxon_name -----
#----------------------------------------------------------#

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


#----------------------------------------------------------#
# 5. Make a list of taxons, which are not present in Birks -----
#----------------------------------------------------------#

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























