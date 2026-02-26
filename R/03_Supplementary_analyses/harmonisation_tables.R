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




