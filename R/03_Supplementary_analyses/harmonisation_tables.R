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
Birks_Asia_Siber<- read.csv("Data/Input/Harmonisation_tables/Birks/Asia_Main_2023-04-06.csv")

