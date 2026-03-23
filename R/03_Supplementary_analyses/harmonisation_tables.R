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
  left_join(empty_Asia_Levant, unique_Birks_Asia_Levant, by = "taxon_name")

# Asia Main
joined_empty_Birks_tables_Asia_Main <-
  left_join(empty_Asia_Main, unique_Birks_Asia_Main, by = "taxon_name")

# Asia Siberia
joined_empty_Birks_tables_Asia_Siberia <-
  left_join(empty_Asia_Siberia, unique_Birks_Asia_Siberia, by = "taxon_name")

# Europe
joined_empty_Birks_tables_Europe <-
  left_join(empty_Europe, unique_Birks_Europe, by = "taxon_name")

# Indopacific
joined_empty_Birks_tables_Indospecific <-
  left_join(empty_Indopacific, unique_Birks_Indopacific, by = "taxon_name")

# Latin America
joined_empty_Birks_tables_Latin_America <-
  left_join(empty_Latin_America, unique_Birks_Latin_America, by = "taxon_name")

# North America
joined_empty_Birks_tables_North_America <-
  left_join(empty_North_America, unique_Birks_North_America, by = "taxon_name")


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
# 6.Use {taxospace} to get the classification for the missing taxons ----
# - harm table B  
#------------------------------------------------------------------------------#

# install.packages("remotes")
remotes::install_github("OndrejMottl/taxospace")

# Attach the package
library(taxospace)

## Clean taxon names ----

### Asia Levant ----
NA_Asia_Levant_clean <- NA_Asia_Levant %>% 
  mutate(
    taxon_clean = taxon_name %>% 
      str_remove("cf_") %>%
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

### Asia Main ----
NA_Asia_Main_clean <- NA_Asia_Main %>% 
  mutate(
    taxon_clean = taxon_name %>% 
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

### Asia Siberia ----
NA_Asia_Siberia_clean <- NA_Asia_Siberia %>% 
  mutate(
    taxon_clean = taxon_name %>% 
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


### Europe ----
NA_Europe_clean <- NA_Europe %>% 
  mutate(
    taxon_clean = taxon_name %>% 
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

### Indopacific ----
NA_Indopacific_clean <- NA_Indopacific %>% 
  mutate(
    taxon_clean = taxon_name %>%
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

### Latin America ----
NA_Latin_America_clean <- NA_Latin_America %>% 
  mutate(
    taxon_clean = taxon_name %>%
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


### North America ----
NA_North_America_clean <- NA_North_America %>% 
  mutate(
    taxon_clean = taxon_name %>%
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


## Using {taxospace} for completing missing classification ----

### Asia Levant ----

# Make vector with clean taxon names
taxa_vec_Asia_Levant <- NA_Asia_Levant_clean %>%
  # distinct(taxon_clean_cap) %>% # get unique values
  pull(taxon_clean_cap) # extracting column, pull() makes vector / select() makes data frame

# other cleaning
# taxa_vec_Asia_Levant_clean <- taxa_vec_Asia_Levant %>%
#  str_replace(" [A-Z].*$", "")

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



### Asia Main ----

# Make vector with clean taxon names
taxa_vec_Asia_Main <- NA_Asia_Main_clean %>%
  # distinct(taxon_clean_cap) %>%
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





### Asia Siberia ----
taxa_vec_Asia_Siberia <- NA_Asia_Siberia_clean %>%
 # distinct(taxon_clean_cap) %>%
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





### Europe ----
taxa_vec_Europe <- NA_Europe_clean %>%
 # distinct(taxon_clean_cap) %>%
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
    level_1a = dplyr::case_when(
      genus == "NULL" ~  family,
      .default = genus
    )
  ) %>% 
  dplyr::mutate(
    level_1 = dplyr::if_else(
      level_1a == "NULL", class, level_1a
    )
  ) %>%
  dplyr::select(sel_name, level_1)




### Indopacific ----
taxa_vec_Indopacific <- NA_Indopacific_clean %>%
  # distinct(taxon_clean_cap) %>%
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





### Latin America ----
taxa_vec_Latin_America <- NA_Latin_America_clean %>%
 # distinct(taxon_clean_cap) %>%
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





### North America ----
taxa_vec_North_America <- NA_North_America_clean %>%
 # distinct(taxon_clean_cap) %>%
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


#------------------------------------------------------------------------------#
# 7. Merge harmonisation tables a and b  -----
#------------------------------------------------------------------------------#

## Asia Levant ----

### harm table a ----
joined_empty_Birks_tables_Asia_Levant

### harm table b ---- 

# unlist
get_finest_classification_Asia_Levant <- get_finest_classification_Asia_Levant %>% 
  dplyr::mutate(
    level_1 = map_chr(level_1, 1)
  )

# merge get_finest_classification_Asia_Levant and NA_Asia_Levant_clean to get 
# taxon_name for future merging with Birks

NA_Asia_Levant_clean <- NA_Asia_Levant_clean %>% 
  mutate(
    sel_name = taxon_clean_cap) %>% 
  select(-c(raw_name, level_1, taxon_clean, taxon_clean_cap))


classification_with_taxon_names_Asia_Levant <- 
  left_join(get_finest_classification_Asia_Levant, NA_Asia_Levant_clean, by = "sel_name")


### merge classification_with_taxon_names_Asia_Levant with Birks to final table ----
# level_1 are the harmonisation
harmonisation_table_Asia_Levant <- joined_empty_Birks_tables_Asia_Levant %>%
  left_join(
    classification_with_taxon_names_Asia_Levant %>%
      select(taxon_name, level_1),
    by = "taxon_name",
    suffix = c("", "_new")
  ) %>%
  mutate(
    level_1 = coalesce(level_1, level_1_new)
  ) %>%
  select(-level_1_new)



## Asia Main ----

### harm table a ----
joined_empty_Birks_tables_Asia_Main

### harm table b ---- 

# unlist
get_finest_classification_Asia_Main <- get_finest_classification_Asia_Main %>% 
  dplyr::mutate(
    level_1 = map_chr(level_1, 1)
  )

# merge get_finest_classification_Asia_Main and NA_Asia_Main_clean to get 
# taxon_name for future merging with Birks

NA_Asia_Main_clean <- NA_Asia_Main_clean %>% 
  mutate(
    sel_name = taxon_clean_cap) %>% 
  select(-c(raw_name, level_1, taxon_clean, taxon_clean_cap))


classification_with_taxon_names_Asia_Main <- 
  left_join(get_finest_classification_Asia_Main, NA_Asia_Main_clean, by = "sel_name")


### merge classification_with_taxon_names_Asia_Main with Birks to final table ----
# level_1 are the harmonisation
harmonisation_table_Asia_Main <- joined_empty_Birks_tables_Asia_Main %>%
  left_join(
    classification_with_taxon_names_Asia_Main %>%
      select(taxon_name, level_1),
    by = "taxon_name",
    suffix = c("", "_new")
  ) %>%
  mutate(
    level_1 = coalesce(level_1, level_1_new)
  ) %>%
  select(-level_1_new)




## Asia Siberia ----

### harm table a ----
joined_empty_Birks_tables_Asia_Siberia

### harm table b ---- 

# unlist
get_finest_classification_Asia_Siberia <- get_finest_classification_Asia_Siberia %>% 
  dplyr::mutate(
    level_1 = map_chr(level_1, 1)
  )

# merge get_finest_classification_Asia_Siberia and NA_Asia_Siberia_clean to get 
# taxon_name for future merging with Birks

NA_Asia_Siberia_clean <- NA_Asia_Siberia_clean %>% 
  mutate(
    sel_name = taxon_clean_cap) %>% 
  select(-c(raw_name, level_1, taxon_clean, taxon_clean_cap))


classification_with_taxon_names_Asia_Siberia <- 
  left_join(get_finest_classification_Asia_Siberia, NA_Asia_Siberia_clean, by = "sel_name")


### merge classification_with_taxon_names_Asia_Siberia with Birks to final table ----
# level_1 are the harmonisation
harmonisation_table_Asia_Siberia <- joined_empty_Birks_tables_Asia_Siberia %>%
  left_join(
    classification_with_taxon_names_Asia_Siberia %>%
      select(taxon_name, level_1),
    by = "taxon_name",
    suffix = c("", "_new")
  ) %>%
  mutate(
    level_1 = coalesce(level_1, level_1_new)
  ) %>%
  select(-level_1_new)



## Europe ----

### harm table a ----
joined_empty_Birks_tables_Europe

### harm table b ---- 

# unlist
get_finest_classification_Europe <- get_finest_classification_Europe %>% 
  dplyr::mutate(
    level_1 = map_chr(level_1, 1)
  )

# merge get_finest_classification_Europe and NA_Europe_clean to get 
# taxon_name for future merging with Birks

NA_Europe_clean <- NA_Europe_clean %>% 
  mutate(
    sel_name = taxon_clean_cap) %>% 
  select(-c(raw_name, level_1, taxon_clean, taxon_clean_cap))


classification_with_taxon_names_Europe <- 
  left_join(get_finest_classification_Europe, NA_Europe_clean, by = "sel_name")


### merge classification_with_taxon_names_Europe with Birks to final table ----
# level_1 are the harmonisation
harmonisation_table_Europe <- joined_empty_Birks_tables_Europe %>%
  left_join(
    classification_with_taxon_names_Europe %>%
      select(taxon_name, level_1),
    by = "taxon_name",
    suffix = c("", "_new")
  ) %>%
  mutate(
    level_1 = coalesce(level_1, level_1_new)
  ) %>%
  select(-level_1_new)


## Indopacific ----

### harm table a ----
joined_empty_Birks_tables_Indopacific

### harm table b ---- 

# unlist
get_finest_classification_Indopacific <- get_finest_classification_Indopacific %>% 
  dplyr::mutate(
    level_1 = map_chr(level_1, 1)
  )

# merge get_finest_classification_Indopacific and NA_Indopacific_clean to get 
# taxon_name for future merging with Birks

NA_Indopacific_clean <- NA_Indopacific_clean %>% 
  mutate(
    sel_name = taxon_clean_cap) %>% 
  select(-c(raw_name, level_1, taxon_clean, taxon_clean_cap))


classification_with_taxon_names_Indopacific <- 
  left_join(get_finest_classification_Indopacific, NA_Indopacific_clean, by = "sel_name")


### merge classification_with_taxon_names_Asia_Levant with Birks to final table ----
# level_1 are the harmonisation
harmonisation_table_Indopacific <- joined_empty_Birks_tables_Indospecific %>%
  left_join(
    classification_with_taxon_names_Indopacific %>%
      select(taxon_name, level_1),
    by = "taxon_name",
    suffix = c("", "_new")
  ) %>%
  mutate(
    level_1 = coalesce(level_1, level_1_new)
  ) %>%
  select(-level_1_new)




## Latin America ----

### harm table a ----
joined_empty_Birks_tables_Latin_America
### harm table b ---- 

# unlist
get_finest_classification_Latin_America <- get_finest_classification_Latin_America %>% 
  dplyr::mutate(
    level_1 = map_chr(level_1, 1)
  )

# merge get_finest_classification_Latin_America and NA_Latin_America_clean to get 
# taxon_name for future merging with Birks

NA_Latin_America_clean <- NA_Latin_America_clean %>% 
  mutate(
    sel_name = taxon_clean_cap) %>% 
  select(-c(raw_name, level_1, taxon_clean, taxon_clean_cap))


classification_with_taxon_names_Latin_America <- 
  left_join(get_finest_classification_Latin_America, NA_Latin_America_clean, by = "sel_name")


### merge classification_with_taxon_names_Latin_America with Birks to final table ----
# level_1 are the harmonisation
harmonisation_table_Latin_America <- joined_empty_Birks_tables_Latin_America %>%
  left_join(
    classification_with_taxon_names_Latin_America %>%
      select(taxon_name, level_1),
    by = "taxon_name",
    suffix = c("", "_new")
  ) %>%
  mutate(
    level_1 = coalesce(level_1, level_1_new)
  ) %>%
  select(-level_1_new)


## North America ----

### harm table a ----
joined_empty_Birks_tables_North_America

### harm table b ---- 
# unlist
get_finest_classification_North_America <- get_finest_classification_North_America %>% 
  dplyr::mutate(
    level_1 = map_chr(level_1, 1)
  )

# merge get_finest_classification_North_America and NA_North_America_clean to get 
# taxon_name for future merging with Birks

NA_North_America_clean <- NA_North_America_clean %>% 
  mutate(
    sel_name = taxon_clean_cap) %>% 
  select(-c(raw_name, level_1, taxon_clean, taxon_clean_cap))


classification_with_taxon_names_North_America <- 
  left_join(get_finest_classification_North_America, NA_North_America_clean, by = "sel_name")


### merge classification_with_taxon_names_North_America with Birks to final table ----
# level_1 are the harmonisation
harmonisation_table_North_America <- joined_empty_Birks_tables_North_America %>%
  left_join(
    classification_with_taxon_names_North_America %>%
      select(taxon_name, level_1),
    by = "taxon_name",
    suffix = c("", "_new")
  ) %>%
  mutate(
    level_1 = coalesce(level_1, level_1_new)
  ) %>%
  select(-level_1_new)



#------------------------------------------------------------------------------#
# 8. Putting together all harm tables -----
#------------------------------------------------------------------------------#

harmonisation_tables <- list(
  Asia_Levant = harmonisation_table_Asia_Levant,
  Asia_Main = harmonisation_table_Asia_Main,
  Asia_Siberia = harmonisation_table_Asia_Siberia,
  Europe = harmonisation_table_Europe,
  IndoPacific = harmonisation_table_Indopacific,
  Latin_America = harmonisation_table_Latin_America,
  North_America = harmonisation_table_North_America
)


harmonisation_tables_df <- tibble(
  harmonisation_region = names(harmonisation_tables),
  harm_table = harmonisation_tables
)


#----------------------------------------------------------#
# 9. Harmonise data -----
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
# 10. Save the data  -----
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








