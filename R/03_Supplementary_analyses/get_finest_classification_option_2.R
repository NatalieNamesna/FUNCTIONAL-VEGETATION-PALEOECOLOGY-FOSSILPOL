# get table of finest classification option 2
get_finest_classification_Africa <-
  classification_Africa %>% 
  dplyr::select(sel_name, classification) %>% 
  dplyr::mutate(
    is_classification_null = purrr::map_lgl(
      .x = classification,
      .f = ~ all(is.null(.x))
    ),
    classification_no_null = purrr::map2(
      .x = is_classification_null,
      .y = classification,
      .f = ~ {
        if (isTRUE(.x)) {
          return(tibble(
            name = NA_character_,
            rank = NA_character_
          ))
        }
        return(.y)
      }
    )) %>% 
  dplyr::select(sel_name, classification_no_null) %>% 
  tidyr::unnest(classification_no_null) %>% 
  dplyr::select(-id) %>%
  dplyr::distinct() %>% 
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
