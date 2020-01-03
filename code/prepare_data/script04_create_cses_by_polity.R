# FILE DESCRIPTION -----------------------------------------------------------  
# Data preperation script 3
#
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# This script requires: 'dplyr', 'readr', 'purrr', 'here'           

# Setup env -------------------------------------------------------------------

stopifnot(
 "dplyr" %in% (.packages()) &&
 "readr" %in% (.packages()) &&
 "purrr" %in% (.packages()) &&
 "here"  %in% (.packages()))

# Config containing mainly paths
# source(here("code", "script_set_paths.R"))

# Load data -------------------------------------------------------------------

by_study <- readRDS(PATHS$outp$cses_by_study)

# Generate by_polity datasets -------------------------------------------------

by_polity <- by_study %>% 
  filter((FH_rating <= 2) & (election_type %in% c(10, 12, 13))) %>%
  group_by(polity, region, polity_range) %>%
  summarise() %>%
  ungroup()

tmp <- by_study %>%
  ungroup() %>%
  filter((FH_rating <= 2) & (election_type %in% c(10, 12, 13))) %>%
  select(-data2, -n_study, -polity_range, -election_type, -formula_lh, -FH_rating, -region)

by_polity <- suppressWarnings(
  unique(tmp$polity) %>%
  map(function(select_polity, data = tmp) {
    tmp <- filter(tmp, polity == select_polity) %>%
      unnest(cols = c("data1")) %>%
      select(-age_cat) %>%
      select(id, id_study, year, survey_year, age, birth_year, everything()) %>%
      group_by(polity) %>%
      nest() %>%
      ungroup()

    stopifnot(dim(tmp)[1] == 1)
    return(tmp)
  }) %>%
  bind_rows() %>%
    right_join(by_polity, by = "polity") %>%
    select(polity, region, polity_range, everything())
)

rm(tmp)

# Save data -------------------------------------------------------------------

saveRDS(by_polity, here("output", "data", "products", "cses_by_polity"))

# Clean env -------------------------------------------------------------------

rm(by_study); rm(by_polity)

