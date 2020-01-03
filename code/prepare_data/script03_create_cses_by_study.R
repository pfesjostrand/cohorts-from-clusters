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

cses_main <- readRDS(PATHS$outp$cses_main)

# Generate by_election and by_eletion$data1 -----------------------------------
by_study <- cses_main %>% 
  group_by(year, polity, n_study, id_study, election_type, formula_lh, FH_rating, region) %>% 
  nest() %>%
  rename("data1" = data)

by_study$data1 <- map(by_study$data1, as_tibble)
  
names(by_study$data1) <- by_study$id_study

#FIXME no joining without by specified please, but shouldn't be a problem
by_study <- by_study[c(1:4)] %>% group_by(polity) %>% 
  mutate("polity_range" = (max(year) - min(year))) %>%
  right_join(by_study)

# Function renames into uniquie party ID and removes superfulous ID columns.
renamealpha <- function(data) {

  idA <- as.character(unique(data$IMD5000_A))
  idB <- as.character(unique(data$IMD5000_B))
  idC <- as.character(unique(data$IMD5000_C))
  idD <- as.character(unique(data$IMD5000_D))
  idE <- as.character(unique(data$IMD5000_E))
  idF <- as.character(unique(data$IMD5000_F))
  
# if there are multiple IDs something is wrong with the grouping. Call error.
  stopifnot(length(idA) < 2 & length(idB) < 2 & length(idC) < 2 &
    length(idD) < 2 & length(idE) < 2 & length(idF) < 2)
  
# remove the colums containing numeric ID as they are no longer necessary
  data <- select(data, -IMD5000_A, -IMD5000_B, -IMD5000_C, 
                       -IMD5000_D, -IMD5000_E, -IMD5000_F)
  
# If the id was a missing value remove the alphanumeric column since it is not
# useful; including it will bring errors due to identical column names. Else
# rename the column according to the party-ID making it a unique column name.

  if (idA == 9999999) {
    data <- select(data, -IMD3008_A)
  } else {
    data <- rename(data, !!idA := IMD3008_A)
  }

  if (idB == 9999999) {
    data <- select(data, -IMD3008_B)
  } else {
    data <- rename(data, !!idB := IMD3008_B)
  }

  if (idC == 9999999) {
    data <- select(data, -IMD3008_C)
  } else {
    data <- rename(data, !!idC := IMD3008_C)
  }

  if (idD == 9999999) {
    data <- select(data, -IMD3008_D)
  } else {
    data <- rename(data, !!idD := IMD3008_D)
  }

  if (idE == 9999999) {
    data <- select(data, -IMD3008_E)
  } else {
    data <- rename(data, !!idE := IMD3008_E)
  }

  if (idF == 9999999) {
    data <- select(data, -IMD3008_F)
  } else {
    data <- rename(data, !!idF := IMD3008_F)
  }

  return(data)
}

by_study$data1 <- by_study$data1 %>% map(renamealpha)

names(by_study$data1) <- by_study$id_study

# Generate by_election$data2; -------------------------------------------------

# Calculate party level
party_data <- cses_main %>%
  group_by(polity, year, id_study, vote_lh) %>%
  mutate("n_vote_lh" = n()) %>% # number that voted for a party
  ungroup() %>%
  mutate("p_vote_lh" = n_vote_lh / n_study) %>% # part that voted for a party
  group_by(id_study, polity, year, survey_year, n_study, vote_lh, n_vote_lh, p_vote_lh) %>%
  summarise(
    "avg_birth_year" = mean(birth_year, na.rm = T),
    "avg_pid"        = mean(pid_degree, na.rm = T),
    "avg_education"  = mean(education, na.rm = T),
    "avg_left_right" = mean(left_right, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(year, polity, n_study, id_study) %>%
  nest() %>%
  rename("data2" = data) %>%
  ungroup()

party_data$data2 <- map(party_data$data2, as.tibble)
names(party_data$data2) <- by_study$id_study

by_study <- right_join(party_data, by_study, by = c("id_study", "polity", "year", "n_study")) %>%
  ungroup()

rm(party_data)

# Reorder the colums for clarity
by_study$data1 <- by_study$data1 %>%
  map(select, id, vote_lh, everything())
by_study$data2 <- by_study$data2 %>%
  map(select, vote_lh, everything())
by_study <- by_study %>%
  select(id_study, polity, region, year, polity_range, election_type, formula_lh, FH_rating, data1, data2, everything())
by_study <- by_study %>% arrange(id_study)

# Save data -------------------------------------------------------------------
saveRDS(by_study,  here("output", "data", "products", "cses_by_study"))

# Clean env -------------------------------------------------------------------

rm(cses_main); rm(by_study); rm(renamealpha)
gc()

