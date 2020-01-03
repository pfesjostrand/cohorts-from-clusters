# FILE DESCRIPTION                                                          
# ***************************************************************************** 
# Further data preperation for cohorts-from-clusters project. This script take
# prepared data from cses and snes scripts and creates a combined dataset for
# Sweden. 
#
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# This script requires: 'dplyr', 'tibble', 'purrr', 'here', 'xpfe', 'amelia'
# ***************************************************************************** 

# Setup env -------------------------------------------------------------------

stopifnot(
 "dplyr" %in% (.packages()) &&
 "readr" %in% (.packages()) &&
 "purrr" %in% (.packages()) &&
 "here"  %in% (.packages()) &&
 "xpfe"  %in% (.packages()))

#source(here("code", "script_set_paths.R"))

# Load data -------------------------------------------------------------------

by_polity <- readRDS(PATHS$outp$cses_by_polity)
snes_main <- readRDS(PATHS$outp$snes_main)

# Deselect / Simply CSES data -------------------------------------------------

se_cses <- by_polity %>%
  ungroup() %>%
  filter(polity == "Sweden") %>%
  unnest(cols = c("data")) %>% 
  select(-polity, -region, -year, -polity_range) %>%
  mutate("id_study" = paste("CSES", survey_year, sep="_")) %>%
  rename("ID" = id, "ID_study" = id_study, "left_right_placement" = left_right, 
         "urban_rural" = urbanity)

# TODO I remove variables for which SNES have not been homogenized yet but these
# should be homogenized and coded so they can be joined with the snes_main data 
# if possible. 
se_cses <- select(se_cses, -age, -urban_rural, -education, -income, -vote_lh,
                  -dem_sat, -dem_power, -religion, -married, -pid_party, -pid_3005_2,
                  -pid_3005_1, -eval_fp, -eval_kd,
                  "cses_pid_index_1" = pid_index_1,
                  "cses_pid_index_2" = pid_index_2,
                  "cses_pid_degree" = pid_degree)

stopifnot(length(se_cses$ID) == length(unique(se_cses$ID)))

# Deselect / Simply SNES data -------------------------------------------------

# Deselect variables used for generation of indexes but not neded. 
snes <- snes_main %>%
  select(ID, ID_study, birth_year, gender, "snes_pid_index" = pid_index, survey_year,
         left_right_placement, eval_c, eval_m, eval_v, eval_s, pid_1)

# TODO fix so that ID is named the same earlier in the pipe. Also types should
# be fixed earlier in the pipe. 
snes$ID <- as.character(snes_main$ID)

# Join data sets --------------------------------------------------------------

# ! NOTE SE_CSES HAS TO BE FILTERD ON SURVEY YEAR BEFORE USE SO THAT RESPONDENTS ARE NOT DUBLICATED ! #
se_combined <- se_cses %>% 
  filter(survey_year > 2002) %>%
  bind_rows(snes) %>%
  mutate("age" = survey_year-birth_year) %>%
  select(ID, ID_study, survey_year, birth_year, age, gender, 
         snes_pid_index, cses_pid_index_1, cses_pid_index_2,
         cses_pid_degree, left_right_placement, everything())

se_cses_small <- se_cses %>% 
  mutate("age" = survey_year-birth_year) %>%
  select(ID, ID_study, survey_year, birth_year, age, gender, 
         cses_pid_index_1, cses_pid_index_2, cses_pid_degree, 
         left_right_placement, everything())

se_snes_small <- snes %>% 
  mutate("age" = survey_year-birth_year) %>%
  select(ID, ID_study, survey_year, birth_year, age, gender, 
         snes_pid_index, left_right_placement, everything())

 
# check for duplicate respondents / surveys
#stopifnot(length(se_combined$ID) == length(unique(se_combined$ID)))


# Save data ------------------------------------------------------------------

saveRDS(se_combined,   PATHS$outp$se_combined)
saveRDS(se_cses_small, PATHS$outp$se_combined)
saveRDS(se_snes_small, PATHS$outp$se_combined)

saveRDS(se_combined,   PATHS$outp$se_combined_analysis)
saveRDS(se_cses_small, PATHS$outp$se_snes_small)
saveRDS(se_snes_small, PATHS$outp$se_cses_small)



# Clean env ------------------------------------------------------------------

rm(se_combined); rm(snes); rm(se_cses); rm(by_polity)
rm(se_cses_small); rm(se_snes_small); rm(snes_main)
gc()
