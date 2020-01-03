#!/usr/bin/env Rscript

library("tidyverse")
library("here")
library("xpfe")

# Set paths
source(here("code", "script_set_paths.R"))

# 1 Load cses_main data set
# FIXME find out why I get 
# Warning Column `polity` has different attributes on LHS and RHS of join 
source(here("code", "prepare_data", "script01_load_cses_main.R"))

# 2 Recode cses_main data set. Creates the two cses pid indexes.
# TODO put cses pid index creation in a seperate file as for snes.
source(here("code", "prepare_data", "script02_recode_cses_main.R"))

# 3 Create cses_by_study data set
source(here("code", "prepare_data", "script03_create_cses_by_study.R"))

# 4 Create cses_by_polity data set
source(here("code", "prepare_data", "script04_create_cses_by_polity.R"))

# 5 Combines snes data from different years into one data set and homogenizes 
# A lot of work still required here but some variables are done.
source(here("code", "prepare_data", "script05_load_recode_snes_main.R"))

# 6 Creates the snes pid index. 
source(here("code", "prepare_data", "script06_create_snes_pid_index.R"))

# 7 Combines the SNES and CSES data for Sweden into a small per variable dataset
# but BIG in rows dataset.
source(here("code", "prepare_data", "script07_create_combined_data_for_sweden.R"))

# 8 Imputes CSES data for use in cluster analysis. Currently not run. 
# source(here("code", "prepare_data", "script08_create_imputed_cses_data.R "))

# Create se_cses, de_cses, us_cses and imputed versions , se_am2, de_am2, us_am2

 