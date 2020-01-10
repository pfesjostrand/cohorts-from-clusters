# FILE DESCRIPTION                                                          
# Config file for the code in the project 
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# Sets paths for the data read and data output. 


# Do not change
stopifnot("here" %in% (.packages()))
PATHS      <- list()
PATHS$data <- list()
PATHS$outp <- list()
PATHS$figs <- list()



# DATA INPUT CSES -------------------------------------------------------------

# Path to CSES IMD .rdata file downloadable online
PATHS$data$cses_imd <- 
  here("data", "fixed_cses", "imd", "20191017", "cses_imd.rdata")

# Path to CSES metadata 'master' excel sheet for NA coding and information
PATHS$data$cses_meta_master <-
  here("data", "cses_metadata_master_sheet.xlsx")

# Path to CSES metadata sheet with party names for recoding from alphanumeric codes
PATHS$data$cses_meta_parties <-
  here("data", "cses_metadata_party_names.csv")

# Path to CSES metadata sheet with polity-regions for adding such information
PATHS$data$cses_meta_regions <-
  here("data", "cses_metadata_politys_regions.csv")

# DATA INPUT SNES -------------------------------------------------------------

# Path to SNES data file directory.
PATHS$data$snes_data_dir <- here("data", "fixed_snes", "sav")



# DATA OUTPUT CSES ------------------------------------------------------------

PATHS$outp$cses_main  <- 
  here("output", "data", "products", "cses_main")

PATHS$outp$cses_main_metadata <- 
  here("output", "data", "products", "cses_main_metadata")

PATHS$outp$cses_by_study <- 
  here("output", "data", "products", "cses_by_study")

PATHS$outp$cses_by_polity <- 
  here("output", "data", "products", "cses_by_polity")

PATHS$outp$se_cses <-
  here("output", "data", "products", "se_cses")

PATHS$outp$de_cses <-
  here("output", "data", "products", "de_cses")

PATHS$outp$se_am2 <-
  here("output", "data", "products", "se_am2")

PATHS$outp$de_am2 <-
  here("output", "data", "products", "de_am2")

# DATA OUTPUT SNES ------------------------------------------------------------

PATHS$outp$snes_main  <- 
  here("output", "data", "products", "snes_main")

PATHS$outp$pidTestDf <- 
  here("output", "data", "products", "test_df_for_cses_pid")



# DATA OUTPUT FOR ANALYSSIS IN PAPER 1 AND COMBINED -----------------------------

PATHS$outp$se_combined  <- 
  here("output", "data", "products", "se_combined")

PATHS$outp$se_snes_small  <- 
  here("output", "data", "products", "se_snes_small")

PATHS$outp$se_cses_small  <- 
  here("output", "data", "products", "se_cses_small")

PATHS$outp$se_combined_analysis  <- 
  here("output", "data", "paper1", "se_combined")

PATHS$outp$se_snes_small_analysis  <- 
  here("output", "data", "paper1", "se_snes_small")

PATHS$outp$se_cses_small_analysis  <- 
  here("output", "data", "paper1", "se_cses_small")


# FIGS  -----------------------------

PATHS$figs$left_right_placement <- 
  here("figs", "left_right_placement")
