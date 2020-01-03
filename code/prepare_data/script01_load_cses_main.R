# FILE DESCRIPTION -----------------------------------------------------------  
# Inital data preperation for cohorts-from-clusters project. 
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# This script requires: 'dplyr', 'readr', 'purrr', 'here'           

# Load CSES IMD and metadata from fixed data files. Paths are specified in the
# /code/config.R file sourced below The script creates a cses_main dataset w.
# no recodings. Seperate script handles recoding. 


# Setup env -------------------------------------------------------------------

stopifnot(
 "dplyr" %in% (.packages()) &&
 "readr" %in% (.packages()) &&
 "purrr" %in% (.packages()) &&
 "here"  %in% (.packages()))

# Config containing mainly paths
# source(here("code", "script_set_paths.R"))


# Load data -------------------------------------------------------------
#* Load data and create a raw object for reference. Clean attributes and load
#* metadata shees, check the metadata for errors. 

loadRdata <- function(file) {load(file); get(ls())}
cses_raw  <- loadRdata(PATHS$data$cses_imd)
cses_main <- cses_raw

attributes(cses_main) <- NULL
names(cses_main)      <- names(cses_raw)
cses_main             <- as_tibble(cses_main)

metadata <- xpfe::read_excel_sheet(
  PATHS$data$cses_meta_master, 
  intermediary_path = here("output", "tmp", ".intermediary.csv"))

# Error checks: All cses names in metadata_old must match a name in cses_raw
# There should not be any duplicate names in any of the data sets          
                          
stopifnot(all(metadata$names_old %in% names(cses_raw)))         
stopifnot(length(unique(metadata$names_old))  == length(metadata$names_old))
stopifnot(length(unique(metadata$names_new))  == length(metadata$names_new))
stopifnot(length(unique(names(cses_raw))) == length(names(cses_raw)))
stopifnot(length(unique(names(cses_main)))     == length(names(cses_main)))


# Change variables ------------------------------------------------------------
#' Select subset of variables, and rename to make them more easy to work with.
#' Selection and renaming are done according to metadata sheets, all variables
#' that are to be keept need to be specified by the metadata spreadsheet.

# Selects those variable specified in the metadata frame
cses_main %>%
  select_if(names(.) %in% metadata$names_old) ->
  cses_main

# Rearrange data cols to match the order in metadata, then rename by position.
cses_main        <- cses_main[match(metadata$names_old, names(cses_main))]
names(cses_main) <- metadata$names_new

# Assign cses names as attributes to cses_main. 
for(i in seq_along(cses_main)) {
  attr(cses_main[[i]], "cses_name") <- metadata$names_old[i]
  }; rm(i)

# Add region (Europe, North America etc.) to cses_main data
readr::read_csv(PATHS$data$cses_meta_regions) %>%
  select(polity, region) %>%
  right_join(cses_main, by = "polity") ->
  cses_main

# Avoid numeric class on the vote choice 
cses_main$vote_lh <- as.character(cses_main$vote_lh)

# Calculate birth year
cses_main$birth_year <- cses_main$survey_year - cses_main$age

# Compute number of individuals in each election
cses_main %>%
  group_by(polity, year, id_study) %>%
  mutate("n_study" = n()) %>%
  ungroup() ->
  cses_main 

# IMD2004, maritial status, is recoded into char to make it categorical
cses_main$married <- as.character(cses_main$married)

# IMD2005, religion , is recoded into into char to make it categorical
cses_main$religion <- as.character(cses_main$religion)

# Exclude belgium since it causes problems due to multiple parties in the like
# / dislike variables for unique polity and year. Unsure why this is; get back
# and investigate this. Belgium only has obs. over few years so no real loss, 
# i.e. it would not be used in analysis anyway. 
cses_main <- cses_main %>% 
  filter(polity != "Belgium")

# Get metadata containing party names and corresponding cses code
party_names <- read_csv(
  PATHS$data$cses_meta_parties, 
  col_types = cols(cses_code = col_character())
  )

# Rename those codes that match a name in party_names
tmp <- cses_main[c("IMD5000_A", "IMD5000_B", "IMD5000_C", "IMD5000_D", "IMD5000_E", "IMD5000_F")]
for (i in seq_along(party_names$cses_code)) {
  code <- party_names$cses_code[[i]]
  for (k in seq_along(tmp)) {
    col <- tmp[[k]]
    col[col == code] <- party_names$short_name[[i]]
    tmp[[k]] <- col
  }
}
tmp -> cses_main[c("IMD5000_A", "IMD5000_B", "IMD5000_C", "IMD5000_D", "IMD5000_E", "IMD5000_F")]
rm(tmp); rm(col)

# Save data -------------------------------------------------------------------
saveRDS(cses_main, PATHS$outp$cses_main)
saveRDS(metadata,  PATHS$outp$cses_main_metadata)

# Clean env -------------------------------------------------------------------

rm(cses_main); rm(cses_raw); rm(metadata); rm(party_names); rm(code); rm(i); rm(k); rm(loadRdata)
gc()

