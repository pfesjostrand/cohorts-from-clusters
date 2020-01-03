# FILE DESCRIPTION -----------------------------------------------------------  
# Inital data preperation for cohorts-from-clusters project. 
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# This script requires: 'dplyr', 'readr', 'purrr', 'here'           
# Load SNES data perform subselection and inital necessary recoding and join 
# into one dataset. Further recoding is done in a seperate file for clarity.

# TODO: (IMPORTANCE HIGH) Error check these operaitons.
# TODO: (IMPORTANCE LOW) Simplification of these operations. 
# TODO: (IMPORTANCE MED) Expand the dataset with some class / income measure + rural/urban measure
# TODO: (IMPORTANCE LOW) Expand the dataset produced for more uses and EDA.

# Setup env -------------------------------------------------------------------

stopifnot(
 "dplyr" %in% (.packages()) &&
 "readr" %in% (.packages()) &&
 "purrr" %in% (.packages()) &&
 "here"  %in% (.packages()))

# Config containing mainly paths
# source(here("code", "script_set_paths.R"))

# RECODING METHODOLOGY --------------------------------------------------------
#
# STEP/EXPLANTION
#
# 1. I define tmp_reader functions and a namekey containing the SPSS labe attr 
# of each variable and a correspodning name I want it changed into. 
#
# tmp_reader does the following: i. load the data ii. renames the variables into
# their label iii. select those vars where the name (label) match the namekey
# iv. renames the variable acc to the namekey creating unifified variables names
# that can be joined.
# 
# 2. I go through each year and make new unified coding for the parties. Before
# 1994 block options where used (resp could specify a combination of parties as
# the party resp feelt closest to), I create a new variable indiciating if the
# resp picked a block option.
#
# 3. I join the sets from different years together into one unified data frame.

# STEP 1A. Define namekeys ----------------------------------------------------
# Namekey contains old name of variable (its label attr.) as a name attribute &
# new name as value. It is a mess since SNES keeps changing how they name vars.

namekey <- list()

# 2010 available dataset do notw contain precise birth year. Will not be used. 
namekey$VU2010 <- c(
  "ID-nummer" = "ID", "SCB: Kön" = "gender",
  "F.29A Partianahängare"     = "pid_1", 
  "F.29B Bästa parti"         = "pid_2_partyA", 
  "F.29C Stark anhängare"     = "pid_3_degree",
  "F.29D Närmare något parti" = "pid_4_partyB",
  "F.9AA Gillar/ogillar politiska partier: Centerpartiet"      = "eval_c",
  "F.9AA Gillar/ogillar politiska partier: Moderaterna"        = "eval_m",
  "F.9AA Gillar/ogillar politiska partier: Vänsterpartiet"     = "eval_v",
  "F.9AA Gillar/ogillar politiska partier: Socialdemokraterna" = "eval_s"
  )

# 2006 available dataset do not contain precise birth year. Will not be used. 
namekey$VU2006 <- c(
  "ID-nummer" = "ID", "SCB Kön" = "gender", 
  "F.43A Partianhängare"      = "pid_1", 
  "F.43B Partipreferens"      = "pid_2_partyA", 
  "F.43C Partiidentifikation" = "pid_3_degree",
  "F.43D Närmaste parti"      = "pid_4_partyB",
  "F.9AA Inställning till centerpartiet"      = "eval_c", 
  "F.9AB Inställning till moderaterna"        = "eval_m",
  "F.9AC Inställning till vänsterpartiet"     = "eval_v", 
  "F.9AE Inställning till socialdemokraterna" = "eval_s"
  )
  
namekey$VU2002 <- c(
  "ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Partipreferens och valdeltagande - Närmaste parti" = "pid_4_partyB",
  "Inställning till Centerpartiet"            = "eval_c", 
  "Inställning till Moderata samlingspartiet" = "eval_m",
  "Inställning till Vänsterpartiet"           = "eval_v", 
  "Inställning till Socialdemokraterna"       = "eval_s"
  )

namekey$VU1998 <- c(
  "ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Närmaste parti"      = "pid_4_partyB",
  "Inställning till Centerpartiet"            = "eval_c", 
  "Inställning till Moderata samlingspartiet" = "eval_m",
  "Inställning till Vänsterpartiet"           = "eval_v", 
  "Inställning till Socialdemokraterna"       = "eval_s"
  )

namekey$VU1994 <- namekey$VU1998
namekey$VU1991 <- namekey$VU1998

namekey$VU1988 <- c(
  "UP:s ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Närmaste parti"      = "pid_4_partyB",
  "Centerpartiet"                = "eval_c", 
  "Moderata samlingspartiet"     = "eval_m", 
  "Socialdemokraterna"           = "eval_s", 
  "Vänsterpartiet kommunisterna" = "eval_v"
  )

namekey$VU1985 <- namekey$VU1988
namekey$VU1982 <- namekey$VU1988
namekey$VU1979 <- namekey$VU1988

namekey$VU1976 <- c(
  "UP:s ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Närmaste parti"      = "pid_4_partyB"
  )

namekey$VU1973 <- c(
  "UP:s ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Bästa parti"         = "pid_4_partyB"
  )

# STEP 1B. Define the function -------------------------------------------------

tmp_reader <- function(path, namekey=NULL, subset=NULL, select=T, rename=T) {
  
  # Read and rename variables into their 'label' attribute
  x <- haven::read_sav(path) 
  if(!(is.null(subset))) x <- x[subset]
  new_names <- map_chr(x, attr, 'label')
  names(x)  <- new_names
  
  # Deselect non-unique names, which for some strange reason appear,
  # not important in this project since the concerned variables are not used. 
  x <- x[(unique(names(x)))]
  
  # Select a subselection of variables and rename after namekey
  if(select==T) x <- select(x, one_of(names(namekey)))
  if(rename==T) names(x) <- namekey[names(x)]
  return(x)
}

# STEP 2. Load, subselect, rename necessary inital recoding -------------------

# Output is saved into the following list
snes_files <- list()

# # 2010 ------------------------------------------------------------------------
# EXCLUDED BECAUSE BIRTH YEAR NOT INCLUDED CSES DATA USED INSTEAD
# snes_files$VU2010 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU2010.sav", sep = "/"),
#            namekey$VU2010, subset = NULL, select = T, rename = T)
# 
# attr(snes_files$VU2010$pid_4_partyB, 'labels') <- NULL
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 1]  <- "V"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 2]  <- "S"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 3]  <- "C"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 4]  <- "Fp"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 5]  <- "M"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 6]  <- "Kd"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 7]  <- "Mp"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 8]  <- "SD"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 9]  <- "Fi"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 10] <- "PIRAT"
# snes_files$VU2010$pid_4_partyB[snes_files$VU2010$pid_4_partyB == 12] <- "other"
# 
# snes_files$VU2006$pid_4_partyB[
#   snes_files$VU2006$pid_4_partyB %in% c(0,55,88,96,99)] <- NA
# 
# snes_files$VU2006$pid_4_partyB[
#   snes_files$VU2006$pid_4_partyB %in% c(8886,8888,9992,9994,9998)] <- NA


# 2006 ------------------------------------------------------------------------
# EXCLUDED BECAUSE BIRTH YEAR NOT INCLUDED CSES DATA USED INSTEAD
# VU2006 only used to get test data for checking correlation of pid_degree
# variable coded from CSES data with pid_degree coded from VU data.

# FIXME Running tmp_reader on 2006 and 2010 only possible if input to name(x) is a
# list for unknown reason. This does not work with some older datasets. I have not
# prioritized solving this. Using work around for now.
## snes_files$VU2006 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU2006.sav", sep = "/"),
##            namekey$VU2006, subset = NULL, select = F, rename = F)
  
test_df_for_cses_pid <- haven::read_sav(
  paste(PATHS$data$snes_data_dir, "VU2006.sav", sep = "/")) %>%
  select("ID" = ID, "VU_pid_1" = V580, "VU_pid_2_partyA_v" = V581, 
         "VU_pid_3_degree" = V582, "VU_pid_4_partyB" = V583, "pid_3005_1" = V728, 
         "pid_3005_2" = V729, "pid_party" = V730, "pid_degree" = V731)

attr(test_df_for_cses_pid$VU_pid_4_partyB, 'labels') <- NULL
 
test_df_for_cses_pid$VU_pid_4_partyB[
  test_df_for_cses_pid$VU_pid_4_partyB == 1] <- "V"
 
test_df_for_cses_pid$VU_pid_4_partyB[
  test_df_for_cses_pid$VU_pid_4_partyB == 2] <- "S"
 
test_df_for_cses_pid$VU_pid_4_partyB[
  test_df_for_cses_pid$VU_pid_4_partyB == 3] <- "C"
 
test_df_for_cses_pid$VU_pid_4_partyB[
  test_df_for_cses_pid$VU_pid_4_partyB == 4] <- "Fp"
 
test_df_for_cses_pid$VU_pid_4_partyB[
  test_df_for_cses_pid$VU_pid_4_partyB == 5] <- "M"
 
test_df_for_cses_pid$VU_pid_4_partyB[
  test_df_for_cses_pid$VU_pid_4_partyB == 6] <- "Kd"
 
test_df_for_cses_pid$VU_pid_4_partyB[
  test_df_for_cses_pid$VU_pid_4_partyB == 7] <- "Mp"

test_df_for_cses_pid$VU_pid_4_partyB[
   test_df_for_cses_pid$VU_pid_4_partyB == 20] <- "SD"

test_df_for_cses_pid$VU_pid_4_partyB[
   test_df_for_cses_pid$VU_pid_4_partyB == 21] <- "Fi"

test_df_for_cses_pid$VU_pid_4_partyB[
   test_df_for_cses_pid$VU_pid_4_partyB == 22] <- "JUNILISTAN"

test_df_for_cses_pid$VU_pid_4_partyB[
   test_df_for_cses_pid$VU_pid_4_partyB == 9] <- "other"
 
test_df_for_cses_pid$VU_pid_4_partyB[
   test_df_for_cses_pid$VU_pid_4_partyB %in% c(0,55,88,95,96,98,99,998,998)] <- NA

# 2002 ------------------------------------------------------------------------
snes_files$VU2002 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU2002.sav", sep = "/"),
           namekey$VU2002, subset = NULL, select = T, rename = T)

attr(snes_files$VU2002$pid_4_partyB, 'labels') <- NULL
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 1] <- "V"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 2] <- "S"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 3] <- "C"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 4] <- "Fp"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 5] <- "M"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 6] <- "Kd"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 7] <- "Mp"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB == 9] <- "other"
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB %in% c(0, 55, 88, 96,99)] <- NA

snes_files$VU2002$ID_study <- "VU2002"

# 1998 ------------------------------------------------------------------------
snes_files$VU1998 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1998.sav", sep = "/"),
           namekey$VU1998, subset = NULL, select = T, rename = T)

# Recode from numerics to short party names, drop explanatory labels. 
attr(snes_files$VU1998$pid_4_partyB, 'labels') <- NULL
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 1] <- "V"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 2] <- "S"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 3] <- "C"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 4] <- "Fp"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 5] <- "M"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 6] <- "Kd"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 7] <- "Mp"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB == 9] <- "other"
snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB %in% c(0, 55, 88, 96,99)] <- NA

snes_files$VU1998$ID_study <- "VU1998"

# 1994 ------------------------------------------------------------------------
snes_files$VU1994 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1994.sav", sep = "/"),
           namekey$VU1994, subset = NULL, select = T, rename = T)


# Recode from numerics to short party names, drop explanatory labels. 
attr(snes_files$VU1994$pid_4_partyB, 'labels') <- NULL
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 1] <- "V"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 2] <- "S"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 3] <- "C"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 4] <- "Fp"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 5] <- "M"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 6] <- "Kd"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 7] <- "Mp"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 8] <- "NyD"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB == 9] <- "other"
snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB %in% c(0, 88, 96,99)] <- NA

snes_files$VU1994$ID_study <- "VU1994"

# 1991 ------------------------------------------------------------------------
snes_files$VU1991 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1991.sav", sep = "/"),
           namekey$VU1991, subset = NULL, select = T, rename = T)

# Binary variable indicating if a combination of parties given as best party.
snes_files$VU1991$pid_combination <- FALSE
snes_files$VU1991$pid_combination[
  snes_files$VU1991$pid_4_partyB %in% c(71)] <- TRUE

attr(snes_files$VU1991$pid_4_partyB, 'labels') <- NULL

# Note that the inclusion of a bloc in this question meses up the time series a lot. 
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 11] <- "V"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 22] <- "S"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 33] <- "C"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 44] <- "Fp"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 55] <- "M"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 66] <- "Kd"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 9]  <- "Mp"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 10] <- "NyD"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 87] <- "other"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB == 71] <- "right-bloc"
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB %in% c(0, 88, 96,99)] <- NA

snes_files$VU1991$ID_study <- "VU1991"

# 1988 ------------------------------------------------------------------------
snes_files$VU1988 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1988.sav", sep = "/"),
           namekey$VU1988, subset = NULL, select = T, rename = T)

# Binary variable indicating if a combination of parties given as best party.
snes_files$VU1988$pid_combination <- FALSE
snes_files$VU1988$pid_combination[
  snes_files$VU1988$pid_4_partyB %in% c(23,24,47,71)] <- TRUE

attr(snes_files$VU1988$pid_4_partyB, 'labels') <- NULL

# Parties
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 11] <- "V"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 14] <- "SP"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 22] <- "S"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 33] <- "C"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 44] <- "Fp"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 55] <- "M"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 66] <- "Kd"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 9]  <- "Mp"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 82] <- "SKÅNE"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 17] <- "other_communist"

# Blocks
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 23] <- "S+communist"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 25] <- "S+Fp"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 47] <- "Vpk+Fp"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB == 71] <- "right-bloc"
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB %in% c(0, 88,96,99)] <- NA

snes_files$VU1988$ID_study <- "VU1988"

# 1985 ------------------------------------------------------------------------
snes_files$VU1985 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1985.sav", sep = "/"),
           namekey$VU1985, subset = NULL, select = T, rename = T)

snes_files$VU1985$pid_combination <- FALSE
snes_files$VU1985$pid_combination[
  snes_files$VU1985$pid_4_partyB %in% c(23,25,47,58,71,72)] <- TRUE

attr(snes_files$VU1985$pid_4_partyB, 'labels') <- NULL

# Parties
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 9]  <- "Mp"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 11] <- "V"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 14] <- "SP"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 16] <- "ARB_comunist"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 17] <- "other_communist"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 22] <- "S"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 33] <- "C"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 44] <- "Fp"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 46] <- "LIB_partiet"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 55] <- "M"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 66] <- "Kd"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 82] <- "SKÅNE"

# Blocks
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 23] <- "S+communist"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 25] <- "S+Fp"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 47] <- "Vpk+Fp"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 58] <- "M+S"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 71] <- "right-bloc"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB == 72] <- "Fp+C"
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB %in% c(0,88,96,99)] <- NA

snes_files$VU1985$ID_study <- "VU1985"

# 1982 ------------------------------------------------------------------------
snes_files$VU1982 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1982.sav", sep = "/"),
           namekey$VU1982, subset = NULL, select = T, rename = T)

snes_files$VU1982$pid_combination <- FALSE
snes_files$VU1982$pid_combination[
  snes_files$VU1982$pid_4_partyB %in% c(23,25,47,58,71,72,77,79,80)] <- TRUE

attr(snes_files$VU1982$pid_4_partyB, 'labels') <- NULL

# Parties
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 9]  <- "Mp"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 11] <- "V"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 12] <- "KMFL"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 13] <- "KP"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 14] <- "SP"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 16] <- "ARB_comunist"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 17] <- "other_communist"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 22] <- "S"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 33] <- "C"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 44] <- "Fp"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 46] <- "LIB_partiet"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 55] <- "M"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 66] <- "Kd"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 82] <- "SKÅNE"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 85] <- "CENT_DEM"

# Blocks
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 23] <- "S+communist"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 25] <- "S+Fp"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 47] <- "Vpk+Fp"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 58] <- "M+S"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 71] <- "right-bloc"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 72] <- "Fp+C"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 77] <- "M+Fp"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 79] <- "C+Mp"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB == 80] <- "Fp+Mp"
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB %in% c(0,88,96,99)] <- NA

snes_files$VU1982$ID_study <- "VU1982"

# 1979 ------------------------------------------------------------------------

snes_files$VU1979 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1979.sav", sep = "/"),
           namekey$VU1979, subset = NULL, select = T, rename = T)

snes_files$VU1979$pid_combination <- FALSE
snes_files$VU1979$pid_combination[
  snes_files$VU1979$pid_4_partyB %in% c(23,25,47,58,71,72,73,76,77,79,80)] <- TRUE

attr(snes_files$VU1979$pid_4_partyB, 'labels') <- NULL

# Parties
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 11] <- "V"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 12] <- "KMFL"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 13] <- "KP"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 14] <- "SP"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 22] <- "S"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 33] <- "C"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 44] <- "Fp"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 55] <- "M"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 66] <- "Kd"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 82] <- "SKÅNE"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 84] <- "NYA_PARTIET"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 85] <- "CENT_DEM"

# Blocks
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 23] <- "S+communist"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 25] <- "S+Fp"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 47] <- "Vpk+Fp"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 58] <- "M+S"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 71] <- "right-bloc"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 72] <- "Fp+C"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 73] <- "right-local"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 76] <- "M+S"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 77] <- "M+Fp"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 79] <- "C+Mp"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB == 80] <- "Fp+Mp"
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB %in% c(0,88,96,99)] <- NA

snes_files$VU1979$ID_study <- "VU1979"


# 1976 ------------------------------------------------------------------------

snes_files$VU1976 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1976.sav", sep = "/"),
           namekey$VU1976, subset = NULL, select = T, rename = T)

snes_files$VU1976$pid_combination <- FALSE
snes_files$VU1976$pid_combination[
  snes_files$VU1976$pid_4_partyB %in% c(23,25,47,58,71,72,73,76,77,79,80)] <- TRUE

attr(snes_files$VU1976$pid_4_partyB, 'labels') <- NULL

# Parties
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 11] <- "V"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 13] <- "KP"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 14] <- "SP"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 22] <- "S"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 33] <- "C"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 44] <- "Fp"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 55] <- "M"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 66] <- "Kd"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 82] <- "SKÅNE"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 84] <- "NYA_PARTIET"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 85] <- "CENT_DEM"

# Blocks
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 23] <- "S+communist"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 25] <- "S+Fp"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 47] <- "Vpk+Fp"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 58] <- "M+S"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 71] <- "right-bloc"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 72] <- "Fp+C"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 73] <- "right-local"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 76] <- "M+S"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 77] <- "M+Fp"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 79] <- "C+Mp"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB == 80] <- "Fp+Mp"
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB %in% c(0,88,96,99)] <- NA

snes_files$VU1976$ID_study <- "VU1976"


# 1973 ------------------------------------------------------------------------

snes_files$VU1973 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1973.sav", sep = "/"),
           namekey$VU1973, subset = NULL, select = T, rename = T)

snes_files$VU1973$pid_combination <- FALSE
snes_files$VU1973$pid_combination[
  snes_files$VU1973$pid_4_partyB %in% c(23,24,25,47,58,71,72,73,76,77,79,80)] <- TRUE

attr(snes_files$VU1973$pid_4_partyB, 'labels') <- NULL

# Parties
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 11] <- "V"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 13] <- "KP"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 14] <- "SP"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 22] <- "S"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 33] <- "C"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 44] <- "Fp"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 55] <- "M"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 66] <- "Kd"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 75] <- "NORD_RIKS"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 82] <- "SKÅNE"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 84] <- "NYA_PARTIET"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 85] <- "CENT_DEM"

# Blocks
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 23] <- "S+communist"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 24] <- "S+C"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 25] <- "S+Fp"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 47] <- "Vpk+Fp"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 58] <- "M+S"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 71] <- "right-bloc"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 72] <- "Fp+C"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 73] <- "right-local"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 76] <- "M+S"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 77] <- "M+Fp"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 79] <- "C+Mp"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB == 80] <- "Fp+Mp"
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB %in% c(0,88,96,99)] <- NA

snes_files$VU1973$ID_study <- "VU1973"

# STEP 3. Join datasets -------------------------------------------------------

#snes_files <- 
#  map(snes_files, map, function(input) as_tibble(sjlabelled::remove_all_labels(input)))

snes_files[2:length(snes_files)] <- 
  modify(snes_files[2:length(snes_files)], function(input) {
  input[["birth_year"]] <- input[["birth_year"]] + 1900
  return(input)
  })

suppressWarnings(
  snes_main <- reduce(snes_files[2:7], bind_rows) 
)

snes_main %>% map(unique)
filter(snes_main, pid_4_partyB == 25)$ID_study

snes_main[snes_main$pid_4_partyB == 25]


# Save data -------------------------------------------------------------------

saveRDS(snes_main, PATHS$outp$snes_main)
saveRDS(test_df_for_cses_pid, PATHS$outp$test_df_for_cses_pid)

# Clean env -------------------------------------------------------------------

rm(namekey); rm(snes_files); rm(test_df_for_cses_pid); rm(tmp_reader); rm(snes_main)

