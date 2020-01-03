# FILE DESCRIPTION -----------------------------------------------------------  
# Inital data preperation for cohorts-from-clusters project. 
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# This script requires: 'dplyr', 'readr', 'purrr', 'here'           
# Load SNES data perform subselection and inital necessary recoding and join 
# into one dataset. Further recoding is done in a seperate file for clarity.

# TODO: (IMPORTANCE HIGH) Error check these operaitons.
# TODO: (IMPORTANCE LOW) Simplification of these operations. Use a file for names or similar. 
# TODO: (IMPORTANCE MED) Expand the dataset with some class / income measure + rural/urban measure
# TODO: (IMPORTANCE LOW) Expand the dataset produced for more uses and EDA.
# !SEVERAL VARIABLES HAVE BEEN INCLUDED BUT ARE DESELCTED DUE TO NOT YET HOMOGENIZED!

# Setup env -------------------------------------------------------------------

stopifnot(
 "dplyr" %in% (.packages()) &&
 "readr" %in% (.packages()) &&
 "purrr" %in% (.packages()) &&
 "here"  %in% (.packages()))

# Config containing mainly paths
# source(here("code", "script_set_paths.R"))

# LOADING METHODOLOGY/STEPS ---------------------------------------------------
#
# STEP/EXPLANTION
#
# 1. I define tmp_reader functions and a namekey containing the SPSS label
# attribute of each variable and a correspodning name I want it changed into.
#
# The function tmp_reader does the following: 
#    i.   Load the data at a provided path
#    ii.  Renames the variables into the label attribute of the variable. 
#    iii. Selects those variables where the name (label) match the namekey
#    iv.  Renames the variable after privded namekey resulting in unifified names
#         for all yeas and variables that can be joined into one dataset.
# 
# 2. I go through each year and make new unified coding for the parties. I've
#    checked year by year that parties are coded the same etc. Before 1994 it
#    was recorded when voters picked blocks as the best party (combinations of
#    parties). I create a new variable indiciating if the picked a block.
#
# 3. I join the sets from different years together into one unified data frame.
#    I recode NA values. 

# STEP 1A. Define namekeys ----------------------------------------------------
# Namekey contains old name of variable (its label attr.) as a name attribute &
# new name as value. It is a mess since SNES keeps changing how they name vars.

namekey <- list()

# 2010 available dataset do notw contain precise birth year. Will not be used. 
namekey$VU2010 <- c(
  "ID-nummer" = "ID", "SCB: Kön" = "gender",
  "F.29A Partianahängare"     = "pid_1", 
#  "F.29B Bästa parti"         = "pid_2_partyA", 
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
#  "F.43B Partipreferens"      = "pid_2_partyA", 
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
# "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Partipreferens och valdeltagande - Närmaste parti" = "pid_4_partyB",
  "Inställning till Centerpartiet"            = "eval_c", 
  "Inställning till Moderata samlingspartiet" = "eval_m",
  "Inställning till Vänsterpartiet"           = "eval_v", 
  "Inställning till Socialdemokraterna"       = "eval_s",
  "Vänster-högerskalan: UP:s egen placering"  = "left_right_placement",
  "Utbildning"  = "education",
  "Bostadsort"  = "urban_rural",
  "Yrkessektor" = "job_sector",
  "Yrkesgruppstillhörighet" = "job_group",
  "Civilstånd" = "marriage"
  )

namekey$VU1998 <- c(
  "ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
#  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Närmaste parti"      = "pid_4_partyB",
  "Inställning till Centerpartiet"            = "eval_c", 
  "Inställning till Moderata samlingspartiet" = "eval_m",
  "Inställning till Vänsterpartiet"           = "eval_v", 
  "Inställning till Socialdemokraterna"       = "eval_s",
  "Vänster-högerskalan: UP:s egen placering"  = "left_right_placement",
  "Skolutbildning"  = "education",
  "Bostadsort"  = "urban_rural",
  "Yrkessektor" = "job_sector",
  "Yrkesgruppstillhörighet" = "job_group",
  "Civilstånd" = "marriage"
  )

namekey$VU1994 <- namekey$VU1998
namekey$VU1991 <- namekey$VU1998

namekey$VU1988 <- c(
  "UP:s ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
#  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Närmaste parti"      = "pid_4_partyB",
  "Centerpartiet"                = "eval_c", 
  "Moderata samlingspartiet"     = "eval_m", 
  "Socialdemokraterna"           = "eval_s", 
  "Vänsterpartiet kommunisterna" = "eval_v",
  "UP:s egen placering"          = "left_right_placement",
  "Allmän grundutbildning"  = "education",
  "Bostadsort"  = "urban_rural",
  "Yrkets sektor enligt den nya indelningen" = "job_sector",
  "Yrkesgrupper enligt den nya indelningen" = "job_group",
  "Civilstånd" = "marriage"
  )

namekey$VU1985 <- namekey$VU1988
namekey$VU1982 <- c(
  "UP:s ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
#  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Närmaste parti"      = "pid_4_partyB",
  "Centerpartiet"                = "eval_c", 
  "Moderata samlingspartiet"     = "eval_m", 
  "Socialdemokraterna"           = "eval_s", 
  "Vänsterpartiet kommunisterna" = "eval_v",
  "UP:s egen placering"          = "left_right_placement",
  "Allmän grundutbildning"       = "education",
  "Hemortstyp"                   = "urban_rural",
  "Yrkets sektor enligt den nya indelningen" = "job_sector",
  "Yrkesgrupper enligt den nya indelningen"  = "job_group",
  "Civilstånd" = "marriage"
  )
namekey$VU1979 <- namekey$VU1982

namekey$VU1976 <- c(
  "UP:s ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
#  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Närmaste parti"      = "pid_4_partyB",
  "Vänster-högerskalan: UP:s placering" = "left_right_placement",
  "Allmän grundutbildning"       = "education",
  "Hemortstyp"                   = "urban_rural",
  "Yrkets sektor enligt den nya indelningen" = "job_sector",
  "Yrkesgrupper enligt den nya indelningen"  = "job_group",
  "Civilstånd" = "marriage"
  )

namekey$VU1973 <- c(
  "UP:s ID-nummer" = "ID", "Födelseår" = "birth_year", "Kön" = "gender",
  "Partianhängare"      = "pid_1", 
#  "Partipreferens"      = "pid_2_partyA", 
  "Partiidentifikation" = "pid_3_degree",
  "Bästa parti"         = "pid_4_partyB",
  "Vänster-högerskalan: UP:s placering" = "left_right_placement",
  "Allmän grundutbildning"       = "education",
  "Civilstånd" = "marriage"
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

# STEP 2. Load, subselect, rename necessary, inital recoding -------------------

# Output is saved into the following list
snes_files <- list()

# 2010 ------------------------------------------------------------------------
#          !EXCLUDED BECAUSE BIRTH YEAR NOT INCLUDED CSES DATA USED INSTEAD!
#
# snes_files$VU2010 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU2010.sav", sep = "/"),
#            namekey$VU2010, subset = NULL, select = T, rename = T)
#
# snes_files$VU2006$pid_4_partyB[
#   snes_files$VU2006$pid_4_partyB %in% c(0,55,88,96,99)] <- NA
# 
# snes_files$VU2006$pid_4_partyB[
#   snes_files$VU2006$pid_4_partyB %in% c(8886,8888,9992,9994,9998)] <- NA
#

# 2006 ------------------------------------------------------------------------
# EXCLUDED BECAUSE BIRTH YEAR NOT INCLUDED CSES DATA USED INSTEAD
# VU2006 only used to get test data for checking correlation of pid_degree
# variable coded from CSES data with pid_degree coded from VU data.

# FIXME Running tmp_reader on 2006 and 2010 only possible if input to name(x) is a
# list for unknown reason. This does not work with some older datasets. I have not
# prioritized solving this. Using work around for now.
## snes_files$VU2006 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU2006.sav", sep = "/"),
##            namekey$VU2006, subset = NULL, select = F, rename = F)
  
pidTestDf <- haven::read_sav(
  paste(PATHS$data$snes_data_dir, "VU2006.sav", sep = "/")) %>%
  select("ID" = ID, "VU_pid_1" = V580, "VU_pid_2_partyA_v" = V581, 
         "VU_pid_3_degree" = V582, "VU_pid_4_partyB" = V583, "pid_3005_1" = V728, 
         "pid_3005_2" = V729, "pid_party" = V730, "pid_degree" = V731)

pidTestDf$VU_pid_4_partyB[
   pidTestDf$VU_pid_4_partyB %in% c(0,55,88,95,96,98,99,998,998)] <- NA

# 2002 ------------------------------------------------------------------------
snes_files$VU2002 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU2002.sav", sep = "/"),
           namekey$VU2002, subset = NULL, select = T, rename = T)

attr(snes_files$VU2002$pid_4_partyB, 'labels') <- NULL
snes_files$VU2002$pid_4_partyB[snes_files$VU2002$pid_4_partyB %in% c(0,55,88,96,99)] <- NA
snes_files$VU2002$ID_study <- "VU2002"
snes_files$VU2002$survey_year <- 2002

# 1998 ------------------------------------------------------------------------
snes_files$VU1998 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1998.sav", sep = "/"),
           namekey$VU1998, subset = NULL, select = T, rename = T)

snes_files$VU1998$pid_4_partyB[snes_files$VU1998$pid_4_partyB %in% c(0,55,88,96,99)] <- NA
snes_files$VU1998$ID_study <- "VU1998"
snes_files$VU1998$survey_year <- 1998

# 1994 ------------------------------------------------------------------------
snes_files$VU1994 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1994.sav", sep = "/"),
           namekey$VU1994, subset = NULL, select = T, rename = T)

snes_files$VU1994$pid_4_partyB[snes_files$VU1994$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1994$ID_study <- "VU1994"
snes_files$VU1994$survey_year <- 1994

# 1991 ------------------------------------------------------------------------
snes_files$VU1991 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1991.sav", sep = "/"),
           namekey$VU1991, subset = NULL, select = T, rename = T)

snes_files$VU1991$pid_combination <- FALSE
snes_files$VU1991$pid_combination[snes_files$VU1991$pid_4_partyB %in% c(71)] <- TRUE
snes_files$VU1991$pid_4_partyB[snes_files$VU1991$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1991$ID_study <- "VU1991"
snes_files$VU1991$survey_year <- 1991

# 1988 ------------------------------------------------------------------------
snes_files$VU1988 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1988.sav", sep = "/"),
           namekey$VU1988, subset = NULL, select = T, rename = T)

snes_files$VU1988$pid_combination <- FALSE
snes_files$VU1988$pid_combination[
snes_files$VU1988$pid_4_partyB %in% c(23,24,47,71)] <- TRUE
snes_files$VU1988$pid_4_partyB[snes_files$VU1988$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1988$ID_study <- "VU1988"
snes_files$VU1988$survey_year <- 1988

# 1985 ------------------------------------------------------------------------
snes_files$VU1985 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1985.sav", sep = "/"),
           namekey$VU1985, subset = NULL, select = T, rename = T)

snes_files$VU1985$pid_combination <- FALSE
snes_files$VU1985$pid_combination[
snes_files$VU1985$pid_4_partyB %in% c(23,25,47,58,71,72)] <- TRUE
snes_files$VU1985$pid_4_partyB[snes_files$VU1985$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1985$ID_study <- "VU1985"
snes_files$VU1985$survey_year <- 1985

# 1982 ------------------------------------------------------------------------
snes_files$VU1982 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1982.sav", sep = "/"),
           namekey$VU1982, subset = NULL, select = T, rename = T)

snes_files$VU1982$pid_combination <- FALSE
snes_files$VU1982$pid_combination[
snes_files$VU1982$pid_4_partyB %in% c(23,25,47,58,71,72,77,79,80)] <- TRUE
snes_files$VU1982$pid_4_partyB[snes_files$VU1982$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1982$ID_study <- "VU1982"
snes_files$VU1982$survey_year <- 1982


# 1979 ------------------------------------------------------------------------
snes_files$VU1979 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1979.sav", sep = "/"),
           namekey$VU1979, subset = NULL, select = T, rename = T)

snes_files$VU1979$pid_combination <- FALSE
snes_files$VU1979$pid_combination[
snes_files$VU1979$pid_4_partyB %in% c(23,25,47,58,71,72,73,76,77,79,80)] <- TRUE
snes_files$VU1979$pid_4_partyB[snes_files$VU1979$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1979$ID_study <- "VU1979"
snes_files$VU1979$survey_year <- 1979

# 1976 ------------------------------------------------------------------------
snes_files$VU1976 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1976.sav", sep = "/"),
           namekey$VU1976, subset = NULL, select = T, rename = T)

snes_files$VU1976$pid_combination <- FALSE
snes_files$VU1976$pid_combination[
  snes_files$VU1976$pid_4_partyB %in% c(23,25,47,58,71,72,73,76,77,79,80)] <- TRUE
attr(snes_files$VU1976$pid_4_partyB, 'labels') <- NULL
snes_files$VU1976$pid_4_partyB[snes_files$VU1976$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1976$ID_study <- "VU1976"
snes_files$VU1976$survey_year <- 1976

# 1973 ------------------------------------------------------------------------
snes_files$VU1973 <- tmp_reader(paste(PATHS$data$snes_data_dir, "VU1973.sav", sep = "/"),
           namekey$VU1973, subset = NULL, select = T, rename = T)

snes_files$VU1973$pid_combination <- FALSE
snes_files$VU1973$pid_combination[
  snes_files$VU1973$pid_4_partyB %in% c(23,24,25,47,58,71,72,73,76,77,79,80)] <- TRUE
snes_files$VU1973$pid_4_partyB[snes_files$VU1973$pid_4_partyB %in% c(0,88,96,99)] <- NA
snes_files$VU1973$ID_study <- "VU1973"
snes_files$VU1973$survey_year <- 1973

# STEP 3. Join datasets -------------------------------------------------------

# Join data set this will also remove all labels
# TODO Create or retrive labels.
suppressWarnings(
  snes_main <- reduce(snes_files, bind_rows) 
)

# Further recoding ------------------------------------------------------------

# One row in the entire dataset is missing birth year this row is dropped
# Some years give birth years as two digits this is changed for consistency
snes_main <- snes_main[(is.na(snes_main$birth_year)==FALSE),] 

snes_main$birth_year[snes_main$survey_year < 1995 & snes_main$birth_year %in% c(80:99)] <- 
  snes_main$birth_year[snes_main$survey_year < 1995 & snes_main$birth_year %in% c(80:99)] + 1800

snes_main$birth_year[!(snes_main$birth_year %in% c(1800:1899)) & snes_main$birth_year < 1900] <-
  snes_main$birth_year[!(snes_main$birth_year %in% c(1800:1899)) & snes_main$birth_year < 1900] + 1900

# Answer 88 indicate 'did not answer' coded to NA
select(snes_main, eval_c, eval_m, eval_v, eval_s, left_right_placement) %>% 
  modify(
    function(input) na_if(input, 88) %>% na_if(888)
         ) -> 
  snes_main[c("eval_c", "eval_m", "eval_v", "eval_s","left_right_placement")]

#TODO Homogenize education variable, curretly deselected do to different codings
snes_main <- select(snes_main, -education)

#TODO Homogenize urban_rural variable and ensure fit with CSES data, currently 
# deselcted due to different codings (although quite similar so probably should
# not be impossible)
snes_main <- select(snes_main, -urban_rural)

#TODO Marriage ensure homogenized data internally and with CSES
snes_main <- select(snes_main, -marriage)

#TODO I have deselected the occupation/class variables as not currently used, same 
# as the other variables above here quite a lot of work required if to be consistent 
# with some class schema
snes_main <- select(snes_main, -job_sector, -job_group)

# TODO 1973 and 1976 use 0 to 100 indiciations for left_right placement currently
# deselcted. Can opt to divide by 10 according to below and then round it. But
# yeah that is a bit problematic. Marked NA for 1973 and 1976.
snes_main$left_right_placement[which(snes_main$survey_year %in% c(1973, 1976))] <- NA

# Deactivated code for recoding by dividing by ten-
#filter(snes_main, survey_year %in% c(1973, 1976)) %>% 
#  pull(left_right_placement)/10 ->
#  snes_main$left_right_placement[which(snes_main$survey_year %in% c(1973, 1976))]

# Answers with combination of parties which exists in older data sets are set as 
# NA for consistency with later data. Applies only to 36 elements
snes_main$pid_4_partyB[which(snes_main$pid_combination == TRUE)] <- NA

# Save data -------------------------------------------------------------------

saveRDS(snes_main, PATHS$outp$snes_main)
saveRDS(pidTestDf, PATHS$outp$pidTestDf)

#snes_main <- readRDS(PATHS$outp$snes_main)

# Clean env -------------------------------------------------------------------

rm(namekey); rm(snes_files); rm(pidTestDf); rm(tmp_reader); rm(snes_main)
gc()

# Sraps -----------------------------------------------------------------------

# SCRAPS For printing labels
#pre_print_labels <- function(data_set, nom, filepath) {
#  rowstart <- paste(paste("VU",nom, sep=""), sep = " - ", seq_along(data_set))
#  out <- paste(rowstart, names(data_set), sep = " - ")
#  cat(out, sep = "\n", file = filepath)
#}
#print_paths <- paste(paste(here("data", "fixed_snes"), names(snes_files), sep = "/"), "_labels.txt", sep = "") 
#pre_print_labels(snes_files$VU2002, "2002", here("data", "fixed_snes", "labels_VU2002.txt"))
# map2(snes_files,names(snes_files)
# out <- unlist(out)
# out <- paste(c(1:4125), out, sep = " -- ")
# out %>% cat(sep = "\n", file = here("data", "fixed_snes", "snes_label.txt"))
# SCRAPS rmeoving labels
#snes_files <- 
#  map(snes_files, map, function(input) as_tibble(sjlabelled::remove_all_labels(input)))

