# FILE DESCRIPTION ------------------------------------------------------------                                                          
# Recodin of cses variables
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# This script requires: 'dplyr', 'readr', 'purrr', 'here'             

# Recoding documentation ------------------------------------------------------
# 
# Notes on what/why recodings are made follow
#
#   1.1 Reasons for recoding
# 
# Recoding are done for two purposes. 1. To change categorical scales into ordinal scales when possible.
# 2. To change numeric missing value codes into NA. Recoding of missing values, 2, are done according to
# a prepepares spreadsheet for changing scales, 1, this is more nuisance than worth, therefore these are
# instead hardcoded below. If names of variables are changed names here should also be changed. 
# 
# 
#   1.2 How non-standard / missing values are recoded in cses_main data
# 
# CSES uses various numeric codes, 99, 9999 etc. to cocde for NON STANDARD responses such as resp. give
# no response, are not able to anwswer question, or record is missing. In some cases such numeric codes
# can be given an ordinal standard code based on the repsondents answer in other questions. In other 
# cases they are here changed to Rs standard code for missing value 'NA'. 
# 
# 
#   2.1 About party identication in cses_main                                                          
# 
# In CSES variable IMD3005_1 and IMD3005_2 resps. are asked if they are close to any party, if they 
# answer YES the resp. is asked to specify what party in IMD3005_3, and to rate how close in IMD3005_4,
# here named pid. Many resps. respond that they do not feel close to any party and these will
# be marked as a missing value i IMD3005_4 / pid. Recoding of this is done to avoid the missing
# values in IMD3005_4 / pid, as follows:
# 
#  2.2 How pid is recoded in cses_main 
# 
# If the resp. specify in BOTH IMD3005_1 and IMD3005_2 that they are not close to any  they are 
# marked as 0 in pid, as then they little ident. If the resp. specify that they are not close
# to any party in EITHER IMD3005_1 OR IMD3005_2 AND they either can not answer or is missing answer in
# IMD3005_3 then pid is recoded into 0. This makes the pid var ordinal. If pid has a standard 
# answer no recoding is done. I.e. no correction is made to achive consistency.
# 
# 
#  3.1 Explanation of non-unique and unique party IDs in cses_main
# 
# The data contains survey questions where the respondent is asked to rate parties on a scale, -5 to 
# +5. Answers are recorded in IMD3008_* variables; IMD3008_A contain ratings for the largest party, 
# IMD3008_B for the second largest and so forth. Treatment of this 
# 
# To use these ratings thios coding needs to be mapped to unique party ID. the IMD5000_* variables 
# supply party IDs for A, B and so forth. Thes IDs are numeric therefore in a mapping of a subset of 
# these numeric codes to more easily identifiable short names is done. This is pased on a pre-prepared
# preadsheet and only done for a subset of politys. 
# 
# Mapping is then done between the IMD5000_. and IMD_3008_. variables, by nesting the data on each 
# election. Then renaming each IMD3008_ variable by the unique ID or short name containd in IMD5000_.
# For each  election there  should be a unique value for IMD5000_A.

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
metadata  <- readRDS(PATHS$outp$cses_main_metadata)

# Manual recoding -------------------------------------------------------------

# Recode dem_sat, IMD3010: In Var IMD3010, answer 6 denotes 'neither satisfied
# nor dissatisfied' while 3 does not exist. The scale is 1 to 5. By recoding 6
# into 3 an ordinal scale is creted.
cses_main$dem_sat[which(cses_main$dem_sat == 6)] <- 3

# Recode married: Observe that 3 means divorced OR seperated (married but sep. /
# not living with legal spouse). Five is used differenly for different countries
# here treated as NA.
cses_main$married[which(cses_main$married == 1)] <- "married"
cses_main$married[which(cses_main$married == 2)] <- "widowed"
cses_main$married[which(cses_main$married == 3)] <- "seperated"
cses_main$married[which(cses_main$married == 4)] <- "never_married"
cses_main$married[which(cses_main$married == 5)] <- NA

# Recode religion, IMD2005: 96 is specified as other. 
cses_main$religion[which(cses_main$religion == 1)]  <- "Christian_Catholic"
cses_main$religion[which(cses_main$religion == 2)]  <- "Christian_Protestant"
cses_main$religion[which(cses_main$religion == 3)]  <- "Christian_Orthodox"
cses_main$religion[which(cses_main$religion == 4)]  <- "Christian_other"
cses_main$religion[which(cses_main$religion == 5)]  <- "Jewish"
cses_main$religion[which(cses_main$religion == 6)]  <- "Islam_Sunni"
cses_main$religion[which(cses_main$religion == 7)]  <- "Islam_other"
cses_main$religion[which(cses_main$religion == 8)]  <- "Buddhism"
cses_main$religion[which(cses_main$religion == 9)]  <- "Hinduism"
cses_main$religion[which(cses_main$religion == 10)] <- "Indigenous"
cses_main$religion[which(cses_main$religion == 11)] <- "Ethnoreligions"
cses_main$religion[which(cses_main$religion == 12)] <- "Non_Belivers"
cses_main$religion[which(cses_main$religion == 13)] <- "Agnostics"
cses_main$religion[which(cses_main$religion == 96)] <- NA

# Create pid_index_1 ----------------------------------------------------------

# Recode pid, IMD3005_4 into a new variable pid_index_1

# Party identification is recoded introducing 0 to indicate no pid this is based 
# on responses in other questions (IMD3005_1,_2, _3). See note 2.X above.  
# TODO CHECK THAT THIS ADDS UP pid_index_2 is probably better checked. 

# create cses index, default to pid_degree.
 cses_main$pid_index_1 <- vector("numeric", length = dim(cses_main)[1])
 cses_main$pid_index_1 <- cses_main$pid_degree
 
cses_main$pid_index_1[
  cses_main$pid_3005_1 == 5 &   
  cses_main$pid_3005_2 == 5 &   
  cses_main$pid_degree %in% c(7,8,9)
  ] <- 0

cses_main$pid_index_1[
  (cses_main$pid_3005_1 == 5 | cses_main$pid_3005_2 == 5) &
   cses_main$pid_party %in% c(9999998, 9999999) &
   cses_main$pid_degree %in% c(7,8,9)
  ] <- 0

# Create pid_index_2 ----------------------------------------------------------

# pid_index_2 was created to accord as well ass posisble with the SNES index for
# sweden based on SNES data these correlate to about 0.68 on the 2006 data. 

# create cses index, default to NA.
 cses_main$pid_index_2 <- vector("numeric", length = dim(cses_main)[1])
 cses_main$pid_index_2 <- NA

# If Resp. indicate that they are a strong suporter of a party assign 3.
 cses_main$pid_index_2[which(
   cses_main$pid_degree %in% c(1)
 )] <- 3

# If Resp. indicate that that are close to a party and in follow up question (pid_degree)
# on how close indicate "pretty close", or "not particularly close" assign 2.
 cses_main$pid_index_2[which(
   cses_main$pid_3005_1 == 1 &
   cses_main$pid_degree %in% c(2,3)
 )] <- 2

# If resp do not indiciate that they are cloe to a party but in follow up question
# (pid_degree) put themselves as "pretty close", or "not particuarly close" assign 1.
 cses_main$pid_index_2[which(
   cses_main$pid_3005_1 == 5 &
   cses_main$pid_degree %in% c(2, 3)
 )] <- 1

# If resp do not indicate that they are supporter and give no degree assign 0.
# Last NOT statement simply a precaution double 5s should only be NA in degree
  cses_main$pid_index_2[which(
   cses_main$pid_3005_1 == 5 &
   cses_main$pid_3005_2 == 5 &
   !(cses_main$pid_degree %in% c(1,2,3))
 )] <- 0

# If the respondent did not answer either of the two first questions assign NA.
# This is done to avoid mis-coding NA values. Respondents which did not answer
# the first questions are therefore not used to form the index.
  cses_main$pid_index_2[which(
   is.na(cses_main$pid_3005_1) &
   is.na(cses_main$pid_3005_2)
  )] <- NA

# Recode NA values by sheet ---------------------------------------------------

## All new names in metadata should exist in cses_main    
## stopifnot(all(names(cses_main) %in% metadata$names_new))                          

## All cses names in metadata_old must match a name in cses_main_raw
## stopifnot(all(metadata$names_old %in% names(cses_raw)))         

# There should not be any duplicate names in any of the data sets          
stopifnot(length(unique(metadata$names_old))   == length(metadata$names_old))
stopifnot(length(unique(metadata$names_new))   == length(metadata$names_new))
#stopifnot(length(unique(names(cses_main_raw))) == length(names(cses_main_raw)))
stopifnot(length(unique(names(cses_main)))     == length(names(cses_main)))

# Recodes according to the metadata sheet If th value is specified in as NA 
# by the sheet it will be recoeded into NA.
cses_main <- xpfe::recode_na_by_sheet(cses_main, metadata)

# Save data -------------------------------------------------------------------

# DEPRECEATED KEEP. Remove IMD3005_1 through 3 as they are no longer needed. 
# cses_main <- select(cses_main, -pid_any, -pid_one, -pid_party)

# Save
saveRDS(cses_main, PATHS$outp$cses_main)

# Clean env -------------------------------------------------------------------

rm(cses_main); rm(metadata)
