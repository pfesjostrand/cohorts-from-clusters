# FILE DESCRIPTION -----------------------------------------------------------  
# Inital data preperation for cohorts-from-clusters project. 
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
# This script requires: 'dplyr', 'readr', 'purrr', 'here'           
# This performs additional recoding and combining of SNES data.

# Setup env -------------------------------------------------------------------

stopifnot(
 "dplyr" %in% (.packages()) &&
 "readr" %in% (.packages()) &&
 "purrr" %in% (.packages()) &&
 "here"  %in% (.packages()))

# Config containing mainly paths
# source(here("code", "script_set_paths.R"))

# Load data -------------------------------------------------------------------

snes_main <- readRDS(PATHS$outp$snes_main)

# Create PID index ------------------------------------------------------------

# Default to NA
snes_main$pid_index <- vector("numeric", length = dim(snes_main)[1])
snes_main$pid_index <- NA

# If resp. say they are a party identifier, AND indicate that they are a strong 
# supporter in the follow-up question (pid_3_degree), assign 3.
snes_main$pid_index[which(
  snes_main$pid_1 == 1 & snes_main$pid_3_degree == 1
  )] <- 3

# If resp. say they are a party identifier, AND do not indicate that they are a
# strong supporter in the follow-up question (pid_3_degree), assign 2.
snes_main$pid_index[which(
  snes_main$pid_1 == 1 & !(snes_main$pid_3_degree == 1)
  )] <- 2

# If resp. do NOT say they are a party identifier AND yet name a party that they
# feel closest to, asign 1.
snes_main$pid_index[which(
  snes_main$pid_1 != 1 & !(is.na(snes_main$pid_4_partyB))
  )] <- 1

# If resp. do NOT say they are a party identifier AND do not name a party that 
# they feel they are close to assign 0.
snes_main$pid_index[which(
  snes_main$pid_1 != 1 & is.na(snes_main$pid_4_partyB)
  )] <- 0

# If resp. do NOT respond to inital question of party identification assign NA.
# This is required to avoid mis-coding NA values.
snes_main$pid_index[which(
  is.na(snes_main$pid_1)
  )] <- NA

# Save data -------------------------------------------------------------------

saveRDS(snes_main, PATHS$outp$snes_main)

# Clean env -------------------------------------------------------------------

rm(snes_main)
gc()


# # Test PID index 2 ############################################################
# 
# x <- readRDS(PATHS$outp$x)
# 
# # Create the VU test index ----------------------------------------------------
# 
# # Default to NA
# x$VU_pid_index <- vector("numeric", length = dim(x)[1])
# x$VU_pid_index <- NA
# 
# # If resp. say they are a party identifier, AND indicate that they are a strong 
# # supporter in the follow-up question (pid_3_degree), assign 3.
# x$VU_pid_index[which(
#   x$VU_pid_1 == 1 & x$VU_pid_3_degree == 1
#   )] <- 3
# 
# # If resp. say they are a party identifier, AND do not indicate that they are a
# # strong supporter in the follow-up question (pid_3_degree), assign 2.
# x$VU_pid_index[which(
#   x$VU_pid_1 == 1 &
#   !(x$VU_pid_3_degree == 1)
#   )] <- 2
# 
# # If resp. do NOT say they are a party identifier AND yet name a party that they
# # feel closest to, asign 1.
# x$VU_pid_index[which(
#   x$VU_pid_1 != 1 &
#   !(is.na(x$VU_pid_4_partyB))
#   )] <- 1
# 
# # If resp. do NOT say they are a party identifier AND do not name a party that 
# # they feel they are close to assign 0.
# x$VU_pid_index[which(
#   x$VU_pid_1 != 1 &
#   is.na(x$VU_pid_4_partyB)
#   )] <- 0
# 
# # If resp. do NOT respond to inital question of party identification assign NA.
# # This is required to avoid mis-coding NA values.
# x$VU_pid_index[which(
#   is.na(x$VU_pid_1)
#   )] <- NA
# 
# # Create the CSES test index --------------------------------------------------
# 
# # create cses index, default to NA.
#  x$CSES_pid_index <- vector("numeric", length = dim(x)[1])
#  x$CSES_pid_index <- NA
#  
# # If Resp. indicate that they are a strong suporter of a party assign 3
#  x$CSES_pid_index[which(
#    x$pid_degree %in% c(1)
#  )] <- 3 
# 
# # If Resp. indicate that that are close to a party and in follow up question (pid_degree)
# # on how close indicate "pretty close", or "not particularly close" assign 2.
#  x$CSES_pid_index[which(
#    x$pid_3005_1 == 1 &
#    x$pid_degree %in% c(2,3)
#  )] <- 2
#  
# # If resp do not indiciate that they are cloe to a party but in follow up question
# # (pid_degree) put themselves as "pretty close", or "not particuarly close" assign 1. 
#  x$CSES_pid_index[which(
#    x$pid_3005_1 == 5 &
#    x$pid_degree %in% c(2, 3)
#  )] <- 1
#  
# # If resp indiciate that they are supporter but in follow up question
# # put themselves as "not particularly close", assign 1.
# # x$CSES_pid_index[which(
# #   x$pid_3005_1 == 1 &
# #   x$pid_degree %in% c(3)
# # )] <- 1
#  
# # If resp do not indiciate that they are supporters but in follow up question
# # put themselves as "not particularly close", assign 1.
# # x$CSES_pid_index[which(
# #   x$pid_3005_1 == 5 &
# #   x$pid_degree %in% c(3)
# # )] <- 1
#  
# # If resp do not indicate that they are supporter and give no degree assign 0.
# # Last NOT statement simply a precaution double 5s should only be NA in degree
#   x$CSES_pid_index[which(
#    x$pid_3005_1 == 5 &
#    x$pid_3005_2 == 5 &
#    !(x$pid_degree %in% c(1,2,3))
#  )] <- 0
# ś
# # If the respondent did not answer either of the two first questions assign NA.
#   x$CSES_pid_index[which(
#    is.na(x$pid_3005_1 == 5) &
#    is.na(x$pid_3005_2 == 5)
#   )] <- NA
#     
# # Conduct tests -----------------------------------------------------------------
#   
#   #x %>% select(VU_pid_index, CSES_pid_index) %>% drop_na() -> x_test
#   x_test %>% table()
#   cor(x_test$VU_pid_index, x_test$CSES_pid_index)
#   
#   x %>% select(VU_pid_index, CSES_pid_index) %>% drop_na() -> x_test2
#   x_test2 %>% table()
#   cor(x_test2$VU_pid_index, x_test2$CSES_pid_index)

