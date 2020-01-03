# FILE DESCRIPTION                                                          
# ***************************************************************************** 
# Further data preperation for cohorts-from-clusters project. This script take
# data previously prepared by /code/script_load_imd.R, cleans data by imputing 
# missing values, dropping remaining unreliable data, and transforming into DFs
# thar are ready for analysis, finished procucts are stored in /output.
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

# Create single polity data sets ----------------------------------------------

se_cses <- by_polity %>%
  ungroup() %>%
  filter(polity == "Sweden") %>%
  unnest(cols = c("data")) %>% 
  select(-polity, -region, -survey_year, -id_study, -polity_range) %>%
  select(id, year, age, birth_year, everything())

de_cses <- by_polity %>%
  ungroup() %>%
  filter(polity == "Germany") %>%
  unnest(cols = c("data")) %>% 
  select(-polity, -region, -survey_year, -id_study, -polity_range) %>%
  select(id, year, age, birth_year, everything())

# Impute data -----------------------------------------------------------------

# Amelia II is used for imputation. Fun amelia_impute is a wrapper that removes
# cols/rows exceding a col or row threshold and stops execution if any two vars
# have a higher correlation that cor_threshold. Uses xpfe::threshold_remove_na.
amelia_impute <- function(df, col_threshold = 0.95, row_threshold = 0.95, cor_threshold = 0.9, rmcols = NULL, ...) {
  
  # Remove any cols specified by rmcols (none) remove cols/rows above threshold
  if (!(is.null(rmcols))) { DF <- select(df, -(rmcols)) } else { DF <- df }
  DF <- xpfe::threshold_remove_na(DF, percentage = T, col_threshold = col_threshold, row_threshold = row_threshold)

  # Warnings are printed when cols or rows removed
  if (dim(DF)[[1]] != dim(df)[[1]]) {
    warning("Some rows exceded the row threshold and have been removed")
  }
  if (dim(DF)[[2]] != dim(df)[[2]]) {
    warning("Some cols exceded the col threshold and have been removed")
  }

  # Stop if collinear, only catches numerics, collinear nominals may crash AML2
  CORMATRIX <- dplyr::select_if(DF, is.numeric) %>% tidyr::drop_na() %>% cor()
  CORDIST   <- as.dist(CORMATRIX)
  if (any((CORDIST > cor_threshold) | (CORDIST < -cor_threshold))) {
    print(CORMATRIX); stop("Correlation exceds the threshold.") 
    }
  
  # Call Aemlia II. AML2 does not work with tibbles so use classic data frame.
  as.data.frame(DF) %>% Amelia::amelia(incheck = TRUE, ...) %>% return()
}

# Imputation is done after dropping columns with above 95% NAs on enitre dataset
# then imputing seperatly for each election (the year variable) to increase acc.

# Note that running this an election at a time really complicates everything 
# since the data has to be sperated by year and then extracted and put together.

# As many variables as possible are used but some have to be removed, for both
# Germany and Sweden survey_year and year is the same therefore survey year is 
# dropped. The age column is dropped since the input data to Ameilia is only one 
# election at a time age and bith year will then be linearly dependent.

# Religion variable is not recorded for Sweden (is NA), hence removed. Vote_lh
# (party vote choice) is removed for Germany beacause Amelia II was not able to
# handle it (crashed the entire R session when run). Probably due to many more 
# observations in the German dataset and more categories in the German vote_lh.

se_am2 <- se_cses %>%
  filter(year != 2002) %>%
  group_by(year) %>%
  select(-age) %>% 
  nest() %>%
  mutate(
    amelia = map(data, amelia_impute,
      m = 2, idvars = "id", noms = c("gender", "vote_lh", "married"),
      col_threshold = 0.95, row_threshold = NULL, cor_threshold = 0.95
    )
  ) 

#de_am2 <- de_cses %>%
#  group_by(year) %>%
#  select(-age) %>%
#  nest() %>%
#  mutate(
#    amelia = map(data, amelia_impute,
#      m = 1, idvars = "id", noms = c("gender","vote_lh", "religion", "married"),
#      col_threshold = 0.95, row_threshold = NULL, cor_threshold = 0.95
#    )
#  )


# Extract data ----------------------------------------------------------------

# To use this data we extract and combine it. 
extract_imputations <- function(data_nest, model_col = "amelia", group_col = "year") {
  stopifnot(is_list(data_nest))
  # Create list of length m, name items m1, m2, ... mn
  m <- data_nest[[model_col]][[1]][["m"]]
  mList <- vector(mode = "list", length = m)
  mNoms <- vector("character", length = length(mList))
  for (i in seq_along(mNoms)) mNoms[[i]] <- paste("m", i, sep = "")
  names(mList) <- mNoms
  
  # Extract imputed data from AML2 output in nested DF
  for (k in seq_along(mList)) {
    mList[[k]] <-
      map2(
        data_nest[[model_col]], data_nest[[group_col]],
        function(DF, GROUP) mutate(as_tibble(DF$imputations[[k]]), "year" = GROUP)
      )
    mList[[k]]<- purrr::reduce(mList[[k]], dplyr::bind_rows)
  }
  
  return(mList)
}

se_am2 <- extract_imputations(se_am2)
#de_am2 <- extract_imputations(de_am2)

# Save data -------------------------------------------------------------------

saveRDS(se_cses, PATHS$outp$se_cses)
saveRDS(se_am2, PATHS$outp$se_am2)

saveRDS(de_cses, PATHS$outp$de_cses)
#saveRDS(de_am2, PATHS$outp$de_am2)

# Clean env --------------------------------------------------------------------

rm(amelia_impute); rm(extract_imputations); rm(se_cses); rm(de_cses); rm(se_am2)
rm(de_am2); rm(by_polity)
