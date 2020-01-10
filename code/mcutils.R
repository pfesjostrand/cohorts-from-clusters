# File comments --------------------------------------------------------------
# Contains utility functions aid analysis and declutter working code. 

mcutils <- list()

# Data utils -----------------------------------------------------------------

# Simplifies loading from by_polity data set
mcutils$getData <- 
  function(PATH, b=T, POL="Sweden")  {if(b==T) readRDS(PATH)[[POL]][[1]] 
  else readRDS(PATH)}


# little function used repeatedly to start data pipes to plot
mcutils$dpipe <- function(path, selection, range1, range2,
                     range1.var = "birth_year", range2.var = "survey_year") {
  readRDS(path) %>% 
    select(selection) %>% 
    filter(!!sym(range1.var) %in% range1) %>%
    filter(!!sym(range2.var) %in% range2) %>%
    return()
}

# Function for producing binned data for aggregate and average cohort track plots. 
mcutils$get_average <- 
  function(data, variable, time = "birth_year", group1 = NULL, group2 = NULL) {
  data <- data %>% rename("VARIABLE"  = !!variable)
  data <- data %>%  rename("TIME"  = !!time)
  
  if(!(is.null(group1))) 
    { stopifnot(group1 %in% names(data))
    data <- data %>% rename("grp1" = !!group1)
    data <- data %>% group_by(grp1) %>% mutate(
        "binMin" = min(TIME, na.rm = T), 
        "binMax" = max(TIME, na.rm = T),
        "binMid" = median(TIME, na.rm = T)) %>%
      group_by(grp1, binMin, binMax, binMid) }

  if(!(is.null(group2))) 
    { stopifnot(group2 %in% names(data))
    data <- data %>% rename("grp2" = !!group2)
    data <- data %>% group_by(grp2, grp1, binMin, binMax, binMid) 
  }
  
  data <- data %>%  summarise(
    "avg" = mean(VARIABLE, na.rm = T), "n" = n(),
    "sd" = sd(VARIABLE, na.rm = T), "se" = sd/sqrt(n),
    
    "err95Min" = avg - qnorm(1-0.05/2) * se, 
    "err95Max" = avg + qnorm(1-0.05/2) * se,
    
    "err90Min" = avg - qnorm(1-0.10/2) * se, 
    "err90Max" = avg + qnorm(1-0.10/2) * se,
    
    "err85Min" = avg - qnorm(1-0.15/2) * se, 
    "err85Max" = avg + qnorm(1-0.15/2) * se,
    
    "err75Min" = avg - qnorm(1-0.25/2) * se, 
    "err75Max" = avg + qnorm(1-0.25/2) * se,
    
    "err50Min" = avg - qnorm(1-0.5/2)  * se, 
    "err50Max" = avg + qnorm(1-0.5/2)  * se
    ) %>% ungroup()
    
  if(!(is.null(group2))) {
    data <- rename(data, !!group2 := "grp2" ) 
    }
  if(!(is.null(group1))) { 
    data <- rename(data, !!group1 := "grp1" )
    data <- mutate(data, "label" = paste(
      as.character(binMin), as.character(binMax), sep = " to ")
      ) %>% 
  select(!!group1, !!group2, label, avg, everything())
    }
  return(data)
}

# Print information on data set for reference. 
mcutils$print_data_info <- 
  function(cses_path = PATHS$outp$se_cses_small_analysis, 
           snes_path = PATHS$outp$se_snes_small_analysis) {
  
  cses <- readRDS(cses_path)
  snes <- readRDS(snes_path)
  
  cat("\n DATA SETS GENERAL \n",
      "\n SNES dimensions \n",       dim(snes), "\n",
      "\n CSES dimensions \n",       dim(cses), "\n",
      "\n SNES Survey years \n",     range(pull(snes, survey_year)), "\n",
      "\n CSES Survey years \n",     range(pull(cses, survey_year)), "\n",
      "\n",
      "\n BIRTH RANGES \n",
      "\n SNES birth year range \n", range(pull(snes, birth_year)), "\n",
      "\n CSES birth year range \n", range(pull(cses, birth_year)), "\n",
      "\n",
      "\n EXPECTED LIMITS \n",
      "\n CSES Lower limit \n",
      cses %$% c(min(survey_year)-35, min(survey_year)-30, min(survey_year)-22, 
                 min(survey_year)-18), "\n",
      "\n CSES Upper limit \n",
      cses %$% c(max(survey_year)-(18+4), max(survey_year)-18), "\n",
      "\n SNES Lower limit \n",
      snes %$% c(min(survey_year)-35, min(survey_year)-30, min(survey_year)-22, 
                 min(survey_year)-18), "\n",
      "\n SNES Upper limit \n",
      snes %$% c(max(survey_year)-(18+4), max(survey_year)-18), "\n",
      "\n"
      )
}

# Table functions -------------------------------------------------------------

mcutils$tab_standard <- function(caption, striped = TRUE, color = "black", ...) {
  
  x <- tibble(...)
  
  if (striped == TRUE) {
    striped <- "striped"
  }
  else {
    striped <- NULL
  }

  XROWS <- c(1:nrow(x))

  tibble(...) %>%
    kable(caption = caption, booktabs = T) %>%
    kable_styling(
      full_width = T,
      latex_options = c(striped, "hold_position")
    ) %>%
    row_spec(XROWS, color = color) %>%
    return()
}


# Calculation/Analysis functions ----------------------------------------------

# Ordinary confidence interval for mean
mcutils$conf_intervall <- 
  function(x, sd, n, alpha = 0.05, dist = "normal", type = "mean") {
  if(type == "mean") {
  if(dist == "normal") {
    error <- qnorm(1-alpha/2)*sd/sqrt(n)
  }
  if(dist %in% c("t", "students")) {
    error <- qt(1-alpha/2,n-1)*sd/sqrt(n)
  }
  c("min" = x - error, "max" = x + error)
  }
}
  
# GGplot mod functions --------------------------------------------------------

mcutils$gg_xscale <- function(by, minval, maxval = NULL, type = "continous") {
  if(is.null(maxval)) {maxval <- minval}
  if(type == 1 | type == "continous") {
    x <- seq(min(minval), max(maxval), by = by)
  }
  return(x)
}

#gg_modify$labs <- function(title = NULL, cap = NULL) return(labs(title, cap))

