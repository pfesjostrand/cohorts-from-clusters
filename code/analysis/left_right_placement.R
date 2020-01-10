# FILE DESCRIPTION ----------------------------------------------------------------------
# Prepares a number of standard figures for the project.
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)           
#
# Modelling / analysis code is placed alongside plots for clarity. The analysis are
# seperated into files by variable-group analyzed. Here left_right is analyzed.
#
# Plotes should reference one of the data files in "/output", not use intermediary
# data stored in memory or on disk. It should be clear from the code in connection 
# to the plot/fig what data treatmeant and analysis is performed.


##################################### PRELIMINARIES #####################################

# Setup env -----------------------------------------------------------------------------

# We use tidyverse and magrittr to make R less of a barbaric mess.
library("tidyverse"); library("magrittr"); library("here");

# Paths to data files and theme objects for ggplot2
source(here("code", "paths.R")); source(here("code", "ggthemes.R"))

# mcutils.R contains important objects and utility functions for this project,
source(here("code", "mcutils.R"))
attach(mcutils)

lr <- lst()


# Parameters ----------------------------------------------------------------------------

# Range of birth years used in analysis, see method.
lr$by_range     <- c(1945:1996)

# Range of surveys used in analysis, exclude 1973 and 1976 bc do not contain lr scale.
lr$survey_range <- c(1979:2014)

# Variables included for use in analysis
lr$selection <- c("ID", "ID_study", "survey_year", "birth_year", 
                  "left_right_placement", "age", "gender")

# Breaks used in 10 year cohort plots
lr$tp10_breaks  <- c(-Inf, 1954, 1964, 1974, 1984, Inf)

# Cohort (bin) width used in ag plots
lr$ag1_binwidth <- 1
lr$ag2_binwidth <- 2

# Path to the data set used
lr$data_path_comb <- PATHS$outp$se_combined_analysis

attach(lr)

# Data pipe illustration, find mean and sd for left right placement
dpipe(PATHS$outp$se_combined_analysis, selection, by_range, survey_range) %>%
  filter(!is.na("left_right_placement")) %T>%
  (function(x) lr$avg <<- mean(x$left_right_placement, na.rm = T)) %>%
  (function(x) lr$sd  <<- mean(x$left_right_placement, na.rm = T))

####################################### PLOTS ###########################################

# ... left_right_tp10 ------------------------------------------------------------------
# Trackplot tracking arbitrary 10 year birth cohorts. 
# Plot pA - all cohorts, no conf. intervals
# Plot pB - 1945:, 1955:, 1965:, cohorts w. conf. intervals
# Plot pC - 1965:, 1975:, 1985;, cohorts w. conf. intervals

plot_lr_tp10 <- list()

# Data transform for A, B and C plots
lr$tp10pipe <- function(path) {
  dpipe(path, selection, by_range, survey_range) %>%
  filter(!is.na("left_right_placement")) %>%
  mutate("bin" = cut(birth_year, lr$tp10_breaks)) %>%
  group_by(bin) %>%
  mutate("cohort" = paste(min(birth_year), max(birth_year), sep = " : "),
         "binMin" = min(birth_year), "binMax" = max(birth_year)) %>%
  ungroup() %>%
  get_average("left_right_placement", "birth_year", "cohort", "survey_year") %>%
  return()
}

# PLOT tp10 pA
lr$tp10pipe(lr$data_path_comb) %>%
  (function(p) {
    p <-
      ggplot(p, aes(x = survey_year, y = avg)) +
      geom_line(aes(group = binMin, color = cohort, linetype = cohort), size = 0.7) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c("red", "blue", "green", "brown", "grey20")) +
      scale_fill_manual(values = c("red", "blue", "green", "brown", "E69F00")) +
      scale_linetype_manual(values = c(
        "1945 : 1954" = 1, "1955 : 1964" = 6, "1965 : 1974" = 4,
        "1975 : 1984" = 2, "1985 : 1996" = 5
      )) +
      scale_x_continuous(breaks = gg_xscale(5, 1970, 2015)) +
      ylim(0, 10) + coord_cartesian(ylim = c(4, 6), xlim = c(1979, 2014)) +
      gg_themes$myMinimal +
      theme(
        legend.position = "right",
        legend.direction = "vertical",
      ) +
      guides(color = guide_legend(override.aes = list(size = 0.8))) +
      xlab("Year") + ylab("Average left-right placement") +
      labs(color = "Legend,\nBirth cohort:", linetype = "Legend,\nBirth cohort:")
  }) %T>%
# Save png
  (function(p) {
   png(here("output", "figs", "plot_left_right_tp10_A.png"))
   print(p) 
   dev.off()
  }) -> plot_lr_tp10$pA

# PLOT tp10 pB
lr$tp10pipe(lr$data_path_comb) %>%
  filter(binMin %in% c(1945, 1955, 1965)) %>%
  (function(p) {
    p <-
      ggplot(p, aes(x = survey_year, y = avg)) +
      geom_ribbon(
        aes(group = binMin, ymin = err75Min, ymax = err75Max, fill = cohort),
        alpha = 0.2, show.legend = F
      ) +
      geom_line(aes(group = binMin, color = cohort, linetype = cohort), size = 0.7) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c("red", "blue", "green")) +
      scale_fill_manual(values = c("red", "blue", "green")) +
      scale_linetype_manual(
        values = c("1945 : 1954" = 1, "1955 : 1964" = 6, "1965 : 1974" = 4)
        ) +
      scale_x_continuous(breaks = gg_xscale(5, 1970, 2015)) +
      ylim(0, 10) + 
      coord_cartesian(ylim = c(4, 6), xlim = c(1979, 2014)) +
      gg_themes$myMinimal +
      theme(legend.position = "none") +
      xlab("Year") + ylab("Average left-right placement") +
      labs(color = "Birth cohort")
  }) %T>%
# Save png
  (function(p) {
   png(here("output", "figs", "plot_left_right_tp10_pB.png"))
   print(p) 
   dev.off()
  }) -> plot_lr_tp10$pB

# PLOT tp10 pC
lr$tp10pipe(lr$data_path_comb) %>%
  filter(binMin %in% c(1965, 1975, 1985)) %>%
  (function(p) {
    p <-
      ggplot(p, aes(x = survey_year, y = avg)) +
      geom_ribbon(
        aes(group = binMin, ymin = err75Min, ymax = err75Max, fill = cohort),
        alpha = 0.2, show.legend = F
      ) +
      geom_line(aes(group = binMin, color = cohort, linetype = cohort), size = 0.7) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c("green", "brown", "grey20")) +
      scale_fill_manual(values = c("green", "brown", "#E69F00")) +
      scale_linetype_manual(
        values = c("1965 : 1974" = 4, "1975 : 1984" = 2, "1985 : 1996" = 5)
        ) +
      scale_x_continuous(breaks = gg_xscale(5, 1970, 2015)) +
      ylim(0, 10) + 
      coord_cartesian(ylim = c(4, 6), xlim = c(1979, 2014)) +
      gg_themes$myMinimal +
      theme(legend.position = "none") +
      xlab("Year") + ylab("Average left-right placement") +
      labs(color = "Birth cohort")
  }) %T>%
# Save png
  (function(p) {
   png(here("output", "figs", "plot_left_right_tp10_pC.png"))
   print(p) 
   dev.off()
  }) -> plot_lr_tp10$pC

# Save .rds with plots 
saveRDS(plot_lr_tp10, here("output", "figs", "plot_left_right_tp10.rds"))
rm(plot_lr_tp10)

# ... left_right_ag --------------------------------------------------------------------
# Aggregate  

plot_lr_ag <- list() 

# The number of bins are calculated from desired binwidth
lr$ag2_nbin <- length(by_range)/lr$ag2_binwidth
lr$ag1_nbin <- length(by_range)/lr$ag2_binwidth
# Check that the cohorts can be of equal length
#stopifnot(length(by_range) %% ag2_binwidth == 0)
#stopifnot(ag2_binwidth * lr$ag2_nbin == length(by_range))

# Data transform for ag1 and ag2
lr$agpipe <- function(path, nbin) {
  dpipe(path, selection, by_range, survey_range) %>%
  filter(!is.na("left_right_placement")) %>%
  mutate("bin" = cut(birth_year, nbin)) %>%
  group_by(bin) %>%
  mutate("cohort" = paste(min(birth_year), max(birth_year), sep = " : "),
         "binMin" = min(birth_year), "binMax" = max(birth_year)) %>%
  ungroup() %>%
  get_average("left_right_placement", "birth_year", "cohort") %>%
  return()
}


# PLOT ag1 pA
lr$agpipe(lr$data_path_comb, lr$ag1_nbin) %>%
  (function(p) {
    p <-
  ggplot(p, mapping = aes(x = binMin, y = avg)) +
  geom_ribbon(aes(ymin = err95Min, ymax = err95Max), alpha = 1, fill = "grey90" ) +
  geom_ribbon(aes(ymin = err75Min, ymax = err75Max), alpha = 1, fill = "grey75") +
  geom_line(size = 0.40) +
  geom_hline(yintercept = lr$avg, linetype = "dashed", color = "blue", size = 0.4) +
  scale_x_continuous(breaks = gg_xscale(3, 1945, 1990)) +
  ylim(0,10) +
  coord_cartesian(ylim = c(4, 6), xlim =c(1945,1990)) + 
  xlab("Birth year") + ylab("Average left-right placement") +
  labs(caption = paste(
    "Avg LR placement for", lr$ag1_binwidth, "year cohorts", sep = " ")) +
  gg_themes$myMinimal
  }) %T>%
# Save png
  (function(p) {
   png(here("output", "figs", "plot_left_right_ag1A.png"))
   print(p) 
   dev.off()
  }) -> plot_lr_ag$p1a

# PLOT ag2 pA
lr$agpipe(lr$data_path_comb, lr$ag2_nbin) %>%
  (function(p) {
    p <-
  ggplot(p, mapping = aes(x = binMin, y = avg)) +
  geom_ribbon(aes(ymin = err95Min, ymax = err95Max), alpha = 1, fill = "grey90" ) +
  geom_ribbon(aes(ymin = err75Min, ymax = err75Max), alpha = 1, fill = "grey75") +
  geom_line(size = 0.40) +
  geom_hline(yintercept = lr$avg, linetype = "dashed", color = "blue", size = 0.4) +
  scale_x_continuous(breaks = gg_xscale(3, 1945, 1990)) +
  ylim(0,10) +
  coord_cartesian(ylim = c(4, 6), xlim =c(1945,1990)) + 
  xlab("Birth year") + ylab("Average left-right placement") +
  labs(caption = paste(
    "Avg LR placement for", lr$ag2_binwidth, "year cohorts", sep = " ")) +
  gg_themes$myMinimal
  }) %T>%
# Save png
  (function(p) {
   png(here("output", "figs", "plot_left_right_ag_2A.png"))
   print(p) 
   dev.off()
  }) -> plot_lr_ag$p2a

# Save .rds with plots 
saveRDS(plot_lr_ag, here("output", "figs", "plot_lr_ag.rds"))
rm(plot_lr_ag)

# ... left_right_ag --------------------------------------------------------------------

################################## Cluster analysis ####################################




####################################### Exit ###########################################

# Save lr_params -----------------------------------------------------------------------
saveRDS(lr,"output","figs","lr_params.rds")
# Clean env ----------------------------------------------------------------------------
rm(lr); rm(mcutils); rm(gg_themes)