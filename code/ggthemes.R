# File comments --------------------------------------------------------------
# Contains figure themes for ggplot2.

gg_themes <- list()

gg_themes$myClassic <- 
  theme_classic() + 
  theme(plot.title = element_text(color = "dimgrey", size = 10.5, face = "plain"))

gg_themes$myMinimal <- 
  theme_minimal() + 
  theme(plot.title   = element_text(size = 8,   face = "plain", family = "URWTimes"),
        plot.caption = element_text(size = 7,   face = "plain", family = "URWTimes"),
        axis.title   = element_text(size = 7,   face = "plain", family = "URWTimes"),
        axis.text    = element_text(size = 7,   face = "plain", family = "URWTimes"),
        #axis.title.x = element_text(size = 7,   face = "plain", family = "URWTimes"),
        #axis.title.y = element_text(size = 7,   face = "plain", family = "URWTimes"),
        legend.title = element_text(size = 7,   face = "plain", family = "URWTimes"),
        legend.text  = element_text(size = 7,   face = "plain", family = "URWTimes"),
        #plot.margin = element_rect(colour= "black", fill = NA, size = 1),
        panel.border = element_rect(colour= "black", fill = NA, size = 1),
        panel.grid.minor = element_blank())
        
gg_themes$myMinimal_no_x_axis <-
  gg_themes$myMinimal +
  theme(axis.title.x  = element_blank())

gg_themes$noLines <-
  gg_themes$myMinimal  +
  theme(
  panel.grid.minor = element_blank())