# File comments ---------------------------------------------------------------
# Contains figure scripts

# Table functions -------------------------------------------------------------

tab_standard <- function(caption, striped = TRUE, color = "black", ...) {
  
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

# GGplot theme objects --------------------------------------------------------

gg_themes <- list()

gg_themes$myClassic <- 
  theme_classic() + 
  theme(plot.title = element_text(color = "dimgrey", size = 10.5, face = "plain"))

gg_themes$myMinimal <- 
  theme_minimal() + 
  theme(plot.title   = element_text(size = 11, face = "plain"),
        plot.caption = element_text(size = 10, face = "plain"),
        axis.title.x = element_text(size = 10.5, face = "plain"))

# GGplot mod functions --------------------------------------------------------

gg_xscale <- function(by, minval, maxval = NULL, type = "continous") {
  if(is.null(maxval)) {maxval <- minval}
  if(type == 1 | type == "continous") {
    x <- seq(min(minval), max(maxval), by = by)
  }
  return(x)
}

#gg_modify$labs <- function(title = NULL, cap = NULL) return(labs(title, cap))


# Scraps ----------------------------------------------------------------------
# 
# plot_classic <- function(df = data.frame(), plot_title = NULL, plot_caption = NULL) {
#   plot <- ggplot(data = df) +
#     theme_classic() +
#     theme(
#       plot.title = element_text(color = "dimgrey", 
#                                 size = 10.5, hjust = -0.1, 
#                                 face = "plain" 
#                                 )) +
#     labs(title = plot_title,
#          caption = plot_caption)
# } 
# 
# 
# plot_minimal <- function(df = data.frame(), plot_title = NULL, plot_caption = NULL) {
#   plot <- ggplot(data = df) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(color = "dimgrey", 
#                                 size = 10.5, hjust = -0.1, 
#                                 face = "plain", 
#                                 family = "URWHelvetica")) +
#     labs(title = plot_title,
#          caption = plot_caption)
# } 
# 
# plot_nolines <- function(df = data.frame(), plot_title = NULL, plot_caption = NULL) {
#   plot_minimal(df, plot_title, plot_caption) +
#     theme(
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank())
# }
# 
# 
