
# Simplifies loading from by_polity data set
getData <- function(PATH, b=T, POL="Sweden")  {if(b==T) readRDS(PATH)[[POL]][[1]] else readRDS(PATH)}

