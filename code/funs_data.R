# File comments ---------------------------------------------------------------

# Description                                                          
# ***************************************************************************** 
# Contains functions for additional transforming of already prepared data. The
# inital loading and wrangling of raw data is done by a seperate script. 

function()by_election %>% filter(polity == "Sweden") %>% .[c(1,3,4), ] %>%
  unnest(cols = "data") %>% 
  ungroup() %>% 
  prune("impute") %>%
  remove_na() %>% 
  as.matrix() %>%
  bnstruct::knn.impute() %>% 
  as_tibble()

prepare_sweden <- function(data = "by_election", POLITY = "Sweden", prune_list, choice) {
filter(polity = POLITY) %>%
    .[c(1,3,4), ] %>%
    unnest(cols = "data") %>%
    ungroup() %>%
    prune_by_list()
  
  
  
}