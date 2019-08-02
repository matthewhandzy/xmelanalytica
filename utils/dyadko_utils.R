# ----------------------------------------
#
# dyadko_utils.R
#
# author: Даїшник
# email: matthewhandzy@gmail.com
# date: 08/01/2019 
# 
# copyright (c) Matthew Handzy, 2019
# 
# ----------------------------------------

library(collapsibleTree)
library(data.tree)
library(plotly)
library(tidyverse)

# -- dyadko page utility functions -- #

# --
#
# function:   render_dyadko_tree
# params:     none
# returns:    (collapsibleTree) tree of dyadko relationship
#
# purpose:    visualize the relationship of everybody in the kurin
# 
# note:       none
#
# --
render_dyadko_tree <- function() {
  "%ni%" <- Negate('%in%')
  
  # pre-process kandydat/povnyj information
  xm_relu <- tibble()
  xm23 <- xm_povnyj_23 %>% select(psevdo, dyadko) %>% na.omit()
  xm27 <- xm_povnyj_27 %>% select(psevdo, dyadko) %>% na.omit()
  xmc <- xm_candidates %>% select(ukr_name, dyadko) %>% 
    select(psevdo = ukr_name, dyadko) %>% 
    na.omit()
  xm <- bind_rows(xm23, xm27, xmc)
  
  # create relationship between children and parents
  for (row in 1:nrow(xm)) {
    ch <- xm[row,"psevdo"]$psevdo
    pa <- xm[row,"dyadko"]$dyadko
    
    xm_relu <- bind_rows(xm_relu, tibble(child = ch, parent = pa))
  }
  
  # add in null parents for dyadkos with no parents
  xm_missing <- (xm_relu)[xm_relu$parent %ni% xm_relu$child,]$parent
  for (name in xm_missing) {
    xm_relu <- bind_rows(xm_relu, tibble(parent = ".", child = name))
  }
  
  formatted_df <- ToDataFrameTypeCol(FromDataFrameNetwork(as.data.frame(xm_relu %>% select(parent, child))))
  
  # need to now remove those unnecessary "."s
  tree <- collapsibleTree(formatted_df, 
                          colnames(formatted_df)[2:5], 
                          collapsed = FALSE,
                          width = "500px",
                          height = "1200px",
                          root= "Дядки")
  
  rm(xm, xmc, xm27, xm23, xm_relu, xm_missing)
  rm(formatted_df, row, name, ch, pa)
  rm("%ni%")
  
  return(tree)
}
