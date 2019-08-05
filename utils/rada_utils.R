# ----------------------------------------
#
# rada_utils.R
#
# author: Даїшник
# email: matthewhandzy@gmail.com
# date: 08/01/2019 
# 
# copyright (c) Matthew Handzy, 2019
# 
# ----------------------------------------

library(plotly)
library(tidyverse)

# --
# -- rada page utility functions -- #
# --

# --
#
# function:   rada_attendance
# params:     none
# returns:    (plotly) bar graph of rada attendance
#
# purpose:    visualize the attendance of rada
# 
# note:       none
#
# --
rada_attendance <- function() {
  tmp_df <- tibble(rada = tf <- factor(colnames(xm_attendance)[5:length(colnames(xm_attendance))-1], 
                                       ordered = TRUE, 
                                       levels = colnames(xm_attendance)[5:length(colnames(xm_attendance))-1]), 
                   attendance = NA_integer_)
  
  for (i in 4:ncol(xm_attendance)-1) {
    tmp_df[i-3, 2] <- (xm_attendance[i] %>% na.omit() %>% count())$n
  }
  
  p <- tmp_df %>% 
    plot_ly(
      x = ~rada,
      y = ~attendance,
      name = "test",
      type = "bar"
    )
  
  rm(tmp_df, i)
  
  return(p)
}
