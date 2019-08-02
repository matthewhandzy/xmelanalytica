# ----------------------------------------
# 
# sich_utils.R
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

# -- sich main page functions -- #

# --
#
# function:   sich_get_map
# params:     (reactive list) choices
# returns:    (plotly) plotly map graph object
#
# purpose:    generate a plot of the geographic distribution of 
#             kandydaty, sichovyky (27 or 23) in the USA
# 
# note:       none
# 
# --
sich_get_map <- function(choices) {
  
  df <- tibble()
  
  if ("кандидати" %in% choices) {
    df <- bind_rows(df, xm_candidates %>% select(state))
  }
  
  if("січовики27" %in% choices) {
    df <- bind_rows(df, xm_povnyj_27 %>% select(state))
  }
  
  if ("січовики23" %in% choices) {
    df <- bind_rows(df, xm_povnyj_23 %>% select(state))
  }
  
  df <- df %>% 
    na.omit() %>% 
    select(state) %>% 
    group_by(state) %>% 
    summarise(count = n())
  
  l <- list(color = toRGB("white"), width = 2)
  g <- list(
    scope = "usa",
    projection = list(type = "albers usa"),
    showlakes = TRUE,
    lakecolor = toRGB("white")
  )
  
  pal <- c("violet", "blue", "green", "yellow", "orange", "red")
  
  p <- plot_geo(df, locationmode = "USA-states") %>%
    add_trace(z = ~count, 
              text = ~count, 
              locations = ~state,
              color = ~count, 
              colors = pal
    ) %>%
    colorbar(title = "Січовики") %>%
    layout(title = "Distribution of Січовики in USA",
           geo = g
    )
  
  rm(df, l, g, pal)
  
  return(p)
}