# ----------------------------------------
#
# sichovyk_utils.R
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
# -- sichovyk page utility functions -- #
# --

# --
#
# function:   sichovyk_makeup_scatterplot
# params:     none
# returns:    (plotly) bubble scatterplot timeseries of age distribution of povnyj
#
# purpose:    visualize the makeup of our members over time
# 
# note:       none
#
# --
sichovyk_makeup_scatterplot <- function() {
  
  source("utils/general_utils.R", local = TRUE)
  
  tmp_df <- xm_attendance[,4:length(xm_attendance)]
  
  for (i in 1:nrow(tmp_df)) {
    for (j in 1:(ncol(tmp_df)-1)) {
      if (is.na(tmp_df[i, j+1])) {
        tmp_df[i, j+1] <- tmp_df[i, j]
      }
    }
  }
  
  data <- tibble()
  
  for (i in 1:ncol(tmp_df)) {
    
    # number of kandydaty + povnyj
    tmp <- (tmp_df[,i] %>% 
              na.omit() %>% 
              count()
            )$n
    
    # count number of povnyj
    tmp_povnyj_count <- (xm_povnyj_27 %>% filter(nich < (strsplit(tmp_df[,i] %>% names(.), split = " ")[[1]][2])) %>% count())$n
    
    # number of kandydaty
    tmp_kandydat_count <- tmp - tmp_povnyj_count
    
    # get povnyj from 23 list that were relevant in __year__
    senior <- tibble(bday = xm_povnyj_23$bday %>% na.omit())
    tmp_year <- as.integer((strsplit(tmp_df[,i] %>% names(.), split = " ")[[1]][2]))
    for (j in 1:nrow(senior)) {
      if (calc_age(lubridate::as_datetime(senior[j,]$bday, format = "%m/%d/%Y")) <= 35) {
        tmp <- tmp + 1
      }
    }
    
    data <- bind_rows(data, tibble(year = tmp_year,
                                   kandydat_count = tmp_kandydat_count,
                                   povnyj_count = tmp_povnyj_count,
                                   total_count = tmp,))
  }
  
  data <- data %>% 
    group_by(year) %>% 
    summarise_each(funs(max))
  
  p <- data %>% 
    plot_ly(x = ~year,
            y = ~povnyj_count,
            text = ~kandydat_count,
            type = "scatter",
            mode = "markers",
            marker = list(size = ~kandydat_count,
                          opacity = 0.5))
  
  rm(data, i, j, senior, tmp, tmp_df, tmp_kandydat_count, tmp_povnyj_count, tmp_year)
  
  return(p)
}

# --
#
# function:   sichovyk_ages_boxplot
# params:     none
# returns:    (plotly) boxplot timeseries of age distribution of povnyj
#
# purpose:    visualize the ages of our members over time
# 
# note:       none
#
# --
sichovyk_ages_boxplot <- function() {
  summer_base <- "09/20/"
  tmp <- tibble()
  
  for (i in 2004:(Sys.Date() %>% lubridate::year())) {
    itmp <- lubridate::as_datetime((xm_povnyj_27 %>% filter(nich <= i))$bday, format = "%m/%d/%Y") %>% 
      calc_age(refdate = lubridate::as_datetime(paste0(summer_base, i), format = "%m/%d/%Y"))
    
    tmp <- bind_rows(tmp, tibble(year = i, age = itmp)) %>% na.omit()
  }
  
  tmp$year <- factor(tmp$year, ordered = TRUE)
  
  p <- tmp %>% 
    plot_ly(x = ~year,
            y = ~age,
            type = "box") %>% 
    layout(title = "Distribution of Січовики Age by Year",
           xaxis = list(title = "year"),
           yaxis = list(title = "age range")
    )
  
  rm(summer_base, tmp, i, itmp)
  
  return(p)
}

# --
#
# function:   sichovyk_ages_density
# params:     none
# returns:    (plotly) density timeseries of age distribution of povnyj
#
# purpose:    visualize the ages of our members over time
# 
# note:       none
#
# --
sichovyk_ages_density <- function() {
  summer_base <- "09/20/"
  tmp <- tibble()
  
  for (i in 2004:(Sys.Date() %>% lubridate::year())) {
    itmp <- lubridate::as_datetime((xm_povnyj_27 %>% filter(nich <= i))$bday, format = "%m/%d/%Y") %>% 
      calc_age(refdate = lubridate::as_datetime(paste0(summer_base, i), format = "%m/%d/%Y"))
    
    tmp <- bind_rows(tmp, tibble(year = i, age = itmp)) %>% na.omit()
  }
  
  tmp$year <- factor(tmp$year, ordered = TRUE)
  
  p <- ggplot(tmp, aes(age, fill = year)) + 
    geom_density(alpha = 0.25, position = "identity") +
    labs(title = "Density Plot of Січовик Ages per Year") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(label = "age") +
    ylab(label = "density")
    
  
  rm(summer_base, tmp, i, itmp)
  
  return(p)
}

# --
#
# function:   povnyj_ages_boxplot
# params:     none
# returns:    (plotly) pie graph of povnyj pledge classes
#
# purpose:    visualize the makeup of our membership based
#             on when they became povnyj
# 
# note:       none
#
# --
# produces a plotly graph of where each povni class came from
sichovyk_nich_plotly_proportions <- function() {
  p <- xm_povnyj_27 %>% 
    group_by(nich) %>% 
    summarise(count = n()) %>% 
    plot_ly(labels = ~nich, values = ~count) %>% 
    add_pie(hole = 0.6) %>% 
    layout(title = "Proportion of Повний by Class",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p)
}