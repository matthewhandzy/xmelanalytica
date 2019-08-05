# ----------------------------------------
#
# kandydat_utils.R
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
# -- kandydat page utility functions -- #
# --

# ----- calculating functions ----- #

# --
#
# function:   get_rank
# params:     (string) firstname, (string) lastname
# returns:    (string) rank code for a kandydat/sichovyk
#
# purpose:    calculate the rank of a given member
# 
# --
get_rank <- function(firstname, lastname) {
  cand <- xm_progress %>% filter(first_name == firstname, last_name == lastname)
  if (nrow(cand) == 0) {
    return("nodata")
  }
  
  current_rank <- NA_character_
  indicator <- TRUE
  # check for povnyj
  if (!is.na(cand$sichovyk) & indicator) {
    current_rank <- "sichovyk"
    indicator <- FALSE
  } 
  
  if (!is.na(cand$mk) & indicator) {
    current_rank <- "mk"
    indicator <- FALSE
  } 
  
  if (!is.na(cand$dzura)  & indicator) {
    current_rank <- "dzura"
    indicator <- FALSE
  }
  
  if (!is.na(cand$pryx) & indicator) {
    current_rank <- "pryx"
    indicator <- FALSE
  } 
  
  if (!is.na(cand$pidx) & indicator) {
    current_rank <- "pidx"
    indicator <- FALSE
  }
  
  rm(cand, indicator)
  
  return(current_rank)
}

# --
#
# function:   rankup
# params:     (string) rank, () date_rank, (flag) next_rada_flag
# returns:    (string) a combinatio of "w/s" and a year of promotion
#
# purpose:    calculate the next eligible promotion for a particular
#             rank, given a particular date / rada
# 
# note:       rankup returns the next rank based on curr, next, 
#             and the date of curr 
#
# --
rankup <- function(rank, date_rank, next_rada_flag) {
  
  nextrada <- "s19"
  
  # every rank-up is 6 months with the exception of mk->sichovyk
  if (rank == "sichovyk") {
    return(paste0("s", toString(strtoi(substr(date_rank, 2, 3))+1)))
  }
  
  if (next_rada_flag) {
    return(nextrada)
  }
  
  month <- substr(date_rank, 1, 1)
  year <- substr(date_rank, 2, 3)
  next_month <- NA_character_
  next_year <- NA_character_
  
  if (month == "w") {
    next_month = "s"
    next_year <- year
  }
  if (month == "s") {
    next_month <- "w"
    next_year <- toString(strtoi(year)+1)
  }
  
  rm(nextrada, month, year)
  
  return(paste0(next_month, next_year))
  
}

# --
#
# function:   project_future
# params:     (string) firstname, (string) lastname
# returns:    (tibble) a tibble of future promotion dates
#
# purpose:    calculate the potential future promotion dates
#             given a particular member
# 
# note:       projects the future timeline for each stupin
#
# --
project_future <- function(firstname, lastname) {
  progress <- xm_progress %>% filter(first_name == firstname, last_name == lastname)
  
  current_rank <- get_rank(firstname, lastname)
  if (current_rank == "nodata") {
    return(current_rank)
  }
  if (current_rank == "sichovyk") {
    return(progress)
  }
  
  ranks <- colnames(progress)[3:7]
  
  # next eligible rada flag (keeps future promotions up-to-date)
  next_rada <- TRUE
  
  # find future projected dates
  for (i in (match(current_rank, ranks)+1):5) {
    progress[,i+2] <- rankup(ranks[i], data.frame(progress)[,i+2-1], next_rada)
    next_rada <- FALSE
  }
  
  rm(current_rank, ranks, i)
  return(progress)
}

# ----- graphing functions ----- #

# --
#
# function:   kandydat_makeup
# params:     none
# returns:    (plotly) a pie plot of kandydat composition
#
# purpose:    creates a graph of what kandydat ranks we have
#             and in what numbers
# 
# note:       none
#
# --
kandydat_makeup <- function() {
  tmp <- tibble()
  for (i in 1:nrow(xm_candidates)) { 
    tmp <- bind_rows(tmp, 
                     tibble(kandydat = paste(xm_candidates[i,]$first_name, xm_candidates[i,]$last_name),
                            rank = get_rank(xm_candidates[i,]$first_name, xm_candidates[i,]$last_name)
                     )
    )
  } 
  
  p <- tmp %>% 
    na.omit() %>% 
    group_by(rank) %>% 
    count() %>% 
    ungroup() %>% 
    plot_ly(labels = ~factor(rank, levels = c("pryx", "dzura", "mk")),
            values = ~n,
            marker = list(colors = c("A9A9A9", "800000", "FFBF00"))) %>% 
    add_pie(hole = 0.4) %>% 
    layout(title = "Composition of Кандидати",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  rm(i, tmp)
  
  return(p)
}

# --
#
# function:   kandydat_makeup_timeseries
# params:     none
# returns:    (plotly) stacked bar chart of distribution of each rank
#             each year
#             
#
# purpose:    creates a graph of what kandydat ranks we have
#             and in what numbers, per year
# 
# note:       none
#
# --
kandydat_makeup_timeseries <- function() {
  tmp_df <- xm_attendance[,26:length(xm_attendance)]
  
  for (i in 1:nrow(tmp_df)) {
    for (j in 1:(ncol(tmp_df)-1)) {
      if (is.na(tmp_df[i, j+1])) {
        tmp_df[i, j+1] <- tmp_df[i, j]
      }
    }
  }
  
  data <- tibble()
  
  for (i in 1:ncol(tmp_df)) {
    tmp <- tmp_df[,i] %>% 
      na.omit() %>% 
      group_by_at(1) %>% 
      count()

    yearz <- (tmp %>% names(.))[1]
    colnames(tmp) <- c("rank", "count")
    
    data <- bind_rows(data, tibble(year = yearz, rank = tmp$rank, count = tmp$count))
  }
  
  data$year <- factor(data$year, levels = (data$year %>% unique()))
  
  p <- data %>% 
    filter(rank != "x") %>% 
    group_by(year) %>% 
    plot_ly(x = ~year,
            y = ~count,
            type = "bar",
            name = ~rank,
            color = ~rank) %>% 
    layout(barmode = "stack")
  
  rm(i, j, tmp_df, tmp, data)
  
  return(p)
}

# --
#
# function:   one_rada_plotly_proportions
# params:     none
# returns:    (plotly) a tibble of future promotion dates
#
# purpose:    creates a graph of what other kureni we have lost
#             potential members to
# 
# note:       none
#
# --
one_rada_plotly_proportions <- function() {
  p <- xm_one_rada %>% 
    select(status) %>% 
    group_by(status) %>% 
    summarise(count = n()) %>% 
    plot_ly(labels = ~factor(status), 
            values = ~count,
            marker = list(colors = c("FFDF00", "69FFCA", "800080", "C21807", "1B1E23", "0B6623", "3895D3"))) %>% 
    add_pie(hole = 0.6) %>% 
    layout(title = "One Рада Кандидати",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p)
}

# --
#
# function:   one_rada_plotly_attendance
# params:     none
# returns:    (plotly) a bar graph of first-timers per year
#
# purpose:    show how many first timers we attract per year
# 
# note:       none
#
# --
# plots what year one rada attendees came
one_rada_plotly_attendance <- function() {
  p <- tibble(year = factor(grep("[0-9]", unlist(strsplit(xm_one_rada$rada, " ")), value = TRUE))) %>%
    group_by(year) %>% 
    count() %>% 
    plot_ly(x = ~year,
            y = ~n,
            type = "bar") %>% 
    layout(title = "One Rada Attendees by Year",
           xaxis = list(title = "year"),
           yaxis = list(title = "number of attendees")
    )
  
  return(p)
}