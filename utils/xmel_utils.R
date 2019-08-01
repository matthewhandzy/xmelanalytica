library(collapsibleTree)
library(eeptools)
library(data.tree)
library(googlesheets)
library(markdown)
library(plotly)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

##### sich main page functions #####

# returns the map of where every member is from
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

##### kandydat functions #####

# function to calculate the current status
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

# rankup returns the next rank based on curr, next, and the date of curr 
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

# projects the future timeline for each stupin
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
  
  rm(ranks, i, current_rank)
  return(progress)
}

# plots who we have lost to
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

##### povnyj functions #####

# produces a boxplot of the ages of povnyj for each year
povnyj_ages_boxplot <- function() {
  summer_base <- "09/20/"
  tmp <- tibble(year = NA_integer_, age = NA_integer_)
  
  for (i in 2004:(Sys.Date() %>% year)) {
    itmp <- lubridate::as_datetime((xm_povnyj_27 %>% filter(nich <= i))$bday, format = "%m/%d/%Y") %>% 
      calc_age(refdate = lubridate::as_datetime(paste0(summer_base, i), format = "%m/%d/%Y"))
    
    tmp <- bind_rows(tmp, tibble(year = i, age = itmp)) %>% na.omit()
  }
  
  tmp$year <- factor(tmp$year, ordered = TRUE)
  
  p <- tmp %>% 
    plot_ly(x = ~year,
            y = ~age,
            type = "box")
  
  rm(summer_base, tmp, i, itmp)
  
  return(p)
}

povnyj_ages_density <- function() {
  summer_base <- "09/20/"
  tmp <- tibble(year = NA_integer_, age = NA_integer_)
  
  for (i in 2004:(Sys.Date() %>% year)) {
    itmp <- lubridate::as_datetime((xm_povnyj_27 %>% filter(nich <= i))$bday, format = "%m/%d/%Y") %>% 
      calc_age(refdate = lubridate::as_datetime(paste0(summer_base, i), format = "%m/%d/%Y"))
    
    tmp <- bind_rows(tmp, tibble(year = i, age = itmp)) %>% na.omit()
  }
  
  tmp$year <- factor(tmp$year, ordered = TRUE)
  
  p <- ggplot(tmp, aes(age, fill = year)) + 
    geom_density(alpha = 0.25, position = "identity")
  
  rm(summer_base, tmp, i, itmp)
  
  return(p)
}

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

##### dyadko #####

# creates the dyadko tree
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

##### rada functions #####

# returns a plotly bar graph of rada attendance
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

##### general util functions #####

# splits a name into firstname and lastname
split_name <- function(name) {
  # firstname = return_val[1]
  # lastname = return_val[2]
  return(strsplit(name, split = " ")[[1]])
}

# joins two names together
join_name <- function(firstname, lastname) {
  return(paste(firstname, lastname))
}

# takes in xm_progress DF and returns a list of joined names
joined_names <- function(df) {
  return((df %>% select(last_name, first_name) %>% mutate(full_name = paste(first_name, last_name)))$full_name)
}

# fetches a rada image from project directory
get_rada_img <- function(full_name, rank) {
  if(is.na(project_future(split_name(full_name)[1], split_name(full_name)[2])[[rank]])) {
    return("unknown.png")
  }
  if(project_future(split_name(full_name)[1], split_name(full_name)[2])[[rank]] == "skipped") {
    return("skipped.png")
  }
  return(paste0(project_future(split_name(full_name)[1], split_name(full_name)[2])[[rank]], ".png"))
}

# calculates the age given a birthday
calc_age <- function(bday, refdate = Sys.Date()) {
  return(as.period(interval(bday, refdate), unit = "year")$year)
}

##### import data #####

import_xmel_data <- function() {
  gs_auth()
  
  xm_att_sheet <- gs_title("xmel_attendance")
  xm_attendance <<- xm_att_sheet %>% gs_read(ws = "master")
  
  xm_katt_sheet <- gs_title("xmel_kandydat_attendance")
  xm_quant_progress <<- xm_katt_sheet %>% gs_read(ws = "quantitative_27")
  xm_qual_progress <<- xm_katt_sheet %>% gs_read(ws = "qualitative_27")
  
  xm_progress_sheet <- gs_title("xmel_progress")
  xm_progress <<-  xm_progress_sheet %>% gs_read(ws = "progress")
  
  xm_roster_sheet <- gs_title("xmel_roster")
  xm_one_rada <<- xm_roster_sheet %>% gs_read(ws = "one_rada")
  xm_candidates <<- xm_roster_sheet %>% gs_read(ws = "candidates")
  xm_povnyj_27 <<- xm_roster_sheet %>% gs_read(ws = "povnyj_27")
  xm_povnyj_23 <<- xm_roster_sheet %>% gs_read(ws = "povnyj_23")
  
  xm_ukraine_sheet <- gs_title("xmel_ukraine_roster")
  xm_ukraine_candidates <<- xm_ukraine_sheet %>% gs_read(ws = "candidates")
  xm_ukraine_povnyj_27 <<- xm_ukraine_sheet %>% gs_read(ws = "povnyj_27")
  xm_ukraine_povnyj_23 <<- xm_ukraine_sheet %>% gs_read(ws = "povnyj_23")
  
  rm(xm_att_sheet, xm_katt_sheet, xm_progress_sheet, xm_roster_sheet)
  export_xmel_data_local()
}

import_xmel_data_local <- function() {
  xm_attedance <<- read_csv(file = "files/xm_attendance.csv")
  xm_quant_progress <<- read_csv(file = "files/xm_quant_progress.csv")
  xm_qual_progress <<- read_csv(file = "files/xm_qual_progress.csv")
  xm_progress <<- read_csv(file = "files/xm_progress.csv")
  xm_one_rada <<- read_csv(file = "files/xm_one_rada.csv")
  xm_candidates <<- read_csv(file = "files/xm_candidates.csv")
  xm_povnyj_27 <<- read_csv(file = "files/xm_povnyj_27.csv")
  xm_povnyj_23 <<- read_csv(file = "files/xm_povnyj_23.csv")
  xm_ukraine_candidates <<- read_csv(file = "files/xm_ukraine_candidates.csv")
  xm_ukraine_povnyj_27<<- read_csv(file = "files/xm_ukraine_povnyj_27.csv")
  xm_ukraine_povnyj_23 <<- read_csv(file = "files/xm_ukraine_povnyj_23.csv")
}

export_xmel_data_local <- function() {
  write_csv(xm_attendance, path = "files/xm_attendance.csv")
  write_csv(xm_quant_progress, path = "files/xm_quant_progress.csv")
  write_csv(xm_qual_progress, path = "files/xm_qual_progress.csv")
  write_csv(xm_progress, path = "files/xm_progress.csv")
  write_csv(xm_one_rada, path = "files/xm_one_rada.csv")
  write_csv(xm_candidates, path = "files/xm_candidates.csv")
  write_csv(xm_povnyj_27, path = "files/xm_povnyj_27.csv")
  write_csv(xm_povnyj_23, path = "files/xm_povnyj_23.csv")
  write_csv(xm_ukraine_candidates, path = "files/xm_ukraine_candidates.csv")
  write_csv(xm_ukraine_povnyj_27, path = "files/xm_ukraine_povnyj_27.csv")
  write_csv(xm_ukraine_povnyj_23, path = "files/xm_ukraine_povnyj_23.csv")
}
