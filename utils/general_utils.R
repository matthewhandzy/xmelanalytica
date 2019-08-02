# ----------------------------------------
#
# general_utils.R
#
# author: Даїшник
# email: matthewhandzy@gmail.com
# date: 08/01/2019 
# 
# copyright (c) Matthew Handzy, 2019
# 
# ----------------------------------------

library(eeptools)
library(googlesheets)
library(plotly)
library(tidyverse)

# -- general util functions -- #

# --
#
# function:   split_name
# params:     (string) full_name
# returns:    (list) split name
#
# purpose:    split a name into firstname, lastname
# 
# note:       none
#
# --
split_name <- function(name) {
  # firstname = return_val[1]
  # lastname = return_val[2]
  return(strsplit(name, split = " ")[[1]])
}

# --
#
# function:   join_name
# params:     (string) firstname, (string) lastname
# returns:    (string) full name
#
# purpose:    joins a firstname, lastname into fullname
# 
# note:       none
#
# --
join_name <- function(firstname, lastname) {
  return(paste(firstname, lastname))
}

# --
#
# function:   joined_names
# params:     (tibble) df
# returns:    (tibble) tibble of joined names
#
# purpose:    join firstname, lastname in large quantities
# 
# note:       none
#
# --
joined_names <- function(df) {
  return((df %>% select(last_name, first_name) %>% mutate(full_name = paste(first_name, last_name)))$full_name)
}

# --
#
# function:   get_rada_img
# params:     (string) full_name, (string) rank
# returns:    (string) pathname of an image file
#
# purpose:    determine when a particular full_name
#             receieved a particular rank, and fetch pathname
# 
# note:       none
#
# --
get_rada_img <- function(full_name, rank) {
  if(is.na(project_future(split_name(full_name)[1], split_name(full_name)[2])[[rank]])) {
    return("unknown.png")
  }
  if(project_future(split_name(full_name)[1], split_name(full_name)[2])[[rank]] == "skipped") {
    return("skipped.png")
  }
  return(paste0(project_future(split_name(full_name)[1], split_name(full_name)[2])[[rank]], ".png"))
}

# --
#
# function:   calc_age
# params:     (datetime) bday, (datetime) refdate
# returns:    (int) age of person with birthday bday
#
# purpose:    calculate the age of a particular kandydat
#             or sichovyk
# 
# note:       none
#
# --
calc_age <- function(bday, refdate = Sys.Date()) {
  return(as.period(interval(bday, refdate), unit = "year")$year)
}

# --
#
# function:   import_xmel_data
# params:     none
# returns:    none
#
# purpose:    imports xmel data from googlesheets
# 
# note:       requires gs authentication
#
# --
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

# --
#
# function:   import_xmel_data_local
# params:     none
# returns:    none
#
# purpose:    imports xmel data from local csv
# 
# note:       none
#
# --
import_xmel_data_local <- function() {
  xm_attendance <<- read_csv(file = "files/xm_attendance.csv")
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

# --
#
# function:   export_xmel_data_local
# params:     none
# returns:    none
#
# purpose:    writes xmel data to local files
# 
# note:       none
#
# --
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

import_and_export_local <- function() {
  
  import_xmel_data()
  export_xmel_data_local()
  
}