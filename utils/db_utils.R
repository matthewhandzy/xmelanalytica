# ----------------------------------------
# 
# db_utils.R
#
# author: Даїшник
# email: matthewhandzy@gmail.com
# date: 08/02/2019 
# 
# copyright (c) Matthew Handzy, 2019
# 
# ----------------------------------------

library(DBI)
library(pool)
library(tidyverse)

# --
# -- database functions -- #
# --

# --
#
# function:   poolConnect
# params:     none
# returns:    (pool) db connection object
#
# purpose:    connect to db
# 
# note:       none
# 
# --
poolConnect <- function() {
  return(
    pool::dbPool(drv = RMySQL::MySQL(),
                 dbname = "xmelanalytica",
                 host = "barbeerdrinker.cwbowizjcrlm.us-west-1.rds.amazonaws.com",
                 username = "juanmatthew",
                 password = "password"
                 )
  )
}

# -- database functions -- #

# --
#
# function:   db_get_xm_table
# params:     (string) table_name
# returns:    (bool) TRUE / FALSE
#
# purpose:    fetch a particular table
# 
# note:       none
# 
# --
db_get_xm_table <- function(table_name) {
  pool <- poolConnect()
  
  tmp <- pool %>% 
    tbl(table_name) %>% 
    collect()
  
  poolClose(pool)
  
  rm(pool)
  
  return(tmp)
}

# --
#
# function:   db_update_xm_table
# params:     (string) df_name, (tibble) df, (bool) OVERWRITE
# returns:    (bool) TRUE / FALSE
#
# purpose:    update a particular table in the database
# 
# note:       none
# 
# --
db_update_xm_table <- function(df_name, df, OVERWRITE) {
  pool <- poolConnect()
  dbWriteTable(conn = pool, 
               name = df_name, 
               value = df, 
               row.names = FALSE,
               overwrite = OVERWRITE)
  poolClose(pool)
  rm(pool)
}

# -- database functions -- #

# --
#
# function:   db_get_from_sheets
# params:     none
# returns:    none
#
# purpose:    imports xmel data from googlesheets
# 
# note:       requires gs authentication
#
# --
db_get_from_sheets <- function() {
  gs_auth()
  
  xm_att_sheet <- gs_title("xmel_attendance")
  xm_attendance <<- xm_att_sheet %>% gs_read(ws = "overall")
  
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
}

# --
#
# function:   db_update_from_sheets
# params:     none
# returns:    (bool) TRUE / FALSE
#
# purpose:    force update all tables
# 
# note:       none
# 
# --
db_update_from_sheets <- function() {
  db_get_from_sheets()
  db_update_all(force_overwrite = TRUE)
}

# --
#
# function:   db_get_all
# params:     none
# returns:    (bool) TRUE / FALSE
#
# purpose:    fetch all tables
# 
# note:       none
# 
# --
db_get_all <- function() {
  pool <- poolConnect()
  
  xm_attendance <<- pool %>% tbl("xm_attendance") %>% collect()
  xm_progress <<- pool %>% tbl("xm_progress") %>% collect()
  xm_one_rada <<- pool %>% tbl("xm_one_rada") %>% collect()
  xm_candidates <<- pool %>% tbl("xm_candidates") %>% collect()
  xm_povnyj_27 <<- pool %>% tbl("xm_povnyj_27") %>% collect()
  xm_povnyj_23 <<- pool %>% tbl("xm_povnyj_23") %>% collect()
  xm_ukraine_candidates <<- pool %>% tbl("xm_ukraine_candidates") %>% collect()
  xm_ukraine_povnyj_27<<- pool %>% tbl("xm_ukraine_povnyj_27") %>% collect()
  xm_ukraine_povnyj_23 <<- pool %>% tbl("xm_ukraine_povnyj_23") %>% collect()
  
  poolClose(pool)
  rm(pool)
}

# --
#
# function:   db_update_all
# params:     none
# returns:    (bool) TRUE / FALSE
#
# purpose:    force update all tables
# 
# note:       none
# 
# --
db_update_all <- function(force_overwrite = FALSE) {
  db_update_xm_table("xm_attendance", xm_attendance, force_overwrite)
  db_update_xm_table("xm_progress", xm_progress, force_overwrite)
  db_update_xm_table("xm_one_rada", xm_one_rada, force_overwrite)
  db_update_xm_table("xm_candidates", xm_candidates, force_overwrite)
  db_update_xm_table("xm_povnyj_27", xm_povnyj_27, force_overwrite)
  db_update_xm_table("xm_povnyj_23", xm_povnyj_23, force_overwrite)
  db_update_xm_table("xm_ukraine_candidates", xm_ukraine_candidates, force_overwrite)
  db_update_xm_table("xm_ukraine_povnyj_27", xm_ukraine_povnyj_27, force_overwrite)
  db_update_xm_table("xm_ukraine_povnyj_23", xm_ukraine_povnyj_23, force_overwrite)
}
