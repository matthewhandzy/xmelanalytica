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

# -- database functions -- #

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
}

# -- database functions -- #

# --
#
# function:   db_force_update_all
# params:     none
# returns:    (bool) TRUE / FALSE
#
# purpose:    force update all tables
# 
# note:       none
# 
# --
db_force_update_all <- function() {
  force_overwrite <- TRUE
  db_update_xm_table("xm_attendance", xm_attendance, force_overwrite)
  db_update_xm_table("xm_candidates", xm_candidates, force_overwrite)
  db_update_xm_table("xm_one_rada", xm_one_rada, force_overwrite)
  db_update_xm_table("xm_povnyj_27", xm_povnyj_27, force_overwrite)
  db_update_xm_table("xm_povnyj_23", xm_povnyj_23, force_overwrite)
  db_update_xm_table("xm_progress", xm_progress, force_overwrite)
  db_update_xm_table("xm_qual_progress", xm_qual_progress, force_overwrite)
  db_update_xm_table("xm_quant_progress", xm_quant_progress, force_overwrite)
  db_update_xm_table("xm_ukraine_candidates", xm_ukraine_candidates, force_overwrite)
  db_update_xm_table("xm_ukraine_povnyj_27", xm_ukraine_povnyj_27, force_overwrite)
  db_update_xm_table("xm_ukraine_povnyj_23", xm_ukraine_povnyj_23, force_overwrite)
}
