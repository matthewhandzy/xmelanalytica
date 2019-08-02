# ----------------------------------------
#
# ui.R
#
# author: Даїшник
# email: matthewhandzy@gmail.com
# date: 08/01/2019 
# 
# copyright (c) Matthew Handzy, 2019
# 
# ----------------------------------------

library(htmltools)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# import important functions and dataframes
source("utils/xmel_utils.R", local = TRUE)

# --
#
# function:   attach_dependencies
# params:     (shiny ui) x
# returns:    none
#
# purpose:    attach html/css dependencies for shinydashboard
# 
# note:       none
# 
# --
attach_dependencies <- function(x) {
  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  } else {
    adminLTE_js <- "app.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }
  
  dashboardDeps <- list(
    htmlDependency("AdminLTE", "2.0.6",
                   c(file = system.file("AdminLTE", package = "shinydashboard")),
                   script = adminLTE_js,
                   stylesheet = adminLTE_css
    ),
    htmlDependency("shinydashboard",
                   as.character(utils::packageVersion("shinydashboard")),
                   c(file = system.file(package = "shinydashboard")),
                   script = "shinydashboard.js",
                   stylesheet = "shinydashboard.css"
    )
  )
  
  shinydashboard:::appendDependencies(x, dashboardDeps)
}

# page layout UI
ui <- tagList(fluidPage(
  
  # initialize shinyjs package
  useShinyjs(),
  
  # initialize theme
  theme = shinytheme("paper"),
  
  # set tab window image
  tags$head(HTML('<link rel="icon", href="herb_circle.png", type="image/png" />')),
  
  navbarPage(
    
    # set navpage title header
    title = div(img(height = 30,
                    width = 30,
                    src = "herb_circle.png"
                    ),
                HTML('&nbsp;'),
                "xmel analytica"
                ),
    
    # set window title
    windowTitle = "xmel analytica",
    
    # ---------- landing page ---------- #
    tabPanel("Січ",
             sidebarLayout(
               sidebarPanel(
                 pickerInput(inputId = "sich_map_choices",
                             label = "select group",
                             choices = c("кандидати", "січовики27", "січовики23"),
                             selected = "січовики27",
                             multiple = TRUE
                             ),
                 width = 2
                 ), # end sidebarPanel
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("хлюп",
                                      tags$br(),
                                      fluidRow(
                                        img(src='summer_rada2019.png', 
                                            align = "center",
                                            width = "100%")
                                        )
                                      ), # end tabPanel
                             tabPanel("карта",
                                      tags$br(),
                                      fluidRow(
                                        plotlyOutput("sich_map")
                                        ) # end fluidrow
                                      ) # end tabPanel
                             ) # end tabsetPanel
                 ) # end mainPanel
               ) # end sidebarLayout
             ), # end tabPanel
    
    # ---------- progress page ---------- #
    tabPanel("Кандидати",
             sidebarLayout(
               sidebarPanel(
                 pickerInput(inputId = "kandydat_progress_predict_selection",
                             label = "select кандидат",
                             choices = joined_names(xm_progress),
                             selected = "Matthew Handzy",
                             multiple = FALSE
                             ), # end picker input
                 width = 2
                 ), # end sidebarPanel
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("overview",
                                      tags$br(),
                                      fluidRow(
                                        valueBoxOutput("kandydat_numbers"),
                                        valueBoxOutput("kandydat_crossing"),
                                        valueBoxOutput("kandydat_influx")
                                      ), # end fluidRow
                                      tags$br(),
                                      fluidRow(
                                        plotlyOutput("kandydat_makeup_plot")
                                      ) # end fluidRow
                                      ), # end tabPanel
                             tabPanel("перша рада",
                                      tags$br(),
                                      fluidRow(column(8, plotlyOutput("kandydat_one_rada_fate")),
                                               column(4, plotlyOutput("kandydat_one_rada_attendance"))
                                        ) # end fluidRow
                                      ), # end tabPanel
                             tabPanel("прогрес",
                                      tags$br(),
                                      fluidRow(
                                        uiOutput("kandydat_progress_ranks"),
                                        uiOutput("kandydat_progress_rada_img")
                                        ) # end fluidRow
                                      ) # end tabPanel
                             ) # end tabsetPanel
                 ) # end mainPanel
               ) # end sidebarLayout
             ), # end tabpanel
    # end kandydat tabpanel
    
    # ---------- povnyj page ---------- #
    tabPanel("Січовики",
             sidebarLayout(
               sidebarPanel(
                 pickerInput(inputId = "dummy_input",
                             label = "dummy input",
                             choices = c("dummy inputs"),
                             multiple = FALSE
                             ),
                 width = 2
                 ), # end sidebarPanel
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("роки",
                                      tags$br(),
                                      plotlyOutput("sichovyk_boxplot_timeseries"),
                                      plotOutput("sichovyk_density_timeseries")
                               ), # end tabPanel
                             tabPanel("ніч",
                                      tags$br(),
                                      plotlyOutput("sichovyk_nich_distributions")
                                      ) # end tabPanel
                             ) # end tabsetPanel
                 ) # end mainPanel
               ) # end sidebarLayout
             ), # end tabPanel
    
    # ---------- dyadko page ---------- #
    tabPanel("Дядьки",
             collapsibleTreeOutput("dyadko_tree", height = "1200px")
             ) # end tabpanel dyadko
    
  ) # end navbarpage
)) # end fluidPage and taglist

# attach dependencies
ui <- attach_dependencies(tags$body(shiny::fluidPage(ui)))
