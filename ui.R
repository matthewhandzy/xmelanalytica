# Даїшник
# 2019
# ui.R

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)

# import important functions and dataframes
source("utils/xmel_utils.R", local = TRUE)
import_xmel_data_local()

ui <- tagList(fluidPage(
  
  # initialize shinyjs package
  useShinyjs(),
  
  # initialize theme
  theme = shinytheme("paper"),
  
  # set window image
  tags$head(HTML('<link rel="icon", href="herb_circle.png", type="image/png" />')),
  
  navbarPage(
    
    # set navpage title
    title = div(img(height = 30,
                    width = 30,
                    src = "herb_circle.png"
                    ),
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
                 pickerInput(inputId = "progress_predict_selection",
                             label = "select кандидат",
                             choices = joined_names(xm_progress),
                             selected = joined_names(xm_progress)[1],
                             multiple = FALSE
                             ), # end picker input
                 width = 2
                 ), # end sidebarPanel
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("overview",
                                      tags$br()
                                      
                                      ), # end tabPanel
                             tabPanel("перша рада",
                                      tags$br(),
                                      fluidRow(column(8, plotlyOutput("one_rada_fate")),
                                               column(4, plotlyOutput("one_rada_attendance"))
                                        ) # end fluidRow
                                      ), # end tabPanel
                             tabPanel("прогрес",
                                      tags$br(),
                                      fluidRow(
                                        uiOutput("progress_ranks"),
                                        uiOutput("progress_rada_img")
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
                             tabPanel("ніч",
                                      tags$br(),
                                      plotlyOutput("nich_distributions")
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
