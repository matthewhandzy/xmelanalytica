# ----------------------------------------
#
# server.R
#
# author: Даїшник
# email: matthewhandzy@gmail.com
# date: 08/01/2019 
# 
# copyright (c) Matthew Handzy, 2019
# 
# ----------------------------------------

library(shiny)
library(tidyverse)

# import important functions and dataframes
source("utils/xmel_utils.R", local = TRUE)

# import working dataset from db
import_xmel_data_local()

# constant definitions
DEF_WIDTH = 110
DEF_HEIGHT = 150
NEXT_RADA = "summer 2019"

# --
#
# function:   sich_get_map
# params:     (ui) input, (ui) output, (shiny) session
# returns:    none
#
# purpose:    drive back-end rendering of output
# 
# note:       none
# 
# --
server <- function(input, output, session) {
  
  # ---------- sich page ---------- #
  
  output$rada_attendance <- renderPlotly({
    rada_attendance()
  })
  
  # --
  #
  # output name:    sich_map
  # output type:    (plotly) map object
  #
  # purpose:        output the geographic distribution of 
  #                 members across the USA
  # 
  # note:           none
  # 
  # --
  output$sich_map <- renderPlotly({ 
    sich_get_map(input$sich_map_choices) 
  })
  
  # ---------- kandydaty page ---------- #
  
  # --
  #
  # output name:    kandydat_numbers
  # output type:    (shinydashboard) valueBox
  #
  # purpose:        output the number of kandydaty
  # 
  # note:           none
  # 
  # --
  output$kandydat_numbers <- renderValueBox({
    valueBox(
      value = (xm_candidates %>% count())$n,
      subtitle = tags$b("Кандидат Count"),
      icon = icon("child"),
      color = "red"
    )
  })
  
  # --
  #
  # output name:    kandydat_crossing
  # output type:    (shinydashboard) valueBox
  #
  # purpose:        output the number of kandydaty
  #                 slated to cross over next rada
  # 
  # note:           none
  # 
  # --
  output$kandydat_crossing <- renderValueBox({
    valueBox(
      value = (xm_attendance[,length(xm_attendance)] %>% 
                 na.omit() %>% 
                 count()
               )$n,
      subtitle = tags$b("Кандидати Crossing"),
      icon = icon("moon"),
      color = "olive"
    )
  })
  
  # --
  #
  # output name:    kandydat_influx
  # output type:    (shinydashboard) valueBox
  #
  # purpose:        output the number of kandydaty
  #                 expected to join next rada
  # 
  # note:           none
  # 
  # --
  output$kandydat_influx <- renderValueBox({
    valueBox(
      value = (xm_one_rada[grep("2019", xm_one_rada$rada),] %>% filter(status == "УСП") %>% count())$n,
      subtitle = tags$b("Кандидат Influx"),
      icon = icon("search-plus"),
      color = "light-blue"
    )
  })
  
  # --
  #
  # output name:    kandydat_makeup_plot
  # output type:    (plotly) graph
  #
  # purpose:        output the composition of kandydaty
  # 
  # note:           none
  # 
  # --
  output$kandydat_makeup_plot <- renderPlotly({
    kandydat_makeup()
  })
  
  # --
  #
  # output name:    kandydat_makeup_timeseries
  # output type:    (plotly) graph
  #
  # purpose:        stacked boxplot of kandydat numbers thru the years
  # 
  # note:           none
  # 
  # --
  output$kandydat_makeup_timeseries <- renderPlotly({
    kandydat_makeup_timeseries()
  })
  
  # --
  #
  # output name:    kandydat_one_rada_fate
  # output type:    (plotly) graph
  #
  # purpose:        output the kureni that STP's who did
  #                 not receive hustky in Xmeli joined
  # 
  # note:           none
  # 
  # --
  output$kandydat_one_rada_fate <- renderPlotly({
    one_rada_plotly_proportions()
  })
  
  # --
  #
  # output name:    kandydat_one_rada_attendance
  # output type:    (plotly) graph
  #
  # purpose:        output the attendance of first
  #                 and only timers each year
  # 
  # note:           none
  # 
  # --
  output$kandydat_one_rada_attendance <- renderPlotly({
    one_rada_plotly_attendance()
  })
  
  # --
  #
  # output name:    kandydat_progress_ranks
  # output type:    (ui image) list of images of ranks
  #
  # purpose:        output the ranks in the kurin in order
  # 
  # note:           none
  # 
  # --
  output$kandydat_progress_ranks <- renderUI({
    tags$div(
      img(src = "pidx.png",
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = "pryx.png",
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = "dzura.png",
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = "mk.png",
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = "sichovyk.png",
          width = DEF_WIDTH,
          height = DEF_HEIGHT)
    )
  })
  
  # --
  #
  # output name:    kandydat_progress_rada_img
  # output type:    (ui image) list of images of future ranks
  #
  # purpose:        output the prediction for dates of promotions
  # 
  # note:           none
  # 
  # --
  output$kandydat_progress_rada_img <- renderUI({
    tags$div(
      img(src = get_rada_img(input$kandydat_progress_predict_selection, "pidx"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$kandydat_progress_predict_selection, "pryx"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$kandydat_progress_predict_selection, "dzura"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$kandydat_progress_predict_selection, "mk"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$kandydat_progress_predict_selection, "sichovyk"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT)
    )
  })
  
  # ---------- sichovyk page ---------- #
  
  # --
  #
  # output name:    sichovyk_boxplot_timeseries
  # output type:    (plotly) graph
  #
  # purpose:        output a boxplot of range of ages of 
  #                 povni through the years
  # 
  # note:           none
  # 
  # --
  output$sichovyk_boxplot_timeseries <- renderPlotly({
    povnyj_ages_boxplot()
  })
  
  # --
  #
  # output name:    sichovyk_density_timeseries
  # output type:    (ggplot2) graph
  #
  # purpose:        output a density plot of age 
  #                 distribution through each year
  # 
  # note:           none
  # 
  # --
  output$sichovyk_density_timeseries <- renderPlot({
    povnyj_ages_density()
  })
  
  # --
  #
  # output name:    sichovyk_nich_distributions
  # output type:    (plotly) graph
  #
  # purpose:        output the makeup of povnyj by nich year
  # 
  # note:           none
  # 
  # --
  output$sichovyk_nich_distributions <- renderPlotly({
    sichovyk_nich_plotly_proportions() 
  })
  
  # ---------- dyadko page ---------- #
  
  # --
  #
  # output name:    dyadko_tree
  # output type:    (collapsibleTree) tree
  #
  # purpose:        output the dyadko tree diagram
  # 
  # note:           none
  # 
  # --
  output$dyadko_tree <- renderCollapsibleTree({ render_dyadko_tree() })
  
}
