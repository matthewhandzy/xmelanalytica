# Даїшник
# 2019
# server.R

library(shiny)

# import important functions and dataframes
source("utils/xmel_utils.R", local = TRUE)
import_xmel_data_local()

DEF_WIDTH = 110
DEF_HEIGHT = 150

server <- function(input, output, session) {
  
  ##### sich #####
  
  output$sich_map <- renderPlotly({ sich_get_map(input$sich_map_choices) })
  
  # create shiny tree
  output$dyadko_tree <- renderCollapsibleTree({ render_dyadko_tree() })
  
  ##### kandydaty #####
  # progress predictor data table output
  output$progress_predict <- renderDataTable({ 
    project_future(split_name(input$progress_predict_selection)[1], split_name(input$progress_predict_selection)[2])
  })
  
  # render 4-panel icons for ranks
  output$progress_ranks <- renderUI({
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
  
  # renders 4-panel prediction for a specific kandydat
  output$progress_rada_img <- renderUI({
    tags$div(
      img(src = get_rada_img(input$progress_predict_selection, "pidx"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$progress_predict_selection, "pryx"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$progress_predict_selection, "dzura"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$progress_predict_selection, "mk"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT),
      img(src = get_rada_img(input$progress_predict_selection, "sichovyk"),
          width = DEF_WIDTH,
          height = DEF_HEIGHT)
    )
  })
  
  # one rada fate plotly chart
  output$one_rada_fate <- renderPlotly({
    one_rada_plotly_proportions()
  })
  
  # one rada year plotly chart
  output$one_rada_attendance <- renderPlotly({
    one_rada_plotly_attendance()
  })
  
  ##### sichovyk #####
  output$nich_distributions <- renderPlotly({
    sichovyk_nich_plotly_proportions() 
  })
  
}