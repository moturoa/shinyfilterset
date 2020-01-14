
library(shiny)
library(shinyfilterset)

library(lgrdata)
data(cereals)


cereal_filters <- shinyfilterset(
  data_filter(column_data = cereals$Manufacturer, 
              column_name = "Manufacturer", 
              filter_ui = "picker", 
              updates = TRUE),
  data_filter(column_data = cereals$calories, 
              column_name = "calories", 
              filter_ui = "picker", 
              updates = TRUE),
  data_filter(column_data = cereals$sodium, 
              column_name = "sodium", 
              filter_ui = "picker", 
              updates = TRUE),
  data_filter(column_data = cereals$protein, 
              column_name = "protein", 
              filter_ui = "picker", 
              updates = TRUE),
  data_filter(column_data = cereals$sugars, 
              column_name = "sugars", 
              filter_ui = "picker", 
              updates = TRUE)
)


ui <- fluidPage(
  fluidRow(
    column(6, 
           uiOutput("cereal_filters"),
           actionButton("btn_reset_filters","Reset")
           ),
    column(6,
           textOutput("cereal_rows"),
           hr(),
           tableOutput("cereal_filtered")
           )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data_filtered = cereals
  )
  
  observe({
    rv$data_filtered <- cereal_filters$apply(cereals)
    
    cereal_filters$update(session, rv$data_filtered, input)
  })
  
  output$cereal_rows <- renderText({
    paste("N rows: ", nrow(rv$data_filtered))
  })
  
  output$cereal_filters <- renderUI({
    input$btn_reset_filters
    
    cereal_filters$ui()
  })
  
  output$cereal_filtered <- renderTable({
    
    rv$data_filtered[,c("Manufacturer","calories","sodium","protein","sugars")]
    
  })
  
  
}

shinyApp(ui, server)