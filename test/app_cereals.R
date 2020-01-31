
library(shiny)
library(shinyfilterset)

library(lgrdata)
data(cereals)

cereals$rating_search <- numeric_breaks_labels(cereals$rating, c(20, 40, 60, 80))


cereal_filters <- shinyfilterset(  #data = cereals,
  
  data_filter("Manufacturer", "picker"),
  data_filter("calories", "picker"),
  data_filter("protein", "picker"),
  data_filter("rating_search", "picker")
  
)


ui <- fluidPage(
  fluidRow(
    column(6, 
           uiOutput("cereal_filters"),
           actionButton("btn_reset_filters","Reset", class="btn-primary")
           ),
    column(6,
           uiOutput("cereal_rows"),
           hr(),
           tags$h4("Gefilterde data"),
           tableOutput("cereal_filtered")
           )
  )
)

server <- function(input, output, session) {
  
  
  data_filtered <- reactive({
    cereal_filters$apply(cereals)
  })
  
  
  output$cereal_rows <- renderUI({
    tags$h2(paste("Aantal rijen: ", nrow(data_filtered())))
  })
  
  output$cereal_filters <- renderUI({
    input$btn_reset_filters
    cereal_filters$ui()
  })
  
  output$cereal_filtered <- renderTable({
    
    data_filtered()[,c("Manufacturer","calories","sodium","protein","sugars")]
    
  })
  
  
}

shinyApp(ui, server)