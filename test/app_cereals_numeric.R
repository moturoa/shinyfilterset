
library(shiny)
library(shinyfilterset)

library(lgrdata)
data(cereals)


source("../R/utils.R")
cereals$rating_search <- numeric_breaks_labels(cereals$rating, c(20, 40, 60, 80))

# Filters: start with nothing, build up
updates <- TRUE

slider <- function(column, data = cereals){
  data_filter(column_data = data[[column]],
              column_name = column,
              filter_ui = "slider",
              updates = TRUE
  )
}

cereal_filters <- shinyfilterset(
  
  slider("calories"),
  slider("sodium"),
  slider("protein"),
  slider("sugars"),
  slider("rating"),
  slider("vitamins")
  
  
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
    print("applying filters")
    cereal_filters$apply(cereals)
  })
  
  
  output$cereal_rows <- renderUI({
    tags$h2(paste("Aantal rijen: ", nrow(data_filtered())))
  })
  
  output$cereal_filters <- renderUI({
    input$btn_reset_filters
    print("filter UI")
    cereal_filters$ui()
  })
  
  output$cereal_filtered <- renderTable({
    
    data_filtered()[,c("Manufacturer","calories","sodium","protein","sugars")]
    
  })
  
  
}

shinyApp(ui, server)