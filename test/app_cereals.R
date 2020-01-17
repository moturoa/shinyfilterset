
library(shiny)
library(shinyfilterset)

library(lgrdata)
data(cereals)

# Filters: start with nothing, build up
updates <- TRUE

picker <- function(column, data = cereals){
  data_filter(column_data = data[[column]],
              column_name = column,
              filter_ui = "picker",
              updates = TRUE,
              n_label = TRUE,
              options = list(selected = NULL)
  )
}

cereal_filters <- shinyfilterset(
  all_data_on_null = TRUE, # default TRUE
  
  picker("Manufacturer"),
  picker("calories"),
  picker("sodium"),
  picker("protein"),
  picker("sugars")
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