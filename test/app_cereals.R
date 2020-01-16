
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
              na_value = "Onbekend",
              updates = TRUE,
              options = list(selected = NULL)
  )
}

cereal_filters_back <- shinyfilterset(all_data_on_null = TRUE, # default TRUE
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
    rv$data_filtered <- cereal_filters_back$apply(cereals)
    
    cereal_filters_back$update(session, rv$data_filtered, input)
  })
  
  observe({
    cereal_filters_back$reactive(input)
    cereal_filters_back$update(session, rv$data_filtered, input)
  })

  output$cereal_rows <- renderText({
    paste("N rows: ", nrow(rv$data_filtered))
  })
  
  output$cereal_filters <- renderUI({
    input$btn_reset_filters
    
    cereal_filters_back$ui()
  })
  
  output$cereal_filtered <- renderTable({
    
    rv$data_filtered[,c("Manufacturer","calories","sodium","protein","sugars")]
    
  })
  
  
}

shinyApp(ui, server)