
library(shiny)
library(shinyfilterset)

library(lgrdata)
data(cereals)


cereal_filters <- shinyfilterset(data = cereals,
  
     data_filter("calories", "slider"),
     data_filter("sodium", "slider"),
     data_filter("protein", "slider"),
     data_filter("sugars", "slider"),
     data_filter("rating", "slider"),
     data_filter("vitamins", "slider")

)


ui <- fluidPage(
  fluidRow(
    column(6, 
           uiOutput("cereal_filters"),
           actionButton("btn_reset_filters", "Reset", class="btn-primary")
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
    cereal_filters$ui()
  })
  
  observeEvent(input$btn_reset_filters, {
    cereal_filters$reset_all()
  })
  
  output$cereal_filtered <- renderTable({
    
    data_filtered()[,c("Manufacturer","calories","sodium","protein","sugars")]
    
  })
  
  
}

shinyApp(ui, server)