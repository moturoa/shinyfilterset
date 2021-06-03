

library(shiny)
library(shinyfilterset)
library(glue)
library(lgrdata)
library(shinyjs)
data(cereals)

cereals$datum_wijziging <- as.Date("2021-05-21")

library(shintoshiny)

.def <- yaml::read_yaml("test/cereal_filters.yml")$filters


cereal_filters <- shinyfilterset(
  updates_on_last_use = FALSE,
  data = cereals,
  .list = shinyfilterset::from_list_definition(.def)
)



ui <- fluidPage(
  
  useShinyjs(),
  shintoshiny_dependencies(),
  
  fluidRow(
    column(6, 
           uiOutput("cereal_filters"),
           actionButton("btn_reset_filters", "Reset", class = "btn-primary"),
           
           tags$hr(),
           verbatimTextOutput("used")
           
           
    ),
    column(6,
           uiOutput("cereal_rows"),
           
           tags$h4("Gefilterde data"),
           tableOutput("cereal_filtered")
    )
  )
)


server <- function(input, output, session) {
  
  cereal_filters$monitor()
  
  data_filtered <- reactive({
    
    cereal_filters$apply(cereals)
    
  })
  
  output$used <- renderPrint({
    
    cereal_filters$used_filters2()()  
    
  })
  
  
  output$cereal_filters <- renderUI({
    
    cereal_filters$ui()
    
  })
  
  
  observeEvent(input$btn_reset_filters,  {
    
    cereal_filters$reset_all()
    
  })
  
  
  output$cereal_rows <- renderUI({
    tags$h2(paste("Aantal rijen: ", nrow(data_filtered())))
  })
  
  
  
  output$cereal_filtered <- renderTable({
    data_filtered()[,c("Manufacturer","calories","sodium","protein","sugars")]
  })
  
  
}

shinyApp(ui, server)

