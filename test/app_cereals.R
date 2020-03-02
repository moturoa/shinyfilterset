
library(shiny)
library(shinyfilterset)
library(glue)
library(lgrdata)
data(cereals)

cereals$rating_search <- numeric_breaks_labels(cereals$rating, c(20, 40, 60, 80))




filter_span <- function(x){
  span(x, 
       style = glue("padding: 8px;",
                    "background-color: #3C8DBC;",
                    "color: white;",
                    "font-weight: 500;",
                    "font-size: 0.9em;",
                    "border-radius: 10px;"))
}

cereal_filters <- shinyfilterset( 
  data = cereals,
  updates = TRUE,
  n_label = FALSE,
  
  data_filter("Manufacturer", "picker"),
  data_filter("calories", "picker"),
  data_filter("protein", "picker"),
  data_filter("rating_search", "picker")
  
)


ui <- fluidPage(
  fluidRow(
    column(6, 
           uiOutput("cereal_filters"),
           actionButton("btn_reset_filters","Reset", class="btn-primary"),
           tags$hr(),
           actionButton("btn_save_filter", "Save"),
           actionButton("btn_load_filter", "Load"),
           tags$hr(),
           actionButton("browse","browser()"),
           actionButton("reset_calories", "Reset calories")
           
           ),
    column(6,
           uiOutput("cereal_rows"),
           hr(),
           uiOutput("ui_used_filters"),
           hr(),
           
           tags$h4("Gefilterde data"),
           tableOutput("cereal_filtered")
           )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$browse,browser())
  cereal_filters$monitor()
  
  data_filtered <- reactive({
    
    print("apply")
    cereal_filters$apply(cereals)
    
  })
  

  # Lijst van gebruikte filters
  output$ui_used_filters <- renderUI({
    
    cereal_filters$reactive()
    fils_used <- names(cereal_filters$used_filters())
    
    if(length(fils_used)){
      tagList(
        tags$span("Filters: "),
        lapply(fils_used, filter_span)
      )  
    }
    
  })
  
  observeEvent(input$reset_calories, {
    
    cereal_filters$reset("calories")
    
    
  })
  
  output$cereal_rows <- renderUI({
    tags$h2(paste("Aantal rijen: ", nrow(data_filtered())))
  })
  
  output$cereal_filters <- renderUI({
    input$btn_reset_filters
    print("ui")
    cereal_filters$ui()
  })
  
  output$cereal_filtered <- renderTable({
    data_filtered()[,c("Manufacturer","calories","sodium","protein","sugars")]
  })
  
  observeEvent(input$btn_save_filter, {
    saveRDS(cereal_filters$used_filters(), "lastfilters.rds")
  })
  
  observeEvent(input$btn_load_filter, {
    fils <- readRDS("lastfilters.rds")
    #print("load")
    cereal_filters$load(fils)
  })
  
  
  
}

shinyApp(ui, server)