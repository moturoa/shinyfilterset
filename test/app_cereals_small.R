
library(shiny)
library(shinyfilterset)
library(glue)
library(lgrdata)
data(cereals)


library(shinyjs)

cereals$rating_search <- numeric_breaks_labels(cereals$rating, c(20, 40, 60, 80))

cereals$boolean <- sample(c(TRUE,FALSE), nrow(cereals), replace = TRUE)

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
  updates_on_last_use = FALSE, 
  n_label = TRUE,
  
  data_filter("Manufacturer", "select"),
  data_filter("calories", "select"),
  data_filter("protein", "select"),
  data_filter("Manufacturer", "virtualsearch"),
  data_filter("rating", "numeric_range", options = list(value=c(0,100)), updates = FALSE, static = TRUE),
  data_filter("boolean", "checkboxes", updates = FALSE, static = TRUE, 
              options = list(choices = c("Ja" = TRUE, "Nee" = FALSE), selected = c(TRUE,FALSE), inline = TRUE))
  
)


ui <- fluidPage(
  
  useShinyjs(),
  
  fluidRow(
    column(6, 
           uiOutput("cereal_filters"),
           actionButton("btn_reset_filters","Reset", class="btn-primary")
           
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
  
  
  cereal_filters$monitor()
  
  
  data_filtered <- reactive({

    #print(paste("apply", sample(1:1000,1)))
    cereal_filters$apply(cereals)
    
  })
  

  output$cereal_filters <- renderUI({
    
    cereal_filters$ui()
    
  })
  
  
  observeEvent(input$btn_reset_filters,  {
    cereal_filters$reset_all()
  })
  
  
  # output$cereal_filters <- renderUI({
  #   
  #   input$btn_reset_filters
  #   uf <- isolate(names(cereal_filters$used_filters()))
  #   if(input$btn_reset_filters > 0 && length(uf)){
  #     cereal_filters$ui()
  #   }
  #   
  # })
  
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
  
  
  output$cereal_rows <- renderUI({
    tags$h2(paste("Aantal rijen: ", nrow(data_filtered())))
  })
  
  
  
  output$cereal_filtered <- renderTable({
    data_filtered()[,c("Manufacturer","calories","sodium","protein","sugars")]
  })
  
  
  
  
}

shinyApp(ui, server)
