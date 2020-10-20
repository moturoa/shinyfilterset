
library(shiny)
library(shinyfilterset)
library(glue)
library(lgrdata)
data(cereals)

cereals$rating_search <- numeric_breaks_labels(cereals$rating, c(20, 40, 60, 80))


cereals$all_na <- NA


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
  
  data_filter("Manufacturer", "select"),
  data_filter("calories", "select"),
  data_filter("protein", "select"),
  data_filter("rating_search", "select"),
  data_filter("rating", "slider", options = list(min = 0, max = 100)),
  data_filter("all_na", "select")
  
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
           actionButton("btn_set","set manufacturer N")
           
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
  
  observeEvent(input$browse, browser())
  cereal_filters$monitor()
  
  observeEvent(input$btn_set, {
    cereal_filters$filters[["Manufacturer"]]$reset()
  })
  
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