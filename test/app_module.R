
library(shiny)
library(shinyjs)
library(DT)
library(dplyr)

library(shinyfilterset)

set.seed(1)
mtcars$binary1 <- sample(c(TRUE,FALSE),nrow(mtcars),replace=TRUE)
mtcars$binary2 <- sample(c(TRUE,FALSE),nrow(mtcars),replace=TRUE)


my_filters <- shinyfilterset(data = mtcars,
                             
                             data_filter("drat","slider"),
                             data_filter("disp", "numeric_range"),
                             data_filter("gear", "picker"),
                             data_filter("cyl", "numeric_min"),
                             data_filter("binary1", "switch", updates = FALSE),
                             data_filter("binary2", "checkboxes", updates = FALSE,
                                         options = list(choices = c("Ja" = TRUE, "Nee" = FALSE), 
                                                        selected = c(TRUE,FALSE), inline = TRUE))
               
)



testmoduleUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
      fluidRow(
        column(4, uiOutput(ns("div_my_filters"))),
        column(4, 
               tags$br(),
               tags$br(),
               actionButton(ns("btn_reset_filters"), 
                            "Reset", 
                            icon = icon("refresh", lib = "glyphicon"), 
                            class = "btn btn-primary"),
               
               tags$h4("Used filters"),
               textOutput(ns("filterused")),
               tags$h4("Save"),
               textInput(ns("txt_save"), "Name filter"),
               shinyjs::disabled(
                 actionButton(ns("btn_save"), "Save")
               ),
               tags$h4("Load"),
               selectInput(ns("sel_load"), "Select filter", choices = dir("data", full.names=TRUE)),
               actionButton(ns("btn_load"), "Load")
               
        )
        
      ),
      fluidRow(
        tags$hr(),
        uiOutput(ns("data_out"))
      )
    )

  
}

testmodule <- function(input, output, session){ 
  
  output$div_my_filters <- renderUI({
    input$btn_reset_filters
    my_filters$ui(ns = session$ns)
  })

  # observeEvent(input$btn_reset_filters, {
  #   my_filters$reset_all()
  # })
  
  observeEvent(input$btn_load, {
    
    fils <- readRDS(input$sel_load)
    my_filters$load(fils)
    
  })
  
  data_filtered <- reactive({
    my_filters$apply(mtcars)
  })
    
  output$filterused <- renderText({
    
    fils <- my_filters$used_filters()
    paste(names(fils), collapse=", ")
    
  })
  
  output$data_out <- renderUI({
    
    tagList(  
      tags$span(nrow(data_filtered()), style = "font-size: 10em;"),
      tags$span(" rijen", style = "font-size: 2em;")
    )
    
  })
  
  observe({
    toggleState("btn_save", input$txt_save != "")
  })
  
  observe({
    toggleState("btn_load", input$sel_load != "")
  })
  
  observeEvent(input$btn_save, {
    
    fils <- my_filters$used_filters()
    saveRDS(fils, paste0(file.path("data", input$txt_save), ".rds"))
    
  })
  
}

ui <- fluidPage(
  testmoduleUI("test1")
)

server <- function(input, output, session){
  
  callModule(testmodule, "test1")
}

shinyApp(ui, server)

