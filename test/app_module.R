
library(shiny)
library(shinyjs)
library(DT)
library(dplyr)

library(shinyfilterset)

set.seed(1)
mtcars$binary1 <- sample(c(TRUE,FALSE),nrow(mtcars),replace=TRUE)
mtcars$binary2 <- sample(c(TRUE,FALSE),nrow(mtcars),replace=TRUE)


my_filters <- shinyfilterset(
  
  filter_section(1,
                 
                 tags$h4("Filters (1)"),
                 
                 data_filter(column_data = mtcars$drat, 
                             column_name = "drat", 
                             filter_ui = "slider", 
                             updates = TRUE,
                             options = list(label = "Select drat value", ticks = FALSE)),
                 data_filter(column_data = mtcars$disp, 
                             column_name = "disp", 
                             filter_ui = "numeric_range", 
                             updates = TRUE,
                             options = list(label = "Select disp value")),
                 data_filter(column_data = mtcars$gear, 
                             column_name = "gear", 
                             filter_ui = "select",
                             updates = TRUE,
                             options = list(label = "Select gear"))               
                 
  ),
  filter_section(2,
                 tags$h4("Filters (2)"),
                 
                 data_filter(column_data = mtcars$cyl, 
                             column_name = "cyl", 
                             filter_ui = "numeric_min",
                             updates = FALSE,
                             options = list(label = "Select cyl")),
                 data_filter(column_name = "binary1", 
                             filter_ui = "switch", 
                             updates = FALSE,
                             options = list(status = "primary")),
                 data_filter(column_data = mtcars$binary2, 
                             column_name = "binary2", 
                             filter_ui = "checkboxes",
                             updates = FALSE,
                             options = list(choices = c("Ja" = TRUE, "Nee" = FALSE), 
                                            selected = c(TRUE,FALSE), inline = TRUE))             
                 
  )
  
  
)



testmoduleUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
      fluidRow(
        column(4, uiOutput(ns("div_my_filters_1"))),
        column(4, uiOutput(ns("div_my_filters_2"))),
        column(4, 
               tags$br(),
               tags$br(),
               actionButton(ns("btn_reset_filters"), "Reset", 
                            icon = icon("refresh", lib = "glyphicon"), class = "btn btn-primary"),
               
               actionButton(ns("updateslider"), "Update"),
               actionButton(ns("browse"),"browser()"),
               actionButton(ns("btn_reset_filters2"), "Reset (2)", 
                            icon = icon("refresh", lib = "glyphicon")),
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
  
  rv <- reactiveValues(
    data_filtered = NULL,
    last_filter = NULL
  )
  
  observe({
    input$btn_reset_filters
    
    output$div_my_filters_1 <- renderUI(my_filters$ui(ns = session$ns, section = 1))
    output$div_my_filters_2 <- renderUI(my_filters$ui(ns = session$ns, section = 2))
  })
  
  observeEvent(input$btn_load, {
    
    fils <- readRDS(input$sel_load)
    
    my_filters$reset(session, input)
    for(i in seq_along(fils)){
      my_filters$set_value(session, input, names(fils)[i], fils[[i]])
    }
    
  })
  
  observeEvent(input$btn_reset_filters2, {
    my_filters$reset(session, input)
  })
  
  observe({
    
    rv$data_filtered <- my_filters$apply(mtcars)
    
  })
  
  
  output$filterused <- renderText({
    
    fils <- my_filters$used_filters(input)
    paste(names(fils), collapse=", ")
    
  })
  
  output$data_out <- renderUI({
    
    tagList(  
      tags$span(nrow(rv$data_filtered), style = "font-size: 10em;"),
      tags$span(" rijen", style = "font-size: 2em;")
    )
    
  })
  
  
  observeEvent(input$browse, browser())
  
  
  observe({
    toggleState("btn_save", input$txt_save != "")
  })
  
  observe({
    toggleState("btn_load", input$sel_load != "")
  })
  
  observeEvent(input$btn_save, {
    
    fils <- my_filters$used_filters(input)
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

