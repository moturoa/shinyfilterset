
library(shiny)
library(shinyjs)
library(DT)
library(dplyr)

library(shinyfilterset)

set.seed(1)
mtcars$binary1 <- sample(c(TRUE,FALSE),nrow(mtcars),replace=TRUE)
mtcars$binary2 <- sample(c(TRUE,FALSE),nrow(mtcars),replace=TRUE)

my_filters <- shinyfilterset(
  
  data_filter(ui_section = 1, 
              column_data = mtcars$drat, 
              column_name = "drat", 
              filter_ui = "slider", 
              options = list(label = "Select drat value", ticks = FALSE)),
  data_filter(ui_section = 1,
              column_data = mtcars$disp, 
              column_name = "disp", 
              filter_ui = "numeric_range", 
              options = list(label = "Select disp value")),
  data_filter(ui_section = 1, 
              column_data = mtcars$gear, 
              column_name = "gear", 
              filter_ui = "select",
              options = list(label = "Select gear")),
  data_filter(ui_section = 2,
              column_data = mtcars$cyl, 
              column_name = "cyl", 
              filter_ui = "numeric_min",
              options = list(label = "Select cyl")),
  data_filter(ui_section = 2, 
              column_name = "binary1", 
              filter_ui = "switch", 
              options = list(status = "primary")),
  data_filter(ui_section = 2,
              column_data = mtcars$binary2, 
              column_name = "binary2", 
              filter_ui = "checkboxes",
              options = list(choices = c("Ja" = TRUE, "Nee" = FALSE), selected = TRUE, inline = TRUE))
)



ui <- fluidPage(
  useShinyjs(),
  
  fluidRow(
    column(4, uiOutput("div_my_filters_1")),
    column(4, uiOutput("div_my_filters_2")),
    column(4, 
           tags$br(),
           tags$br(),
           actionButton("hide_filters", "Toggle", 
                        icon = icon("sort", lib = "glyphicon"), class = "btn btn-primary"),
           actionButton("reset_filters", "Reset", 
                        icon = icon("refresh", lib = "glyphicon"), class = "btn btn-primary"),
           
           actionButton("updateslider", "Update")
    )
  ),
  
  tags$hr(),
  uiOutput("data_out")
)

server <- function(input, output, session){ 
  
  rv <- reactiveValues(
    data_filtered = NULL
  )
  
  observe({
    input$reset_filters
    output$div_my_filters_1 <- renderUI(my_filters$ui(section = 1))
    output$div_my_filters_2 <- renderUI(my_filters$ui(section = 2))
  })
  
  observe({
    my_filters$reactive(input)
    rv$data_filtered <- my_filters$apply(mtcars)
  })
  
  observeEvent(input$updateslider, {
    my_filters$update(session, rv$data_filtered, input)
  })
  
  output$data_out <- renderUI({
    
    tagList(  
      tags$span(nrow(rv$data_filtered), style = "font-size: 10em;"),
      tags$span(" rijen", style = "font-size: 2em;")
    )
    
  })
  
  observeEvent(input$hide_filters, {
    shinyjs::toggle(my_filters$id, anim = TRUE)
  })
  
  
}

shinyApp(ui, server)


