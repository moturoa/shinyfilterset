
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
                                            selected = c(TRUE,FALSE), inline = TRUE)),
                 data_filter(column_data = mtcars$gear, 
                             column_name = "gear", 
                             filter_ui = "select",
                             updates = FALSE,
                             all_choice = "All gears selected",
                             options = list(label = "Select gear", multiple = TRUE))               
                 
    )
  
  
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


