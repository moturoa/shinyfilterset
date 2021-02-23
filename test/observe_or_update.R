

library(shiny)
library(tibble)
library(DT)
library(shinyjs)

ui <- fluidPage(
  
  useShinyjs(),
  
  selectInput("sel1", "Select!", choices = letters[1:20], multiple = TRUE),
  selectInput("sel2", "Select!", choices = letters[10:26], multiple = TRUE),
  
  actionButton("btn1", "CLICK"),
  
  tags$hr(),
  dataTableOutput("dt")
  
)

server <- function(input, output, session) {
  
  logs <- reactiveVal(
    tibble::tribble(~timestamp, ~id, ~from)
  )
  
  
  observeEvent(input$sel1, ignoreNULL = FALSE, {
    
    new_log <- tibble(timestamp = Sys.time(),
                      id = "sel1",
                      from = "observeEvent"
                      )
    logs(rbind(logs(), new_log))
    
  })
  
  observeEvent(input$sel2, ignoreNULL = FALSE, {
    
    new_log <- tibble(timestamp = Sys.time(),
                      id = "sel2",
                      from = "observeEvent"
    )
    logs(rbind(logs(), new_log))
    
  })
  
  
  shinyjs::onclick("sel1", {
         new_log <- tibble(timestamp = Sys.time(),
                           id = "sel1",
                           from = "click"
         )
         logs(rbind(logs(), new_log))
  })
  
  shinyjs::onclick("sel2", {
    new_log <- tibble(timestamp = Sys.time(),
                      id = "sel2",
                      from = "click"
    )
    logs(rbind(logs(), new_log))
  })
  
  
  observeEvent(input$btn1, {
    
    updateSelectInput(session, "sel1", selected = character(0))
    updateSelectInput(session, "sel2", selected = character(0))
    
    
  })
  
  output$dt <- DT::renderDT({
    logs() %>%
    datatable()
  })
  
  
}

shinyApp(ui, server)
