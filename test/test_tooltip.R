
library(prompter)

add_tooltip <- function(txt, hlp){
  
  tags$span(txt, prompter::add_prompt(tags$span(softui::bsicon("info-circle-fill")), 
                            position = "top",
                            message = hlp))
  
}

library(shiny)

ui <- softui::simple_page(
  
  selectInput("sel", add_tooltip("Hallo", "Hier valt iets te lezen"), choices = letters)
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
