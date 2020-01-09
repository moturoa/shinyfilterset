library(shiny)
library(dplyr)
library(shinyWidgets)

make_select_choices <- function(data, column, label_n = TRUE){
  
  data_column <- data[[column]]
  if(is.null(data_column) || nrow(data) == 0){
    return(NULL)
  }
  
  choices <- sort(unique(data[[column]]))
  
  if(label_n){
    tab <- table(data[[column]])
    labels <- sort(paste0(names(tab), " (", tab, ")"))  
  } else {
    labels <- choices
  }
  names(choices) <- labels
  
return(choices)
}


data <- mtcars
data$make <- sapply(strsplit(rownames(data), " "), "[[", 1)

ui <- fluidPage(
  
  pickerInput("sel1", "sel1", 
              choices = make_select_choices(data, "make"), 
              selected = make_select_choices(data, "make"),
              multiple = TRUE),
  sliderInput("slide1", "slide1", min=min(data$drat),max=max(data$drat),
              value = c(min(data$drat), max(data$drat))
              ),
  uiOutput("ui1")
  
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data_filtered = data
  )
  
  output$ui1 <- renderUI({
    h4(as.character(nrow(rv$data_filtered)))
  })
  
  observe({
    rv$data_filtered <- filter(rv$data_filtered, make %in% input$sel1) %>%
      filter(drat <= input$slide1[2], drat >= input$slide1[1])
  })
  
  observe({
    
    updatePickerInput(session, "sel1", 
                      choices = make_select_choices(rv$data_filtered, "make"),
                      selected = input$sel1)
  })
  
}

shinyApp(ui, server)