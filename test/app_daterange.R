library(shiny)
library(shinyfilterset)
library(dplyr)

filter_period <- function(data, value){
  
  begin <- as.Date(value[1])
  eind <- as.Date(value[2])
  
  data1 <- dplyr::filter(data, 
                         between(DD_BEGIN, begin, eind) | 
                           between(DD_EIND, begin, eind))
  
  data2 <- dplyr::filter(data,
                         DD_BEGIN <= begin & DD_EIND >= eind
  )
  
  rbind(data1, data2)
}



dataset <- data.frame(DD_BEGIN = seq.Date(as.Date("2019-1-1"), by = 7, length = 50),
                      DD_EIND = seq.Date(as.Date("2019-1-15"), by = 7, length = 50),
                      value = runif(50))


myfilter <- shinyfilterset(
  data_filter("col1", filter_ui = "date_range", static = TRUE,
              filter_function = filter_period,
              options = list(start = "2019-01-01", end = "2019-12-24"))
)


ui <- fluidPage(
  myfilter$ui(),
  tags$hr(),
  textOutput("txt1")
)

server <- function(input, output, session) {
  
  data_filtered <- reactive({
    myfilter$apply(dataset)
  })  
  
  data_n <- reactive({
    nrow(data_filtered())
  })
  
  
  output$txt1 <- renderText({
    data_n()
  })
  
}

shinyApp(ui, server)


