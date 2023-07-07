

library(shiny)
library(glue)
library(lgrdata)
library(shinyjs)
library(softui)


#library(shinyfilterset)
devtools::load_all()

data(cereals)

cereals$datum_wijziging <- as.Date("2021-05-21")
cereals$boolean <- sample(c(TRUE,FALSE), nrow(cereals), replace = TRUE)

cereals$naam <- replicate(nrow(cereals),paste(sample(letters,5), collapse=""))

cereals$year <- sample(2000:2020, nrow(cereals), replace = TRUE)

cereals$float <- runif(nrow(cereals),100,200)

library(shintoshiny)

.def <- yaml::read_yaml("test/cereal_filters.yml")$filters


cereal_filters <- shinyfilterset(
  updates_on_last_use = FALSE,
  data = cereals,
  .list = shinyfilterset::from_list_definition(.def)
)



ui <- softui::simple_page(
  
  useShinyjs(),
  shintoshiny_dependencies(),
  
  softui::box(title = "Filter test",
              
    softui::fluid_row(
      column(4, 
             uiOutput("cereal_filters"),
             actionButton("btn_reset_filters", "Reset", class = "btn-primary"),
             
             tags$hr(),
             verbatimTextOutput("used")
             
             
      ),
      column(2),
      column(6,
             uiOutput("cereal_rows"),
             
             tags$h4("Gefilterde data"),
             tableOutput("cereal_filtered")
      )
    )
  )
)


server <- function(input, output, session) {
  
  cereal_filters$monitor()
  
  data_filtered <- reactive({
    
    req(ui_done())
    print(glue::glue("[{Sys.time()}] filtering"))
    cereal_filters$apply(cereals)
    
  })
  
  output$used <- renderPrint({
    
    cereal_filters$used_filters2()()  
    
  })
  
  ui_done <- reactiveVal(NULL)
  
  output$cereal_filters <- renderUI({
    
    ui <- cereal_filters$ui()
    ui_done(TRUE)
    ui
  })
  
  
  observeEvent(input$btn_reset_filters,  {
    
    cereal_filters$reset_all()
    
  })
  
  
  output$cereal_rows <- renderUI({
    tags$h2(paste("Aantal rijen: ", nrow(data_filtered())))
  })
  
  
  
  output$cereal_filtered <- renderTable({
    data_filtered()[,c("naam","Manufacturer","calories","sodium","protein","sugars")]
  })
  
  
}

shinyApp(ui, server)

