set_slider <- function(session, id, self, value){
  
  shiny::updateSliderInput(session, id, value = value)
  
}

set_select <- function(session, id, self, value){
  
  if(is.null(value))value <- character(0)
  #print(paste(id, value))
  shiny::updateSelectInput(session, id, selected = value)
  
}

set_checkboxes <- function(session, id, self, value){
  
  if(is.null(value))value <- character(0)
  
  shiny::updateCheckboxGroupInput(session, id, selected = value)
  
}

set_picker <- function(session, id, self, value){
  
  if(is.null(value))value <- character(0)

  shinyWidgets::updatePickerInput(session, id, selected = value)
  
}

set_numeric_min <- function(session, id, self, value){
  
  shiny::updateNumericInput(session, id, value = value)
  
}


set_numeric_max <- function(session, id, self, value){
  
  shiny::updateNumericInput(session, id, value = value)
  
}

set_date_range <- function(session, id, self, value){
  
  shiny::updateDateRangeInput(session, id, 
                              start = value[1],
                              end = value[2])
  
}

set_range <- function(session, id, self, value){
  
  # !! apparently updateNumericRangeInput has a bug that we have to re-set the label.
  shinyWidgets::updateNumericRangeInput(session, id, label = self$label, value = value)
  
}

set_material <- function(session, id, self, value){

  shinyWidgets::updateMaterialSwitch(session, id, value = value)
  
}

