set_slider <- function(session, self, value){
  
  updateSliderInput(session, self$ns_id, value = value)
  
}

set_select <- function(session, self, value){
  
  updateSelectInput(session, self$ns_id, selected = value)
  
}

set_checkboxes <- function(session, self, value){
  
  shiny::updateCheckboxGroupInput(session, self$ns_id, selected = value)
  
}

set_picker <- function(session, self, value){
  
  shinyWidgets::updatePickerInput(session, self$ns_id, choices = value)
  
}

set_numeric_min <- function(session, self, value){
  
  updateNumericInput(session, self$ns_id, value = value)
  
}


set_numeric_max <- function(session, self, value){
  
  updateNumericInput(session, self$ns_id, value = value)
  
}


set_range <- function(session, self, value){
  
  # !! apparently updateNumericRangeInput has a bug that we have to re-set the label.
  shinyWidgets::updateNumericRangeInput(session, self$ns_id, label = self$label, value = value)
  
}

set_material <- function(session, self, value){

  shinyWidgets::updateMaterialSwitch(session, self$ns_id, value = value)
  
}

