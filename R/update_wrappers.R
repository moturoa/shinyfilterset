update_slider <- function(session, id, self, data, input){
  
  val <- range(data, na.rm=TRUE)
  
  shiny::updateSliderInput(session, id, value = val)
  
}


update_select <- function(session, id, self, data, input){
  
  val <- unique(data)
  
  oldval <- input[[id]]

  if(!is.null(oldval)){
    shiny::updateSelectInput(session, id, choices = val, selected = oldval)
  }
  
  
}

update_checkboxes <- function(session, id, self, data, input){
  
  val <- unique(data)
  
  oldval <- input[[id]]
  
  if(!is.null(oldval)){
    shiny::updateCheckboxGroupInput(session, id, choices = val, selected = oldval)
  }

  
}

update_picker <- function(session, id, self, data, input){
  
  val <- unique(data)
  
  shinyWidgets::updatePickerInput(session, id, choices = val)
  
}

update_numeric_min <- function(session, id, self, data, input){
  
  val <- min(data, na.rm=TRUE)
  
  shiny::updateNumericInput(session, id, value = val)
  
}


update_numeric_max <- function(session, id, self, data, input){
  
  val <- max(data, na.rm=TRUE)
  
  shiny::updateNumericInput(session, id, value = val)
  
}


update_range <- function(session, id, self, data, input){
  
  val <- range(data, na.rm=TRUE)
  
  # !! apparently updateNumericRangeInput has a bug that we have to re-set the label.
  shinyWidgets::updateNumericRangeInput(session, id, label = self$label, value = val)
  
}

update_material <- function(session, id, self, data, input){
  
  # do nothing
  # different logic
  
}
