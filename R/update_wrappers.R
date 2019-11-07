update_slider <- function(session, self, data, input){
  
  val <- range(data, na.rm=TRUE)
  updateSliderInput(session, self$ns_id, value = val)
  
}

update_slider <- function(session, self, data, input){
  
  val <- range(data, na.rm=TRUE)
  updateSliderInput(session, self$ns_id, value = val)
  
}

update_select <- function(session, self, data, input){
  
  val <- unique(data)
  
  # !! not a bug (?), but you have to set `selected` when updating the choices
  if(!is.null(self$ns_id)){
    oldval <- input[[self$ns_id]]
    if(!is.null(oldval)){
      updateSelectInput(session, self$ns_id, choices = val, selected = oldval)
    }
  }
  
}

update_picker <- function(session, self, data, input){
  
  val <- unique(data)
  shinyWidgets::updatePickerInput(session, self$ns_id, choices = val)
  
}

update_numeric_min <- function(session, self, data, input){
  
  val <- min(data, na.rm=TRUE)
  updateNumericInput(session, self$ns_id, value = val)
  
}


update_numeric_max <- function(session, self, data, input){
  
  val <- max(data, na.rm=TRUE)
  updateNumericInput(session, self$ns_id, value = val)
  
}


update_range <- function(session, self, data, input){
  
  val <- range(data, na.rm=TRUE)
  
  # !! apparently updateNumericRangeInput has a bug that we have to re-set the label.
  shinyWidgets::updateNumericRangeInput(session, self$ns_id, label = self$label, value = val)
  
}

update_material <- function(session, self, data, input){
  
  # do nothing
  # different logic
  
}
