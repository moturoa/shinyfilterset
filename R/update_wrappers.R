

update_select <- function(session, id, self, data, input){
  
  vals <- make_choices(data, self$n_label, self$sort, self$array_field, self$array_separator,
                       self$select_choices,
                       selected = input[[id]])

  shiny::updateSelectizeInput(session, id, choices = vals, selected = input[[id]], server = self$server)
  
}


update_picker <- function(session, id, self, data, input){
  
  vals <- make_choices(data, self$n_label, self$sort, self$array_field, self$array_separator,
                       self$select_choices,
                       selected = input[[id]])
  
  shinyWidgets::updatePickerInput(session, id, choices = vals, selected = input[[id]])
  
}

update_checkboxes <- function(session, id, self, data, input){
  
  vals <- make_choices(data, self$n_label, self$sort, self$array_field, self$array_separator,
                       self$select_choices,
                       selected = input[[id]])
  
  if(is.null(input[[id]])){
    sel <- NULL
  } else {
    sel <- input[[id]]
  }
  
  shiny::updateCheckboxGroupInput(session, id, choices = vals, selected = input[[id]])
  
}




update_slider <- function(session, id, self, data, input){
  
  data <- data[!is.na(data)]
  
  if(length(data)){
    val <- range(data, na.rm=TRUE)
    
    val <- c(floor_digits(val[1], self$round_digits), 
             ceiling_digits(val[2], self$round_digits))
    
    shiny::updateSliderInput(session, id, value = val)
  }
}



update_numeric_min <- function(session, id, self, data, input){
  
  data <- data[!is.na(data)]
  
  if(length(data)){
    val <- min(data, na.rm=TRUE)
    val <- round(val, digits = self$round_digits)
    shiny::updateNumericInput(session, id, value = val)
    
  }
  
}


update_numeric_max <- function(session, id, self, data, input){
  
  data <- data[!is.na(data)]
  
  if(length(data)){
    val <- max(data, na.rm=TRUE)
    val <- round(val, digits = self$round_digits)
    shiny::updateNumericInput(session, id, value = val)  
  }
  
}

update_date_range <- function(session, id, self, data, input){
  
  shiny::updateDateRangeInput(session, id, 
                              start = min(data, na.rm = TRUE),
                              end = max(data, na.rm = TRUE))
  
}


update_range <- function(session, id, self, data, input){
  
  data <- data[!is.na(data)]
  
  if(length(data)){
    val <- range(data, na.rm=TRUE)
    val <- c(floor_digits(val[1], self$round_digits), 
             ceiling_digits(val[2], self$round_digits))
  
    # !! apparently updateNumericRangeInput has a bug that we have to re-set the label.
    shinyWidgets::updateNumericRangeInput(session, id, label = self$label, value = val)
  }
}

update_material <- function(session, id, self, data, input){
  
  # do nothing
  # different logic
  
}
