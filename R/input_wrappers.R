


slider_input <- function(id, self){
  
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- self$range
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), 
               options, 
               list(min = self$range[1], max = self$range[2]))
  
  list(ui = do.call(shiny::sliderInput, options),
       value = options$value)
  
}


numericrange_input <- function(id, self){
  
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- self$range
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), 
               options)
  
  list(ui = do.call(shinyWidgets::numericRangeInput, options),
       value = options$value)
}


select_input <- function(id, self, type = c("select","picker")){
  
  type <- match.arg(type)
  input_field <- switch(type,
                        select = shiny::selectInput,
                        picker = shinyWidgets::pickerInput
  )
  
  options <- self$options
  
  if(!("selected" %in% names(options))){
    
    options$selected <- self$unique
    
    if(!is.null(self$all_choice)){
      options$selected <- self$all_choice
    }
  }
  if(!("multiple" %in% names(options))){
    options$multiple <- TRUE
  }
  if(!("choices" %in% names(options))){
    options$choices <- c(self$all_choice, self$unique)
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), 
               options)
  
  list(ui = do.call(input_field, options),
       value = options$selected)
  
}


checkboxes_input <- function(id, self){
  
  options <- self$options
  
  if(!("selected" %in% names(options))){
    options$selected <- self$unique
  }
  if(!("choices" %in% names(options))){
    options$choices <- self$unique
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), 
               options)
  
  list(ui = do.call(shiny::checkboxGroupInput, options),
       value = options$selected)
  
}



numeric_input <- function(id, self, type = c("min", "max")){
  
  type <- match.arg(type)
  type_index <- switch(type,
                       min = 1,
                       max = 2)
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- self$range[type_index]
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), 
               options, 
               list(min = self$range[1], max = self$range[2]))
  
  list(ui = do.call(shiny::numericInput, options),
       value = options$value)
}


binary_input <- function(id, self, type = "switch"){

  options <- self$options
  type <- match.arg(type)
  
  if(!("value" %in% names(options))){
    options$value <- FALSE
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), 
               options)
  
  list(ui = do.call(shinyWidgets::materialSwitch, options),
       value = options$value)

}


