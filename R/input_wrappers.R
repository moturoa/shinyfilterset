


slider_input <- function(ns, self){
  
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- self$range
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = ns("input_element")), 
               options, 
               list(min = self$range[1], max = self$range[2]))
  
  do.call(shiny::sliderInput, options)
  
}


numericrange_input <- function(ns, self){
  
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- self$range
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = ns("input_element")), 
               options)
  
  do.call(shinyWidgets::numericRangeInput, options)
}


select_input <- function(ns, self, type = c("select","picker")){
  
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
  
  options <- c(list(inputId = ns("input_element")), 
               options)
  
  do.call(input_field, options)
  
}


checkboxes_input <- function(ns, self){
  
  options <- self$options
  
  if(!("selected" %in% names(options))){
    options$selected <- self$unique
  }
  if(!("choices" %in% names(options))){
    options$choices <- self$unique
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = ns("input_element")), 
               options)
  
  do.call(shiny::checkboxGroupInput, options)
  
}



numeric_input <- function(ns, self, type = c("min", "max")){
  
  type <- match.arg(type)
  type_index <- switch(type,
                       min = 1,
                       max = 2)
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- self$range[type_index]
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = ns("input_element")), 
               options, 
               list(min = self$range[1], max = self$range[2]))
  
  do.call(shiny::numericInput, options)
}


binary_input <- function(ns, self, type = "switch"){

  options <- self$options
  type <- match.arg(type)
  
  if(!("value" %in% names(options))){
    options$value <- FALSE
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = ns("input_element")), 
               options)
  do.call(shinyWidgets::materialSwitch, options)

}


