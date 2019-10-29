


slider_input <- function(ns, self){
  
  options <- self$options
  
  if(!("label" %in% names(options))){
    options$label <- self$column_name
  }
  if(!("value" %in% names(options))){
    options$value <- self$range
  }
  options <- c(list(inputId = ns("input_element")), 
               options, 
               list(min = self$range[1], max = self$range[2]))
  
  do.call(shiny::sliderInput, options)
  
}

numericrange_input <- function(ns, self){
  
  options <- self$options
  
  if(!("label" %in% names(options))){
    options$label <- self$column_name
  }
  if(!("value" %in% names(options))){
    options$value <- self$range
  }
  
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
  
  if(!("label" %in% names(options))){
    options$label <- self$column_name
  }
  if(!("selected" %in% names(options))){
    options$selected <- self$unique
  }
  if(!("multiple" %in% names(options))){
    options$multiple <- TRUE
  }
  options <- c(list(inputId = ns("input_element")), 
               options, 
               list(choices = self$unique))
  
  do.call(input_field, options)
  
}

numeric_input <- function(ns, self, type = c("min", "max")){
  
  type <- match.arg(type)
  type_index <- switch(type,
                       min = 1,
                       max = 2)
  options <- self$options
  
  if(!("label" %in% names(options))){
    options$label <- self$column_name
  }
  if(!("value" %in% names(options))){
    options$value <- self$range[type_index]
  }
  options <- c(list(inputId = ns("input_element")), 
               options, 
               list(min = self$range[1], max = self$range[2]))
  
  do.call(shiny::numericInput, options)
}


binary_input <- function(ns, self, type = "switch"){

  options <- self$options
  type <- match.arg(type)
  
  if(!("label" %in% names(options))){
    options$label <- self$column_name
  }

  if(!("value" %in% names(options))){
    options$value <- FALSE
  }
  
  options <- c(list(inputId = ns("input_element")), 
               options)
  do.call(shinyWidgets::materialSwitch, options)

}


