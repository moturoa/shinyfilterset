


slider_input <- function(id, self){
  
  options <- self$options
  
  if(is.null(options$value)){
    options$value <- c(floor_digits(self$range[1], self$round_digits),
                       ceiling_digits(self$range[2], self$round_digits))
  }
  
  options$label <- self$label
  
  min_val <- ifelse(is.null(options$min), 
                    options$value[1],
                    options$min)
  
  max_val <- ifelse(is.null(options$max), 
                    options$value[2],
                    options$max)
  
  options$min <- NULL
  options$max <- NULL
  
  options <- c(list(inputId = id), 
               options, 
               list(min = min_val, 
                    max = max_val))
  
  list(ui = do.call(shiny::sliderInput, options),
       value = options$value)
  
}


numericrange_input <- function(id, self){
  
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- c(floor_digits(self$range[1], self$round_digits), 
                       ceiling_digits(self$range[2], self$round_digits))

  }
  
  options$label <- self$label

  options <- c(list(inputId = id), options)
  list(ui = do.call(shinyWidgets::numericRangeInput, options),
       value = options$value)
}


select_input <- function(id, self, type = c("select","picker")){
  
  type <- match.arg(type)
  input_field <- switch(type,
                        select = shiny::selectizeInput,
                        picker = shinyWidgets::pickerInput
  )
  
  options <- self$options
  
  if(type == "select"){
    options$options <- list(plugins = 'remove_button')
  }
  
  if(!("selected" %in% names(options))){
    
    options$selected <- NULL
    
    if(!is.null(self$all_choice)){
      options$selected <- self$all_choice
    }
  }
  if(!("multiple" %in% names(options))){
    options$multiple <- TRUE
  }
  if(!("choices" %in% names(options))){
    
    if(is.null(self$n_label)){
      options$choices <- c(self$all_choice, self$unique)  
    } else {
      options$choices <- self$n_label
    }
    
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), 
               options)
  
  list(ui = do.call(input_field, options),
       value = options$selected)
  
}


checkboxes_input <- function(id, self){
  
  options <- self$options
  
  if(self$filter_ui == "checkboxes"){
    if(!("selected" %in% names(options))){
      options$selected <- NULL
    }
    if(!("choices" %in% names(options))){
      options$choices <- self$unique
    }  
  }
  
  options$label <- self$label
  
  options <- c(list(inputId = id), options)
  
  list(ui = do.call(shiny::checkboxGroupInput, options),
       value = options$selected)
  
}


date_range_input <- function(id, self){
  
  options <- self$options
  options$label <- self$label
  options <- c(list(inputId = id), options)
  
  if(!("start" %in% names(options))){
    options$start <- self$range[1]
    options$end <- self$range[2]
  }
  
  list(ui = do.call(shiny::dateRangeInput, options),
       value = c(options$start, options$end))
  
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


