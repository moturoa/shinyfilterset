#' Define a filter set
#' @description An R6 class to define a 'filter set', a collection of ui elements (sliders, pickers, etc.) 
#' to filter a dataset, for use in shiny applications.
#' @param \dots The filters, or HTML elements. Define filters with \code{data_filter}, see Examples.
#' @details
#' @examples
#' @export
#' @rdname datafilterset
#' @importFrom R6 R6Class
shinyfilterset <- function(..., id = NULL){
  
  if(is.null(id)){
    id <- UUIDgenerate()
  }
  
  DataFilterSet$new(..., id = id)
  
}

#' Make a data filter for use in shinyfilterset
#' @param id
#' @param filter_ui
#' @param sort
#' @param options
#' @rdname datafilterset
#' @export
data_filter <- function(id = NULL, column_data, column_name, 
                        filter_ui = c("picker","select","numeric_min","slider",
                                      "numeric_max","numeric_range","switch"), 
                        sort = TRUE,
                        options = list()){
  
  filter_ui <- match.arg(filter_ui)
  
  if(is.null(id)){
    id <- UUIDgenerate()
  }
  
  DataFilter$new(id = id,
                 column_data = column_data, 
                 column_name = column_name, 
                 filter_ui = filter_ui,
                 options = options)

}


# Class definition: a set of filters.
DataFilterSet <- R6::R6Class(
  public = list(
    id = NULL,
    elements = NULL,
    filters = NULL,
    initialize = function(..., id){
      
      self$id <- id
      
      self$elements <- list(...)
      
      # Find elements that are filters.
      is_filter <- sapply(self$elements, is.R6)
      self$filters <- self$elements[is_filter]
      names(self$filters) <- sapply(self$filters, "[[", "column_name")
      
      
    },
    ui = function(ns){
      ns <- NS(self$id)
      
      tags$div(id = self$id,
        lapply(self$elements, function(x){
          
          switch(class(x), 
                 R6 = x$ui(ns),
                 shiny.tag = x)
          
        })
      )
      
    },
    
    apply = function(data){
        callModule(private$module_server, self$id,
                   data = data,
                   filters = self$filters)
    },
    
    update = function(session, data, input){
      
      lapply(self$filters, function(x)x$update(session, data, input))
      
    },
    
    reactive = function(input){
      lapply(self$filters, function(x)input[[x$id]])
    },
    
    get_value = function(input, name){
      
      ns <- NS(self$id)
      id <- private$get_filter_id(name)
      input[[id]]
      
    }
    
  ),
  
  private = list(
    get_filter_id = function(name){
      ns <- NS(self$id)
      paste0(ns(self$filters[[name]]$id), "-input_element")
    },
    module_server = function(input, output, session, data, filters){
      
      out <- filters[[1]]$apply(data)
      if(length(filters) > 1){
        for(i in 2:length(filters)){
          out <- filters[[i]]$apply(out)
        }
      }
      return(out)
    
    }
  )
)


# Class definition: a single filter.
# Not used by user!
DataFilter <- R6Class(
  public = list(
    
    id = NULL,
    ns_id = NULL,
    column_name = NULL,
    label = NULL,
    unique = NULL,
    range = NULL,
    filter_ui = NULL,
    options = NULL,
    input_function = NULL,
    update_function = NULL,
    
    # DataFilter$new()
    initialize = function(id, 
                          column_data = NULL, 
                          column_name, 
                          filter_ui,
                          sort = TRUE,
                          options = list()){

      self$id <- id
      self$column_name <- column_name
      
      if(!("label" %in% names(options))){
        self$label <- self$column_name
      } else {
        self$label <- options$label
      }
      
      self$options <- options
      self$filter_ui <- filter_ui
      
      if(filter_ui %in% c("picker","select")){
        if(is.factor(column_data)){
          column_data <- as.character(column_data)
        }
        self$unique <- unique(column_data)
        if(sort){
          self$unique <- sort(self$unique)
        }
        
        self$range <- NULL
      } else if(filter_ui %in% c("slider","numeric_min","numeric_max","numeric_range")){
        self$unique <- NULL
        self$range <- range(column_data, na.rm = TRUE) 
      } else if(filter_ui == "binary"){
        self$unique <- c(TRUE,FALSE)
        self$range <- NULL
      }
      
      # Register the actual function used to make the input field
      # not used
      self$input_function <- switch(self$filter_ui, 
                                    slider = "shiny::sliderInput",
                                    select = "shiny::selectInput",
                                    picker = "shinyWidgets::pickerInput",
                                    numeric_min = "shiny::numericInput",
                                    numeric_max = "shiny::numericInput",
                                    numeric_range = "shinyWidgets::numericRangeInput",
                                    switch = "shinyWidgets::materialSwitch"
                                    )
      
      # register the function that can be used to update the input field
      self$update_function <- switch(self$filter_ui,
                                     slider = update_slider,
                                     select = update_select,
                                     picker = update_picker,
                                     numeric_min = update_numeric_min,
                                     numeric_max = update_numeric_max,
                                     numeric_range = update_range,
                                     switch = update_material
                                     )
      
    },
    
    #----- Methods
    update = function(session, data, input){

      datavector <- data[[self$column_name]]
      do.call(self$update_function,
              list(session = session, self = self, data = datavector, input = input)
      )

    },
    
    apply = function(data){
      callModule(private$module_server, self$id,
                 data = data,
                 column_name = self$column_name)
    },
    
    ui = function(ns = NS(NULL)){
      
      # https://stackoverflow.com/questions/46693161/wrapping-shiny-modules-in-r6-classes
      ns <- NS(ns(self$id))
      self$ns_id <- ns("input_element")
      
      switch(self$filter_ui, 
             
             slider = slider_input(ns, self),
             select = select_input(ns, self),
             picker = select_input(ns, self, type = "picker"),
             numeric_min = numeric_input(ns, self, "min"),
             numeric_max = numeric_input(ns, self, "max"),
             numeric_range = numericrange_input(ns, self),
             switch = binary_input(ns, self, type = "switch")
             
      )
      
    }
  ),
  private = list(
    module_server = function(input, output, session, data, column_name){
      
      # If the filter UI has not been generated yet
      if(is.null(input$input_element)){
        return(data)
      }
      
      if(self$filter_ui %in% c("slider","numeric_range")){
        data <- dplyr::filter(data, 
                              !!sym(column_name) >= input$input_element[1],
                              !!sym(column_name) <= input$input_element[2])
      }
      
      if(self$filter_ui %in% c("select","picker")){
        data <- dplyr::filter(data, !!sym(column_name) %in% input$input_element)
      }
      
      if(self$filter_ui == "numeric_min"){
        data <- dplyr::filter(data, !!sym(column_name) >= input$input_element)
      }
      
      if(self$filter_ui == "numeric_max"){
        data <- dplyr::filter(data, !!sym(column_name) <= input$input_element)
      }
      
      if(self$filter_ui == "switch"){
        
        # If the switch is OFF (FALSE), don't filter. Only filter the TRUE values if the switch is ON.
        if(input$input_element){
          data <- dplyr::filter(data, !!sym(column_name) == input$input_element)  
        }
        
      }
      
      return(data)  
    }
  )
)





