#' Define a filter set
#' @description An R6 class to define a 'filter set', a collection of ui elements (sliders, pickers, etc.) 
#' to filter a dataset, for use in shiny applications.
#' @param \dots The filters, or HTML elements. Define filters with \code{data_filter}, see Examples.
#' @details This function makes a shinyfilterset objects, which has a number of methods and properties. 
#' id, elements, filters
#' ui(), apply(), reactive(), update()
#' The shinyfilterset is made up of \code{data_filter} objects, and optionally as many HTML tags as you 
#' like, for layout purposes.  These have to be constructed with \code{shiny::tags} (see Examples).
#' Each data filter has properties and methods of its own, most of which don't need to be called directly
#' by the user.
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

#' @export
filter_section <- function(section_nr = 1, ...){
  
  out <- lapply(list(...), function(x){
    
    # for html elements, store ui_section in attributes to avoid trouble
    if(inherits(x, "shiny.tag")){
      attributes(x)$ui_section <- section_nr
    } else {
      # for data filter objects, use R6 slot
      x$ui_section <- section_nr
    }
    return(x)
  })
  
  class(out) <- "filter_section"
  
return(out)
}

#' Make a data filter for use in shinyfilterset
#' @param id
#' @param ui_section
#' @param filter_ui
#' @param sort
#' @param options
#' @rdname datafilterset
#' @export
data_filter <- function(id = NULL, 
                        column_data, 
                        column_name, 
                        filter_ui = c("picker","select","checkboxes",
                                      "numeric_min","slider",
                                      "numeric_max","numeric_range","switch"), 
                        updates = FALSE,
                        sort = TRUE,
                        all_choice = NULL,
                        search_method = c("equal","regex"),
                        options = list(),
                        ui_section = 1){
  
  filter_ui <- match.arg(filter_ui)
  search_method <- match.arg(search_method)
  
  if(is.null(id)){
    id <- UUIDgenerate()
  }
  
  DataFilter$new(id = id,
                 column_data = column_data, 
                 column_name = column_name, 
                 filter_ui = filter_ui,
                 updates = updates,
                 all_choice = all_choice,
                 search_method = search_method,
                 options = options,
                 ui_section = ui_section)

}


# Class definition: a set of filters.
DataFilterSet <- R6::R6Class(
  classname = "datafilterset",
  public = list(
    id = NULL,
    elements = NULL,
    filters = NULL,
    history = c(),
    initialize = function(..., id){
      
      self$id <- id
      
      args <- list(...)
      if(inherits(args[[1]], "filter_section")){
        args <- do.call(c, args)
      }
      self$elements <- args
      
      # Find elements that are filters.
      is_filter <- sapply(self$elements, is.DataFilter)
      self$filters <- self$elements[is_filter]
      names(self$filters) <- sapply(self$filters, "[[", "column_name")
      
      
    },
    ui = function(ns, section = NULL){
      ns <- NS(self$id)
      
      tags$div(id = self$id,
        lapply(self$elements, function(x){
          
          if(is.Tag(x)){
            
            atr <- attributes(x)$ui_section
            if(is.null(atr) || is.null(section) || atr %in% section){
              return(x)
            }
            
          }
          if(is.DataFilter(x) && 
             (is.null(section) || x$ui_section %in% section)){
            return(x$ui(ns))
          }
          
        })
      )
      
    },
    
    apply = function(data){
        callModule(private$module_server, 
                   self$id,
                   data = data,
                   filters = self$filters)
    },
    
    update = function(session, data, input, last_filter = NULL){
      
      lapply(self$filters, function(x)x$update(session, data, input, last_filter))
    
    },
    
    reactive = function(input){
      lapply(self$filters, function(x)input[[x$id]])
    },
    
    monitor = function(input){
      lapply(self$filters, function(x){

        observeEvent(input[[x$ns_id]], {
          
          self$history <- c(self$history, x$column_name)
          
        })

      })
    },
    
    used_filters = function(input){
      
      chk <- sapply(names(self$filters), function(x){
        !isTRUE(all.equal(as.character(self$get_value(input, x)), 
                          as.character(self$filters[[x]]$value_initial)))
      })
      
      names(self$filters)[chk]
      
    },
    
    
    
    get_value = function(input, name){
      
      id <- private$get_filter_id(name)
      input[[id]]
      
    },
    
    set_value = function(session, input, name, val){
      
      self$filters[[name]]$set(session, val, input)
      
    },
    
    input_ids = function(){
      ns <- NS(self$id)
      bare_ids <- sapply(self$filters, "[[", "id")
      nms <- names(self$filters)
      ids <- paste0(ns(bare_ids), "-input_element")
      names(ids) <- nms
    ids
    },
    
    name_from_id = function(id){
      ids <- self$input_ids()
      names(self$filters)[ids == id]
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
  "DataFilter",
  public = list(
    
    id = NULL,
    ns_id = NULL,
    ui_section = NULL,
    column_name = NULL,
    updates = NULL,
    all_choice = NULL,
    search_method = NULL,
    label = NULL,
    unique = NULL,
    range = NULL,
    filter_ui = NULL,
    options = NULL,
    input_function = NULL,
    update_function = NULL,
    value_initial = NULL,
    
    # DataFilter$new()
    initialize = function(id, 
                          ui_section = NULL,
                          column_data = NULL, 
                          column_name, 
                          filter_ui,
                          updates = NULL,
                          sort = TRUE,
                          all_choice = NULL,
                          search_method = NULL,
                          options = list()){

      self$id <- id
      self$ui_section <- ui_section
      self$column_name <- column_name
      self$all_choice <- all_choice
      self$search_method <- search_method
      self$updates <- updates
      
      if(!("label" %in% names(options))){
        self$label <- self$column_name
      } else {
        self$label <- options$label
      }
      
      self$options <- options
      self$filter_ui <- filter_ui
      
      if(filter_ui %in% c("picker","select","checkboxes")){
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
                                    checkboxes = "shiny::checkboxGroupInput",
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
                                     checkboxes = update_checkboxes,
                                     picker = update_picker,
                                     numeric_min = update_numeric_min,
                                     numeric_max = update_numeric_max,
                                     numeric_range = update_range,
                                     switch = update_material
                                     )
    },
    
    #----- Methods
    update = function(session, data, input, last_filter = ""){

      if(self$updates && !last_filter == self$column_name){
        datavector <- data[[self$column_name]]
        if(!is.null(datavector)){
          do.call(self$update_function,
                  list(session = session, self = self, data = datavector, input = input)
          )
        }
      }

    },
    
    set = function(session, val, input){
      
      do.call(self$update_function,
              list(session = session, self = self, val = val, input = input)
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
      
      out <- switch(self$filter_ui, 
             
             slider = slider_input(ns, self),
             select = select_input(ns, self),
             checkboxes = checkboxes_input(ns, self),
             picker = select_input(ns, self, type = "picker"),
             numeric_min = numeric_input(ns, self, "min"),
             numeric_max = numeric_input(ns, self, "max"),
             numeric_range = numericrange_input(ns, self),
             switch = binary_input(ns, self, type = "switch")
             
      )
      
      self$value_initial <- out$value
      
    return(out$ui)
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
      
      if(self$filter_ui %in% c("select","picker","checkboxes")){
        
        # 'all_choice' = single choice that acts as all selector (e.g. "All options")
        if(is.null(self$all_choice) || 
           (!is.null(input$input_element) && input$input_element != self$all_choice)){
          
          # Filter with equality
          if(self$search_method == "equal"){
            data <- dplyr::filter(data, !!sym(column_name) %in% input$input_element)  
          
          # Filter with regular expression
          } else if(self$search_method == "regex"){
            regex <- paste(input$input_element, collapse = "|")
            data <- dplyr::filter(data, grepl(regex, !!sym(column_name)))
          }
          
        }
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





