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
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @importFrom uuid UUIDgenerate
shinyfilterset <- function(..., 
                           data = NULL, 
                           .list = NULL,
                           all_data_on_null = TRUE,
                           id = NULL){
  
  if(is.null(id)){
    id <- uuid::UUIDgenerate()
  }
  
  DataFilterSet$new(..., data = data, id = id, all_data_on_null = all_data_on_null) 
  
}


#y <- x$cereal_filters
#out <- do.call(shinyfilterset, lapply(names(y), function(nm)data_filter(nm, y[[nm]])))

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
data_filter <- function(column_name, 
                        filter_ui = c("picker",   # category
                                      "select",
                                      "checkboxes",
                                      
                                      "slider",    # numeric range
                                      "numeric_range",
                                      
                                      "numeric_min",  # numeric minimum
                                      "numeric_max",  # ... maximum
                                      
                                      "switch"), 
                        label = column_name,
                        updates = TRUE,
                        sort = TRUE,   # category only
                        all_choice = NULL,  # category only
                        n_label = TRUE,  # category only
                        search_method = c("equal","regex"),  # category only
                        array_field = FALSE,  # category only
                        array_separator = ";",  # category only
                        round_digits = 1,
                        
                        # sent to input method
                        options = list(),
                        ui_section = 1,
                        id = NULL){
  
  filter_ui <- match.arg(filter_ui)
  search_method <- match.arg(search_method)
  
  if(is.null(id)){
    id <- uuid::UUIDgenerate()
  }

   
  DataFilter$new(id = id,
                 column_name = column_name, 
                 filter_ui = filter_ui,
                 updates = updates,
                 n_label = n_label,
                 all_choice = all_choice,
                 array_field = array_field,
                 array_separator = array_separator,
                 search_method = search_method,
                 round_digits = round_digits,
                 label = label,
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
    ns = NS(NULL),
    all_data_on_null = NULL,
    last_filter = "",
    initialize = function(..., data, id, all_data_on_null){
      
      self$id <- id
      self$all_data_on_null <- all_data_on_null
      
      args <- list(...)
      if(inherits(args[[1]], "filter_section")){
        args <- do.call(c, args)
      }
      self$elements <- args
      
      # Find elements that are filters.
      is_filter <- sapply(self$elements, is.DataFilter)
      self$filters <- self$elements[is_filter]
      names(self$filters) <- sapply(self$filters, "[[", "column_name")
      
      # Set data summaries.
      # Do NOT store entire passed dataset, only what is necessary: 
      # unique() for character/factor,
      # range() for numerics.
      for(i in seq_along(self$filters)){
        
        # R6 class constructed with data_filter()
        obj <- self$filters[[i]]
        
        column_data <- data[[obj$column_name]]
        
        
        # Text-based categorical filter
        if(obj$filter_ui %in% c("picker","select","checkboxes")){
          
          if(is.factor(column_data)){
            column_data <- as.character(column_data)
          }
          
          .unique <- make_choices(column_data, obj$n_label, obj$sort, obj$array_field, obj$array_separator)
          .range <- NULL
          
        } else if(obj$filter_ui %in% c("slider",
                                   "numeric_min",
                                   "numeric_max",
                                   "numeric_range")){
          .unique <- NULL
          .range <- range(column_data, na.rm = TRUE) 
          
        } else if(obj$filter_ui == "binary"){
          
          .unique <- c(TRUE,FALSE)
          .range <- NULL
          
        } 
        
        self$filters[[i]]$set("range", .range)
        self$filters[[i]]$set("unique", .unique)
        
      }
      
    },
    ui = function(ns = NS(NULL), section = NULL){
      
      ns <- NS(ns(self$id))
      
      tags$div(id = ns(self$id),
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
      out <- callModule(private$filter_server, 
                 id = self$id,
                 data = data)
      
      self$update(out)
      
    return(out)
    },
    
    update = function(data, last_filter = ""){
      
      callModule(private$update_server, 
                 id = self$id,
                 data = data,
                 last_filter = last_filter
                 )
    },

    
    load = function(vals){
      
      callModule(private$load_server, 
                 id = self$id,
                 vals = vals
      )
      
    },
    
    used_filters = function(){
      
      callModule(private$used_filters_server, self$id)
      
    },
    
    reactive = function(){
      
      callModule(private$reactive_server, self$id)
      
    }

  ),
  
  private = list(
    

    filter_server = function(input, output, session, data){
      
      nms <- names(self$filters)
      empt <- c()
      
      for(i in seq_along(nms)){
        
        filt <- self$filters[[nms[i]]]
        suppressWarnings(empt[i] <- is_empty(input[[filt$id]]))
        
        if(!empt[i]){
          data <- apply_filter(data, 
                               value = input[[filt$id]],
                               object = filt)  
        }
      }
      
      # If set, if all filters are NULL or empty, return no data at all.
      if(!self$all_data_on_null){
        if(all(empt))data <- data[0,]  
      }
      
    return(data)
    },
    
    
    update_server = function(input, output, session, data, last_filter){
      
      lapply(self$filters, function(x){
        x$update(session, 
                 id = x$id, 
                 data = data, 
                 input = input, 
                 last_filter = last_filter)
      })
      
    },
    
    load_server = function(input, output, session, vals){
      
      for(i in seq_along(vals)){
        filt <- self$filters[[names(vals)[i]]]
        if(!is.null(filt)){
          filt$set_value(session, id = filt$id, vals[[i]])  
        }
        
      }
      
    },
    
    reactive_server = function(input, output, session){
      
      lapply(self$filters, function(x){
        input[[x$id]]
      })
      
    },
    
    used_filters_server = function(input, output, session){
      
      chk <- sapply(self$filters, function(x){
        !isTRUE(all.equal(as.character(input[[x$id]]), 
                          as.character(x$value_initial)))
      })
      
      vals <- lapply(self$filters[chk], function(x)input[[x$id]])
      
      return(vals)
      
    }
    
    
  )
)


# Class definition: a single filter.
DataFilter <- R6Class(
  "DataFilter",
  public = list(
    
    id = NULL,
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
    #input_function = NULL,
    update_function = NULL,
    set_function = NULL,
    value_initial = NULL,
    n_label = NULL,
    sort = NULL,
    array_field = NULL,
    array_separator = NULL,
    round_digits = NULL,
    
    # DataFilter$new()
    initialize = function(id, 
                          ui_section = NULL,
                          #column_data = NULL, 
                          column_name, 
                          filter_ui,
                          updates = NULL,
                          sort = TRUE,
                          n_label = TRUE,
                          all_choice = NULL,
                          array_field = FALSE,
                          array_separator = ";",
                          search_method = NULL,
                          round_digits = NULL,
                          label = NULL,
                          options = list()){
      
      self$id <- id
      self$ui_section <- ui_section
      self$column_name <- column_name
      self$all_choice <- all_choice
      self$search_method <- search_method
      self$updates <- updates
      
      self$n_label <- n_label
      self$sort <- sort
      self$array_field <- array_field
      self$array_separator <- array_separator
      
      self$round_digits <- round_digits
      
      self$label <- label
      
      self$options <- options
      self$filter_ui <- filter_ui

      
      # # Register the actual function used to make the input field
      # # not used
      # self$input_function <- switch(self$filter_ui, 
      #                               slider = "shiny::sliderInput",
      #                               select = "shiny::selectInput",
      #                               checkboxes = "shiny::checkboxGroupInput",
      #                               picker = "shinyWidgets::pickerInput",
      #                               numeric_min = "shiny::numericInput",
      #                               numeric_max = "shiny::numericInput",
      #                               numeric_range = "shinyWidgets::numericRangeInput",
      #                               switch = "shinyWidgets::materialSwitch"
      # )
      
      # register the function that can be used to update the input field,
      # choices, min/max, etc.
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
      
      # The function to set the value of the filter.
      self$set_function <- switch(self$filter_ui,
                                  slider = set_slider,
                                  select = set_select,
                                  checkboxes = set_checkboxes,
                                  picker = set_picker,
                                  numeric_min = set_numeric_min,
                                  numeric_max = set_numeric_max,
                                  numeric_range = set_range,
                                  switch = set_material
      )
      
      
    },
    
    #----- Methods
    
    # Sets values in self
    set = function(what, value){
      self[[what]] <- value
    },
    
    update = function(session, id, data, input, last_filter = ""){
      
      if(self$updates){ #&& !last_filter == self$column_name){
        
        column_data <- data[[self$column_name]]
        
        # hier niet nodig? zie update functions
        if(is.factor(column_data)){
          column_data <- as.character(column_data)
        }
        
        #print(self$column_name)
        if(!is.null(column_data)){
          do.call(self$update_function,
                  list(session = session, id = id, self = self, data = column_data, input = input)
          )
        }
      }
      
    },
    
    set_value = function(session, id, val){
      
      do.call(self$set_function,
              list(session = session, id = id, self = self, value = val)
      )
      
    },
    
    reset = function(session, input, id){

      do.call(self$set_function,
              list(session = session, id = id, self = self, value = self$value_initial)
      )

    },
    
    ui = function(ns = NS(NULL)){
      
      id <- ns(self$id)
      out <- switch(self$filter_ui, 
                    
              slider = slider_input(id, self),
              select = select_input(id, self),
              checkboxes = checkboxes_input(id, self),
              picker = select_input(id, self, type = "picker"),
              numeric_min = numeric_input(id, self, "min"),
              numeric_max = numeric_input(id, self, "max"),
              numeric_range = numericrange_input(id, self),
              switch = binary_input(id, self, type = "switch")
                    
      )
      
      self$value_initial <- out$value
      
      return(out$ui)
    }
  )
)





