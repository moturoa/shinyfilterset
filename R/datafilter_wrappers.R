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
                           updates = NULL,
                           updates_on_last_use = NULL,
                           n_label = NULL,
                           
                           all_data_on_null = TRUE,
                           id = NULL){
  
  if(is.null(id)){
    id <- uuid::UUIDgenerate()
  }
  
  DataFilterSet$new(..., 
                    data = data, 
                    id = id, 
                    updates = updates, 
                    updates_on_last_use = updates_on_last_use,
                    n_label = n_label,
                    all_data_on_null = all_data_on_null,
                    .list = .list) 
  
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
data_filter <- function(column_name, 
                        filter_ui = c("picker",   # category
                                      "select",
                                      "checkboxes",
                                      
                                      "slider",    # numeric range
                                      "numeric_range",
                                      
                                      "numeric_min",  # numeric minimum
                                      "numeric_max",  # ... maximum
                                      
                                      "date_range",
                                      
                                      "switch"), 
                        label = column_name,
                        updates = TRUE,
                        updates_on_last_use = FALSE,
                        sort = TRUE,   # category only
                        all_choice = NULL,  # category only
                        n_label = TRUE,  # category only
                        search_method = c("equal","regex"),  # category only
                        array_field = FALSE,  # category only
                        array_separator = ";",  # category only
                        round_digits = 1,
                        filter_function = NULL, 
                        static = FALSE,
                        pass_na = FALSE,
                        
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
                 updates_on_last_use = updates_on_last_use,
                 n_label = n_label,
                 all_choice = all_choice,
                 array_field = array_field,
                 array_separator = array_separator,
                 search_method = search_method,
                 round_digits = round_digits,
                 label = label,
                 filter_function = filter_function,
                 static = static,
                 pass_na = pass_na,
                 options = options,
                 ui_section = ui_section)
  
}




