# Class definition: a single filter.
DataFilter <- R6Class(
  "DataFilter",
  public = list(
    
    id = NULL,
    ui_section = NULL,
    column_name = NULL,
    updates = NULL,
    updates_on_last_use = NULL,
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
    n_updates = 0,
    
    # DataFilter$new()
    initialize = function(id, 
                          ui_section = NULL,
                          #column_data = NULL, 
                          column_name, 
                          filter_ui,
                          updates = NULL,
                          updates_on_last_use = NULL,
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
      self$updates_on_last_use <- updates_on_last_use
      
      self$n_label <- n_label
      self$sort <- sort
      self$array_field <- array_field
      self$array_separator <- array_separator
      
      self$round_digits <- round_digits
      
      self$label <- label
      
      self$options <- options
      self$filter_ui <- filter_ui
      
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
      
      is_last <- isTRUE(!is.null(last_filter) && last_filter == self$column_name)
      
      if(self$n_updates == 0 | (self$updates & !(is_last & !self$updates_on_last_use))){
      
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
          self$n_updates <- self$n_updates + 1
        }
      }
      
    },
    
    set_value = function(session, id, val){
      
      do.call(self$set_function,
              list(session = session, id = id, self = self, value = val)
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
      self$n_updates <- 0
      
      return(out$ui)
    }
  )
  
)


