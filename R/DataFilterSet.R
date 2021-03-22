

# Class definition: a set of filters.
DataFilterSet <- R6::R6Class(
  
  classname = "datafilterset",
  
  public = list(
    id = NULL,
    elements = NULL,
    filters = NULL,
    updates = NULL,
    updates_on_last_use = NULL,
    n_label = NULL,
    history = c(),
    ns = NS(NULL),
    all_data_on_null = NULL,
    last_filter = "",
    initialize = function(..., 
                          data, 
                          id, 
                          updates, 
                          all_data_on_null, 
                          updates_on_last_use, 
                          n_label, 
                          .list){
      
      self$id <- id
      self$all_data_on_null <- all_data_on_null
      self$updates <- updates
      self$updates_on_last_use <- updates_on_last_use
      self$n_label <- n_label
      
      if(!is.null(.list)){
        args <- .list
      } else {
        args <- list(...)  
      }
      
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
        
        if(obj$static){
          
          column_data <- NA
          .range <- NA
          .unique <- NA
          
        } else {
        
          column_data <- data[[obj$column_name]]
          
          # Text-based categorical filter
          if(obj$filter_ui %in% c("picker","select","checkboxes")){
            
            if(is.factor(column_data)){
              column_data <- as.character(column_data)
            }
            
            .unique <- make_choices(column_data, obj$n_label, obj$sort, obj$array_field, obj$array_separator,
                                    obj$select_choices)
            
            .range <- NULL
            
          } else if(obj$filter_ui %in% c("slider",
                                         "numeric_min",
                                         "numeric_max",
                                         "numeric_range",
                                         "date_range")){
            
            .unique <- NULL
            .range <- range(column_data, na.rm = TRUE) 
            
          } else if(obj$filter_ui == "switch"){
            
            .unique <- c(TRUE,FALSE)
            .range <- NULL
            
          }   
        }
        
        
        self$filters[[i]]$set("range", .range)
        self$filters[[i]]$set("unique", .unique)
        
        
        # Apply extra arguments
        if(!is.null(self$updates)){
          self$filters[[i]]$set("updates", updates)
        }
        if(!is.null(self$updates_on_last_use)){
          self$filters[[i]]$set("updates_on_last_use", updates_on_last_use)
        }
        if(!is.null(self$n_label)){
          self$filters[[i]]$set("n_label", n_label)
        }
      }
      
    },
    ui = function(ns = NS(NULL), section = NULL, horizontal = FALSE){
      
      ns <- NS(ns(self$id))
      
      self$history <- c()
      
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
                   
                   ui <- x$ui(ns)
                   
                   if(horizontal){
                     ui <- tags$div(style = "display: inline-block; vertical-align: top;", 
                              ui)  
                   }
                   
                   return(ui)
                 }
                 
               })
      )
      
    },
    
    apply = function(data){
      
      out <- callModule(private$filter_server, 
                        id = self$id,
                        data = data)
      
      self$update(out$data)
      
      # If set, if all filters are NULL or empty, return no data at all.
      if(!self$all_data_on_null){
        if(all(out$empty))out$data <- out$data[0,]  
      }
      
      return(out$data)
    },
    
    update = function(data){
      
      last_fil <- self$history[length(self$history)]
      
      #print(paste("LAST:", last_fil))
      
      callModule(private$update_server, 
                 id = self$id,
                 data = data,
                 last_filter = last_fil
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
    
    used_filters2 = function(){
      
      callModule(private$used_filters_server2, self$id)
      
    },
    
    monitor = function(){
      
      callModule(private$monitor_server, self$id)
      
    },
    
    
    reactive = function(){
      
      callModule(private$reactive_server, self$id)
      
    },
    
    # reset = function(name = NULL){
    #   
    #   self$filters[[name]]$reset(outer_id = self$id)
    #   
    # },
    
    reset_all = function(){
      
      self$history <- c()
      callModule(private$reset_server, self$id)
      
    },
    
    get_value = function(name = NULL){
      
      callModule(private$value_server, self$id, name = name)
      
    }
    
  ),
  
  private = list(
    
    value_server = function(input, output, session, name){
      
      filt <- self$filters[[name]]
      input[[filt$id]]
      
    },
    
  filter_server = function(input, output, session, data){
      
      nms <- names(self$filters)
      empt <- c()
      
      for(i in seq_along(nms)){
        
        # Load filter
        filt <- self$filters[[nms[i]]]
        
        # Check if the filter returns a value
        suppressWarnings(
          empt[i] <- is_empty(input[[filt$id]])
        )
        
        if(!empt[i]){
          data <- apply_filter(data, 
                               value = input[[filt$id]],
                               object = filt)  
        }
      }

      return(list(data = data, empty = empt))
  },
    
    
  update_server = function(input, output, session, data, last_filter){
      
      lapply( self$filters, function(x){
        
        x$update(session, 
                 id = x$id, 
                 data = data, 
                 input = input,
                 last_filter = last_filter)
        
      })
      
  },
    
  load_server = function(input, output, session, vals){
      
      for(i in seq_along(vals)){
        
        col <- names(vals)[i]
        val <- vals[[i]]
        filt <- self$filters[[col]]
        
        if(!is.null(filt)){
          filt$set_value(session, id = filt$id, val)  
        }
        
      }
      
  },
    
  reset_server = function(input, output, session){
      
      for(i in seq_along(self$filters)){
        
        filt <- self$filters[[i]]
        
        if(!is.null(filt)){
          filt$set_value(session, id = filt$id, filt$value_initial)  
        }
        
      }
      
      
  },
    
  monitor_server = function(input, output, session){
      
      lapply(self$filters, function(x){

        
        shinyjs::onclick(x$id, {
          
          #print(paste("EVENT", x$column_name))
          self$history <- c(self$history, x$column_name)  
          
        })
        
        
        # observeEvent(input[[x$id]], priority = 100, {
        # 
        #   #self$history <- c(self$history, x$column_name)
        #   #print(paste("OBSERVE EVENT", x$column_name))
        # 
        # })
        
      })
      
  },
    
  # notused
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
      
  },
    
    
  used_filters_server2 = function(input, output, session){
      
      chk <- sapply(self$filters, function(x){
        !isTRUE(all.equal(as.character(input[[x$id]]), 
                          as.character(x$value_initial)))
      })
      
      vals <- lapply(self$filters[chk], function(x)input[[x$id]])
      
      return(reactive(vals))
      
  }
    
    
    
    
  )
)



