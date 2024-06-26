

apply_filter <- function(data, value, object){
  
    colname <- object$column_name
    
    # If we have no data, return data
    if(nrow(data) == 0)return(data)
    
    # If the filter UI has not been generated yet
    if(is.null(value)){
      return(data)
    }
    
    # Custom filter function
    if(!is.null(object$filter_function)){
      
      data <- object$filter_function(data, value)
      return(data)
    }
    
    if(object$filter_ui %in% c("slider","numeric_range")){
      if(!object$pass_na){
        data <- dplyr::filter(data,
                              between(!!sym(colname), value[1], value[2]))  
      } else {

        data <- dplyr::filter(data, 
                              is.na(!!sym(colname)) | 
                                between(!!sym(colname), value[1], value[2]))  
        
      }
      
    }
    
    if(object$filter_ui %in% c("date_range")){
      if(!object$pass_na){
        data <- dplyr::filter(data,
                              between(!!sym(colname), as_datetime(glue("{value[1]} 00:00:00")), as_datetime(glue("{value[2]} 23:59:59"))))  
      } else {
        
        data <- dplyr::filter(data, 
                              is.na(!!sym(colname)) | 
                                between(!!sym(colname), as_datetime(glue("{value[1]} 00:00:00")), as_datetime(glue("{value[2]} 23:59:59"))))  
        
      }
      
    }
    
    if(object$filter_ui %in% c("select","picker","pickersearch","virtualsearch","checkboxes")){
      
      # 'all_choice' = single choice that acts as all selector (e.g. "All options")
      if(is.null(object$all_choice) || 
         (!is.null(value) && value != object$all_choice)){
        
        # Filter with equality
        if(object$search_method == "equal"){
          
          if(!object$array_field){
            data <- dplyr::filter(data, !!sym(colname) %in% value)    
          } else {
            data <- dplyr::filter(data, search_array(!!sym(colname), 
                                                     what = value, 
                                                     array_separator = object$array_separator,
                                                     array_comparison = object$array_comparison))
          }
          
          # Filter with regular expression
        } else if(object$search_method == "regex"){
          regex <- paste(value, collapse = "|")
          data <- dplyr::filter(data, grepl(regex, !!sym(colname)))
        }
        
      }
    }
    
    if(object$filter_ui == "numeric_min"){
      data <- dplyr::filter(data, !!sym(colname) >= value)
    }
    
    if(object$filter_ui == "numeric_max"){
      data <- dplyr::filter(data, !!sym(colname) <= value)
    }
    
    if(object$filter_ui == "switch"){
      
      # If the switch is OFF (FALSE), don't filter. Only filter the TRUE values if the switch is ON.
      if(value){
        data <- dplyr::filter(data, !!sym(colname) == value)  
      }
      
    }
    
return(data)  


}