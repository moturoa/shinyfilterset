

apply_filter <- function(data, value, object){
  
    colname <- object$column_name
    
    # If the filter UI has not been generated yet
    if(is.null(value)){
      return(data)
    }
    
    if(object$filter_ui %in% c("slider","numeric_range")){
      data <- dplyr::filter(data, 
                            !!sym(colname) >= value[1],
                            !!sym(colname) <= value[2])
    }
    
    if(object$filter_ui %in% c("select","picker","checkboxes")){
      
      # 'all_choice' = single choice that acts as all selector (e.g. "All options")
      if(is.null(object$all_choice) || 
         (!is.null(value) && value != object$all_choice)){
        
        # Filter with equality
        if(object$search_method == "equal"){
          data <- dplyr::filter(data, !!sym(colname) %in% value)  
          
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