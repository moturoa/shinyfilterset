#' Convert a list definition from YAML for shinyfilterset
#' @export
from_list_definition <- function(lis, ...){
  
  nms <- names(lis)
  for(i in seq_along(nms)){
    lis[[i]]$column_name <- nms[i]
  }
  
  make_data_filter <- function(obj){
    
    lab <- if(!is.null(obj$tooltip)){
      
      shintoshiny::label_tooltip(obj$label, obj$tooltip)
      
    } else {
      
      if(is.null(obj$label)){
        obj$column_name
      } else {
        obj$label  
      }
      
    }
    
    m_get <- function(what, default){
      if(is.null(obj[[what]])){
        default
      } else {
        obj[[what]]
      }
    }
    
    data_filter(obj$column_name, 
                filter_ui = obj$ui,
                label = lab,
                updates = m_get("updates", FALSE),
                ui_section = m_get("section", 1),
                array_field = m_get("array", FALSE),
                pass_na = m_get("pass_na", TRUE),
                options = list(width = "100%"))
  }
  
  lapply(lis, make_data_filter)
  
}
