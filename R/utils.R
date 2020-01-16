is.DataFilter <- function(x){
  inherits(x, "DataFilter")
}

is.Tag <- function(x){
  inherits(x, "shiny.tag")
}


make_choices <- function(x){
  if(is.factor(x)){
    x <- as.character(x)
  }
  tab <- table(x)
  if(dim(tab) == 0)return(NULL)
  
  vals <- sort(unique(x))
  if(all(tab == 1))return(vals)
  
  names(vals) <- paste0(vals, " (",tab,")")
vals
}


is_empty <- function(x){
  if(is.null(x))return(TRUE)
  
  if(length(x) == 1){
    out <- is.null(x) || x == ""
    out || is.na(out)  
  } else {
    sapply(x, is_empty)
  }
  
}
