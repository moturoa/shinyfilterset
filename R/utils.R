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
  vals <- sort(unique(x))
  names(vals) <- paste0(vals, " (",tab,")")
vals
}
