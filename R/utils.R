is.DataFilter <- function(x){
  inherits(x, "DataFilter")
}

is.Tag <- function(x){
  inherits(x, "shiny.tag")
}


get_unique <- function(x, sort = TRUE, array_field = FALSE, array_separator = ";"){
  
  if(array_field){
    out <- unique(do.call(c, strsplit(x, array_separator)))  
  } else {
    out <- unique(x)
  }
  
  if(sort){
    out <- sort(out)
  }
  
  out
}



floor_digits <- function(x, digits){
  
  floor(x * 10^digits) / 10^digits
  
}


ceiling_digits <- function(x, digits){
  
  ceiling(x * 10^digits) / 10^digits
  
}


numeric_breaks_categories <- function(x, breaks, round_digits = 1){
  
  x <- x[!is.na(x)]
  lower <- c(floor_digits(min(x), round_digits),
             breaks)
  upper <- c(breaks, 
             ceiling_digits(max(x), round_digits))
  
  paste(lower, upper, sep = " - ")
  
}


numeric_breaks_labels <- function(x, breaks, round_digits = 1){
  
  x <- x[!is.na(x)]
  
  brks <- c(floor_digits(min(x),round_digits), 
            breaks, 
            ceiling_digits(max(x), round_digits))
  
  labels <- paste(brks[1:(length(brks)-1)],
                  brks[2:length(brks)], 
                  sep = " - ")
labels[findInterval(x, brks)]
}


make_choices <- function(x, n_label = TRUE, sort = TRUE, 
                         array_field = FALSE, array_separator = ";",
                         selected = NULL){
  
  if(is.factor(x)){
    x <- as.character(x)
  }
  
  
  if(!array_field){
    tab <- table(x)  
  } else {
    tab <- table(do.call(c, strsplit(x, array_separator)))
  }
  
  if(dim(tab) == 0)return(NULL)
  
  vals <- get_unique(x, sort, array_field, array_separator)
  
  # if(all(tab == 1)){
  #   return(vals)
  # }
  
  if(n_label){
    names(vals) <- paste0(vals, " (",tab,")")  
  }
  
  if(array_field && !is.null(selected)){
    vals <- vals[which(vals %in% selected)]
  }
  
vals
}

# x = vector (array)
# what = vector (OR)
search_array <- function(x, what, array_separator = ";"){
  
  lis <- strsplit(x, array_separator)
  sapply(lis, function(el){
    any(what %in% el)
  })
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

