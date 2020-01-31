#' Make a numeric vector into categories based on breaks
#' @description Cuts a vector based on the breaks argument (plus the min and max values, which should NOT)
#' be supplied in the breaks argument), and returns a vector of equal length with the category the data fall in.
#'@export
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

