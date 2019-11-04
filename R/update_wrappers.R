update_slider <- function(session, ns, self, data){
  
  val <- range(data, na.rm=TRUE)
  updateSliderInput(session, self$ns_id, value = val)
  
}
