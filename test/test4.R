f <- shinyfilterset(
  
  filter_section(1,
                 
                 tags$h4("Filters (1)"),
                 
                 data_filter(column_data = mtcars$drat, 
                             column_name = "drat", 
                             filter_ui = "slider", 
                             updates = TRUE,
                             options = list(label = "Select drat value", ticks = FALSE)),
                 data_filter(column_data = mtcars$disp, 
                             column_name = "disp", 
                             filter_ui = "numeric_range", 
                             updates = TRUE,
                             options = list(label = "Select disp value"))
  ),
  filter_section(2,
                 
                 tags$h4("Filters (2)"),
                 
                 data_filter(column_data = mtcars$drat, 
                             column_name = "drat", 
                             filter_ui = "slider", 
                             updates = TRUE,
                             options = list(label = "Select drat value", ticks = FALSE))
  )
  
)

f$ui(section=2)

