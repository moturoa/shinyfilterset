x <- data_filter(column_data = mtcars$drat, column_name = "drat", filter_ui = "slider",
                    options = list(ticks=F))


my_filters <- data_filter_set(
  tags$h4("Filters"),
  data_filter(column_data = mtcars$drat, column_name = "drat", filter_ui = "slider", 
       options = list(label = "Select drat value", ticks = FALSE)),
  data_filter(column_data = mtcars$gear, column_name = "gear", filter_ui = "select",
       options = list(label = "Select gear")),
  data_filter(column_data = mtcars$cyl, column_name = "cyl", filter_ui = "numeric_min",
       options = list(label = "Select cyl")),
  tags$hr()
)

