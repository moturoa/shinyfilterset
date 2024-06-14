
![](https://badgen.net/badge/shintolabs/production/green)
# shinyfilterset 

Create sets of data filters in Shiny applications with ease, with lots of customization options.


*Features*

- Define a collection of elements to filter data, together making a 'filter set' for a dataset.
- Choose from many input fields, such as select boxes, sliders, numeric min/max, with a simple unified API.
- By default, filters auto-update the options based on the filtered data - this behavior can be further customized.
- Filter options indicate the number of observations in the option.
- Easily retrieve a list of the filters set by the user so far, and load saved settings to update the filters.
- Reset filters to their original state (for example with a button).
- Mix filter definitions with bits of HTML to style the filter sets.
- Filter methods are stored in an R6 object, allowing the developer complete control over the integration with the application.


## Quick example

To define a filter set, we use `data_filter` and `shinyfilterset` :

```
my_filters <- shinyfilterset(data = mtcars,
  data_filter("drat", "picker", "Rear Axle Ratio"),
  data_filter("cyl", "slider", "Number of cylinder"),
  data_filter("gear", "checkboxes", "gears")
)

```

Here the first argument to `data_filter` is the column name of the dataset you wish to filter, then a UI for the filter (which can be customized), and then a label to apply to the filter.

Then, in the UI of a shiny app, all you have to do to generate the HTML of the filter set:

```
my_filters$ui()
```


In the server section, we provide a filtered dataset by using the `apply` method:


```
observe({
  rv$mtcars_filtered <- my_filters$apply(mtcars)
})

```

where `rv` is a `reactiveValues` (but you can do whatever you want). The `apply` method is reactive itself, so the filters will be applied whenever the user changes any settings.

To reset the filters to the original state (and thus resetting the filtered data), we just have to tie the `reset_all` method to say, a button:

```
observeEvent(input$btn_reset, {
  my_filters$reset_all()
})
```


## Installation


```
remotes::install_bitbucket("shintolabs/shinyfilterset", auth_user = "YOU", password = "PASS")

```


## Contact

Wesley Brants




