shinyFilters
==========

The idea of `shinyFilters` is to allow quick and easy filtering of data.frames in Shiny. 

* The filter choices are cascading - If the user chooses 'USA' and 'Asia' in filter 1. All subsequent filters will be updated to only contain choices which meet this criteria.

* Enable/disable child filter based on condition of parent - Filter 2 is only enabled when 'USA' or 'Asia' are selected in filter 1. 



Installation
------------

Install using the devtools package

```
# Install devtools, if you haven't already.
install.packages("devtools")

devtools::install_github("davesteps/shinyFilters")
```

Usage
---------------

#### Example 1 ([see here](https://davesteps.shinyapps.io/shinyFilters/))

```
library(shiny)
# library(shinyjs)
library(dplyr)
library(shinyFilters)


# create filterset in global section of app
filterSet <- newFilterSet('FS1') %>%
  # give each filter unique id and tell it which column to filter on
  addSelectFilter('cylinders','cyl') %>%
  addSelectFilter('gears','gear') %>%
  addSliderFilter('disp',value=c(0,500)) %>%
  addSelectFilter('carb') %>%
  addCustomSliderFilter('hp',value=seq(0,500,50))


ui <- fluidPage(
  #shinyjs is required to show/hide filters
  # useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      filterInputs(filterSet),
      #action but to reset filters
      hr(),
      filterMaster(filterSet),
      filterResetButton(filterSet)
      # actionButton('resetFilter','resetFilter')
    ),
    mainPanel(
      DT::dataTableOutput("data")
    )
  )
)

server <- function(input, output,session) {

  # wrap data in reactive expression in case you
  # need to do something to the data before filtering
  data <- reactive(mtcars)

  # initialize the filter set
  filterSet <- initializeFilterSet(filterSet, data)

  # the output is a reactive data.frame
  output$data <- DT::renderDataTable(filterSet$output())

}

shinyApp(ui, server)
```

