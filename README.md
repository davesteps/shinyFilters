shinyFilters
==========

The idea of `shinyFilters` is to allow quick and easy filtering of data.frames in Shiny. 

* The filter choices are cascading - The user chooses 'USA' and 'Asia' in filter 1. All subsequent filters will be updated to only contian choices which meet this criteria.

* Enable/disable child filter based on condition of parent - Filter 2 is only enabled when 'USA' or 'Asia' are selected in filter 1. 


Installation
------------

Install using the devtools package

```
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("davesteps/shinyFilters")
```

Usage
---------------

#### Example 1 ([see here](https://davesteps.shinyapps.io/shinyFilters/))

```
library(shiny)
library(shinyjs)
library(dplyr)
library(shinyFilters)

# create filters in global section of app
f1 <- selectFilter('cyl')
f2 <- selectFilter('gear')
f3 <- selectFilter('carb')
f4 <- sliderFilter('disp',defaults=c(0,500))
f5 <- sliderFilter('hp',defaults=c(0,500))

# create list of filters in
fl <- list(f1,f2,f3,f4,f5)


ui <- fluidPage(
  useShinyjs(),#shinyjs is required to show/hide filters
  sidebarLayout(
    sidebarPanel(
      f1$UI(label = 'Cyl'), #create UIs of filters
      f2$UI(label = 'Gears'),
      f3$UI(label = 'carb'),
      f4$UI(label = 'Disp'),
      f5$UI(label = 'hp'),
      actionButton('reset','Reset Filters') #action but to reset filters
    ),
    mainPanel(
      DT::dataTableOutput("data")
    )))

server <- function(input, output,session) {

  # wrap data in reactive object
  data <- reactive(mtcars)

  # Initilize filters
  filterSet <- initFilters(fl, data)

  # observer for reset button
  observeEvent(input$reset,{
    filterSet$reset()
  })

  # filterSet$output() contains the filtered data
  output$data <- DT::renderDataTable({
    filterSet$output()
  })

}

shinyApp(ui, server)
```

