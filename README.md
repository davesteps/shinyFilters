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

Examples
---------------

#### Example 1 ([see here](https://trestletech.shinyapps.io/ss-01-persist/))

```
library(shiny)
library(shinyjs)
library(dplyr)
library(shinyFilters)


f1 <- selectFilter('cyl',c(6,8))
f2 <- selectFilter('gear')
f3 <- selectFilter('carb')
f4 <- sliderFilter('disp',c(0,500))
f5 <- sliderFilter('hp',c(0,500))

fl <- list(f1,f2,f3,f4,f5)


ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      f1$UI(label = 'Cyl'),
      f2$UI(label = 'Gears'),
      f3$UI(label = 'carb'),
      f4$UI(label = 'Disp'),
      f5$UI(label = 'hp'),
      actionButton('reset','Reset Filters')
    ),
    mainPanel(
      DT::dataTableOutput("data")
    )))

server <- function(input, output,session) {

  data <- reactive(mtcars)

  filterSet <- initFilters(fl, data)

  observeEvent(input$reset,{
    filterSet$reset()

  })

  output$data <- DT::renderDataTable({
    filterSet$output()
  })

}

shinyApp(ui, server)
```

