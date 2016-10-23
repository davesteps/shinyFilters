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

# create list of filters
fl <- list(f1,f2,f3,f4,f5)

ui <- fluidPage(
  #shinyjs is required to show/hide filters
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      #create UIs of filters, id of filter is used as default label
      filterInput(f1),
      filterInput(f2),
      filterInput(f3),
      filterInput(f4),
      filterInput(f5),
      #action but to reset filters
      actionButton('reset','Reset Filters')
    ),
    mainPanel(
      DT::dataTableOutput("data")
    )))

server <- function(input, output,session) {

  # wrap data in reactive expression
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
