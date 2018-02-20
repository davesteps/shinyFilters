library(shiny)
# library(shinyjs)
library(dplyr)
library(shinyFilters)
# devtools::load_all('.')


# create filterset in global section of app
filterSet <- newFilterSet('FS1') %>%
  # give each filter unique id and tell it which column to filter on
  addSelectFilter('cylinders','cyl',childCondition='any(c(5,6)%in%x)') %>%
  addSelectFilter('gears','gear') %>%
  addSliderFilter('disp',value=c(0,500)) %>%
  addSelectFilter('carb',invertOpt=T) %>%
  addCustomSliderFilter('hp',value=seq(0,500,50))



filterUINames(filterSet)

ui <- fluidPage(
  #shinyjs is required to show/hide filters
  shinyjs::useShinyjs(),
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
  output$data <- DT::renderDataTable({
    print(names(input))
    filterSet$output()})

}

shinyApp(ui, server)
