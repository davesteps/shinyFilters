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

filterSet@filterUINames

filterSet@filterList[[1]]





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
      DT::dataTableOutput("data"),
      verbatimTextOutput('activeFilters')
    )
  )
)

server <- function(input, output,session) {

  # wrap data in reactive expression in case you
  # need to do something to the data before filtering
  data <- reactive(mtcars)

  # initialize the filter set
  fs <- initializeFilterSet(filterSet, data)

  output$activeFilters <- renderPrint({
    printActiveFilters(input,filterSet)
  })
  # the output is a reactive data.frame
  output$data <- DT::renderDataTable({

    fs$output()})

}

shinyApp(ui, server)
