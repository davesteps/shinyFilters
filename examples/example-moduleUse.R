if(interactive()) {

  library(shiny)
  library(shinyjs)
  library(dplyr)
  library(shinyFilters)

  # create filterset in global section of app
  filterSet <- newFilterSet('FS1', moduleID = "mod_name") %>%
    # give each filter unique id and tell it which column to filter on
    addSelectFilter('cylinders','cyl') %>%
    addSelectFilter('gears','gear') %>%
    addSliderFilter('disp',value=c(0,500)) %>%
    addSelectFilter('carb') %>%
    addCustomSliderFilter('hp',value=seq(0,500,50))

  ModuleUI <- function(id) {
    ns <- shiny::NS(id)

    shiny::fluidPage(
      #shinyjs is required to show/hide filters
      shinyjs::useShinyjs(),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          filterInputs(filterSet),
          shiny::hr(),
          filterMaster(filterSet),
          filterResetButton(filterSet)
        ),
        shiny::mainPanel(
          DT::dataTableOutput(ns("data"))
        )
      ))
  }


  ModuleServer <- function(input, output, session, data_f = data_mut, filterSet) {
    # wrap data in reactive expression in case you
    # need to do something to the data before filtering
    data <- shiny::reactive(mtcars)

    # initialize the filter set
    filterSet <- initializeFilterSet(filterSet, data)

    # the output is a reactive data.frame
    output$data <- DT::renderDataTable(filterSet$output())

  }


  ui <- shiny::shinyUI(ModuleUI("mod_name"))

  server <- function(input, output, session) {shiny::callModule(ModuleServer, "mod_name", filterSet = filterSet)}

  shiny::shinyApp(ui, server)
}
