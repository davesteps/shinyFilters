library(shiny)
library(shinyjs)
library(dplyr)

# TODO ---- ---------------------------------------------------------------
# invert
# hide
# child condition
# capture UI names for save State
# button tooltip

# 1 filter
# output slot of filter set
# make disabled when bypassed
# keep bypassed filters bypassed when other filters change

filterModuleInput <- function(id,label,choices='',multiple=T,...){
  ns <- NS(id)
  selectizeInput(ns('filter'),label = label,choices,multiple=multiple,...)
}

filterModule <- function(input, output, session,colname, data,reset){



  r <- reactive({
    df <- data()
    df[df[,colname] %in% input$filter,]
  })


  observeEvent(reset(),{
    print(reset())
    updateSelectizeInput(session,'filter',selected = character(0))

  })


  return(r)
}

filterSetModule <- function(input, output, session,data){

  r <- reactive(input$resetFilter)

  f1 <- callModule(filterModule,'cyl',colname='cyl',data=data,reset=r)
  f2 <- callModule(filterModule,'gear',colname='gear',data=f1,reset=r)

  f2

}
# filterResetButton <- function(id,...){
#   ns <- NS(id)
#
# }
filterSetUI <- function(id,col){

  ns <- NS(id)
  # filterModuleInput(ns("cyl"))
  wellPanel(
  filterModuleInput(ns('cyl'),label = 'cyl',choices=unique(mtcars['cyl'])),
  filterModuleInput(ns('gear'),label = 'gear',choices=unique(mtcars['gear']))
  # actionButton(ns('resetFilter'),label='reset')
  )

}

filterSetResetButton <- function(id){

  ns <- NS(id)
    actionButton(ns('resetFilter'),label='reset')

}


newFilterSet <- function(id,data){
  callModule(filterSetModule,id,data)
}

ui <- fluidPage(
  #shinyjs is required to show/hide filters
  # useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      filterSetUI('fs1'),
      filterSetResetButton('fs1')
    ),
    mainPanel(
      DT::dataTableOutput("data")
    )
  )
)

server <- function(input, output,session) {

  # wrap data in reactive expression
  data <- reactive(mtcars)

  # Initilize filters
  # filters <- initializeFilterSet(filterSet, data)

  filters <- newFilterSet('fs1',data)
  # filters@filterResetModule()
  # observeEvent(input$reset,{
  #   filters$reset()
  # })

  # filterSet$output() contains the filtered data
  output$data <- DT::renderDataTable({
    filters()
  })

}

shinyApp(ui, server)
