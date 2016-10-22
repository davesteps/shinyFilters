library(shiny)
library(shinyjs)
library(dplyr)
library(shinyFilters)


# TODo --------------------------------------------------------------------

# parent filter rather than df
# master box


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
