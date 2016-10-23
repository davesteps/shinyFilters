
#' filterModuleInput
#'
#' @param id
#' @param label
#' @param choices
#' @param multiple
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
filterModuleInput <- function(id,label,choices='',multiple=T,...){
  ns <- NS(id)
  selectizeInput(ns('filter'),label = label,choices,multiple=multiple,...)
}

#' filterSliderInput
#'
#' @param id
#' @param label
#' @param defaults
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
filterSliderInput <- function(id,label,defaults,...){
  ns <- NS(id)
  sliderInput(ns('filter'),label = label,min = defaults[1],max = defaults[2],value = defaults)
}

#' filterModule
#'
#' @param input
#' @param output
#' @param session
#' @param inputdf
#' @param fld_name
#' @param childCondition
#' @param bypass
#'
#' @return
#' @export
#'
#' @examples
filterModule <- function(input, output, session, inputdf, fld_name,childCondition,bypass){


  observe({
    # update select box when parent data frame changes
    sel <- isolate(input$filter)
    newchoices <-  select_(inputdf(),y=fld_name)$y %>% unique %>% sort
    if(!is.null(sel)){sel <- sel[sel%in%newchoices]}
    updateSelectInput(session,'filter',choices = newchoices,selected = sel)
  })


  bypassed <- reactive(ifelse(is.null(bypass),FALSE,bypass()))
  inActive <- reactive(is.null(input$filter)||bypassed())
  # bypassed <- reactive(!fld_name%in%act.flts())
  # items <- reactive(input$filter)
  disableChild <- reactive(ifelse(is.null(childCondition),FALSE,!all(childCondition%in%input$filter)))


  # str(bypass)
  # observeEvent(disableME(), print(disableME()))

  observe({
    if(bypassed()){
      shinyjs::hide('filter')
    } else {
      shinyjs::show('filter')
    }
  })

  df <- reactive({
    # update child data when filter changes
    if(inActive()){
      return(inputdf())
    } else {
      i <- select_(inputdf(),y=fld_name)$y%in%input$filter
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inActive=inActive,disableChild=disableChild))

}

#' filterSlider
#'
#' @param input
#' @param output
#' @param session
#' @param defaults
#' @param inputdf
#' @param fld_name
#' @param bypass
#'
#' @return
#' @export
#'
#' @examples
filterSlider <- function(input, output, session,defaults, inputdf,fld_name,bypass){

  inActive <- reactive(all(input$filter==defaults))
  # bypassed <- reactive(fld_name%in%act.flts())
  # items <- reactive(input$filter)
  disableChild <- reactive(F)
  df <- reactive({

    # update child data when filter changes
    if(inActive()){
      return(inputdf())
    } else {
      y <- select_(inputdf(),y=fld_name)$y
      i <- y>=(input$filter[1])&y<=(input$filter[2])
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inActive=inActive,disableChild=disableChild))

}

#' clearFilter
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
clearFilter <- function(input, output, session){
  updateSelectInput(session,'filter',selected = character(0))
}
#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param defaults
#'
#' @return
#' @export
#'
#' @examples
clearSlider <- function(input, output, session, defaults){
  updateSliderInput(session,'filter',value = defaults)
}



#' selectFilter
#'
#' @param id
#' @param fld_name
#' @param childCondition
#'
#' @return
#' @export
#'
#' @examples
selectFilter <- function(id,fld_name=NULL,childCondition=NULL){
  # returns functions for creating server and UI components of filter
  force(id)
  force(fld_name)
  if(is.null(fld_name)) fld_name <- id
  list(UI=function(label,...) filterModuleInput(id,label,...),
       Server=function(inputdf,bypass=NULL) callModule(filterModule, id,inputdf, fld_name, childCondition,bypass),
       reset = function() callModule(clearFilter, id),
       id=id)


}


#' sliderFilter
#'
#' @param id
#' @param fld_name
#' @param defaults
#' @param childCondition
#'
#' @return
#' @export
#'
#' @examples
sliderFilter <- function(id,fld_name=NULL,defaults=NULL,childCondition=NULL){
  # returns functions for creating server and UI components of filter
  force(id)
  force(fld_name)
  if(is.null(fld_name)) fld_name <- id

  list(UI=function(label,...) filterSliderInput(id,label,defaults,...),
       Server=function(inputdf,bypass=NULL) callModule(filterSlider, id, defaults, inputdf, fld_name, bypass),
       reset = function() callModule(clearSlider, id, defaults),
       id=id)


}

#' initFilters
#'
#' @param fl
#' @param data
#'
#' @return
#' @export
#'
#' @examples
initFilters <- function(fl, data){

  fl[[1]]$out = fl[[1]]$Server(data)
  lapply(2:length(fl),function(i){
    fl[[i]]$out <<- fl[[i]]$Server(inputdf=fl[[i-1]]$out$data,
                                   bypass=fl[[i-1]]$out$disableChild)
  })
  list(fl=fl,
       reset = function() lapply(fl,function(f) f$reset()),
       output = last(fl)$out$data)
}


#' filterInput
#'
#' @param filter A filter created with either sliderFilter or selectFilter
#' @param ... Arguments passed to either sliderInput or selectizeInput
#'
#' @return
#' @export
#'
#' @examples
filterInput <- function(filter, label=NULL, ...){

  if(is.null(label)) label <- filter$id
  filter$UI(label=label,...)

}
