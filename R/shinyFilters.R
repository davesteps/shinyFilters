
setClass('filterSet',
         representation(id = 'character',
                        filterList = 'list',
                        output = 'data.frame')
         # prototype=c(fundInfo=NA,cashflow=NA,FX=NA)
)

#' newFilterSet
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
newFilterSet <- function(id='myFilters'){
  new('filterSet',id=id,filterList=list())#)list(filterResetModule(id)))
}

#' filterInput
#'
#' @param set
#' @param filterid
#' @param label
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
filterInput <- function(set, filterid, label=NULL, ...){
  filter <- set@filterList[[filterid]]
  ns <- NS(set@id)
  if(is.null(label)) label <- filterid
  filter$UI(id = ns(filterid),label=label,...)

}

filterModuleInput <- function(id,label,choices='',multiple=T,...){
  ns <- NS(id)
  selectizeInput(ns('filter'),label = label,choices,multiple=multiple,...)
}

filterSliderInput <- function(id,label,value,step=1){
  ns <- NS(id)
  sliderInput(ns('filter'),label = label,min = value[1],max = value[2],value = value,step=step)
}

filterSliderCustomInput <- function(id,label,value){
  ns <- NS(id)

  # args       <- list(inputId=ns('filter'), label=label, ticks=c(seq(0,2000,50),">2000"), value=c(0,2000))
  args       <- list(inputId=ns('filter'), label=label, ticks=c(value,paste0('>',max(value))),
                     value=c(min(value),max(value)))

  args$min   <- 1
  args$max   <- length(args$ticks)

  if (sessionInfo()$otherPkgs$shiny$Version>="0.11") {

    ticks <- paste0(args$ticks, collapse=',')
    args$ticks <- T
    html  <- do.call('sliderInput', args)

    html$children[[2]]$attribs[['data-values']] <- ticks;

  } else {
    html  <- do.call('sliderInput', args)
  }

  html
}

#' filterResetButton
#'
#' @param set
#' @param label
#'
#' @return
#' @export
#'
#' @examples
filterResetButton <- function(set,label='Reset Filters',...){
  id <- set@id
  ns <- NS(id)
  actionButton(ns('resetFilter'),label,...)
}

#' filterInputs
#'
#' @param set
#'
#' @return
#' @export
#'
#' @examples
filterInputs <- function(set){
  lapply(names(set@filterList), function(n) filterInput(set,n))
}



#' filterMaster
#'
#' @param set
#' @param label
#' @param choices
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
filterMaster <- function(set,label='Active Filters',choices='',...){
  id <- set@id
  ns <- NS(id)
  selectizeInput(ns('filterMaster'),label = label,choices,multiple=T,...)
}

#' addSelectFilter
#'
#' @param set
#' @param id
#' @param fld_name
#' @param childCondition
#'
#' @return
#' @export
#'
#' @examples
addSelectFilter <- function(set,id,fld_name=NULL,childCondition=NULL){
  # returns functions for creating server and UI components of filter
  force(id)
  force(fld_name)

  # reset = function() callModule(filterResetModule, set@id)#paste0(set@id,id))
  if(is.null(fld_name)) fld_name <- id
  # id <- paste0(set@id,id)
  newFilter <- list(UI=function(id,label,...) filterModuleInput(id,label,...),
                    Server=function(inputdf,reset=NULL, master = NULL){
                      callModule(filterModule, id,inputdf, fld_name,
                                 reset, master)},
                    # reset = function() callModule(filterResetModule, paste0(set@id,id)),
                    id=id,
                    fld_name=fld_name)

  names <- names(set@filterList)
  set@filterList[[ length(set@filterList) + 1 ]] = newFilter
  names(set@filterList) <- c(names,id)

  set

}

#' addSliderFilter
#'
#' @param set
#' @param id
#' @param fld_name
#' @param value
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
addSliderFilter <- function(set,id,fld_name=NULL, value, ...){
  # returns functions for creating server and UI components of filter
  force(id)
  force(fld_name)
  force(value)

  if(is.null(fld_name)) fld_name <- id

  newFilter <- list(UI=function(id,label,...) filterSliderInput(id,label,value=value,...),
                    Server=function(inputdf,reset=NULL, master = NULL){
                      callModule(filterSliderModule, id,inputdf, fld_name,
                                 reset, master, value)},
                    # reset = function() callModule(filterResetModule, paste0(set@id,id)),
                    id=id,
                    fld_name=fld_name)

  names <- names(set@filterList)
  set@filterList[[ length(set@filterList) + 1 ]] = newFilter
  names(set@filterList) <- c(names,id)

  set

}

#' addCustomSliderFilter
#'
#' @param set
#' @param id
#' @param fld_name
#' @param value
#' @param mod
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
addCustomSliderFilter <- function(set,id,fld_name=NULL, value, mod=1, ...){
  # returns functions for creating server and UI components of filter
  force(id)
  force(fld_name)
  force(value)

  if(is.null(fld_name)) fld_name <- id

  newFilter <- list(UI=function(id,label,...) filterSliderCustomInput(id,label,value=value,...),
                    Server=function(inputdf,reset=NULL, master = NULL){
                      callModule(filterSliderCustom, id,inputdf, fld_name,
                                 reset, master, value, mod)},
                    # reset = function() callModule(filterResetModule, paste0(set@id,id)),
                    id=id,
                    fld_name=fld_name)

  names <- names(set@filterList)
  set@filterList[[ length(set@filterList) + 1 ]] = newFilter
  names(set@filterList) <- c(names,id)

  set

}

filterModule <- function(input, output, session, inputdf, fld_name,
                         reset, master){

  observe({
    # update select box when parent data frame changes
    sel <- isolate(input$filter)
    newchoices <-  select_(inputdf(),y=fld_name)$y %>% unique %>% sort
    if(!is.null(sel)){sel <- sel[sel%in%newchoices]}
    updateSelectInput(session,'filter',choices = newchoices,selected = sel)
  })

  # disabled <- reactive(!is.null(parentCondition)&&!parentCondition())
  inActive <- reactive(is.null(input$filter)||input$filter=='')#||disabled())
  bypassed <- reactive(!is.null(master)&&!fld_name%in%master())
  # items <- reactive(input$filter)
  # invert <- reactive(!is.null(input$filterInvert)&&input$filterInvert)

  # child <- reactive({
  #   if(is.null(childCondition))
  #     return(FALSE)
  #
  #   x <- items()
  #
  #   eval(parse(text = childCondition))
  #
  # })

  # observe({
  #   if(disabled()){
  #     shinyjs::hide('filter')
  #   } else {
  #     shinyjs::show('filter')
  #   }
  # })

  observeEvent(reset(),{
    # str(reset)
    # reset()
    updateSelectInput(session,'filter',selected = character(0))
  })

  df <- reactive({
    # update child data when filter changes
    if(inActive()||bypassed()){
      return(inputdf())
    } else {
      i <- select_(inputdf(),y=fld_name)$y%in%input$filter
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inActive=inActive,bypassed=bypassed))

}

filterSliderModule <- function(input, output, session, inputdf, fld_name,
                               reset, master, value){

  inActive <- reactive((input$filter[1] == value[1]) && (input$filter[2] == value[2]))#||disabled())
  bypassed <- reactive(!is.null(master)&&!fld_name%in%master())

  observeEvent(reset(),{
    # str(reset)
    # reset()
    updateSliderInput(session,'filter',value = value)
  })

  df <- reactive({

    # update child data when filter changes
    if(inActive()||bypassed()){
      return(inputdf())
    } else {
      y <- select_(inputdf(),y=fld_name)$y
      i <- !is.na(y)&y>=(input$filter[1])&y<=(input$filter[2])
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inActive=inActive,bypassed=bypassed))

}

filterSliderCustom <- function(input, output, session, inputdf, fld_name,
                               reset, master, value,mod){

  v <- c(value,Inf)
  items <- reactive(v[input$filter+1])

  inActive <- reactive(all(input$filter==c(0,length(value))))
  bypassed <- reactive(!is.null(master)&&!fld_name%in%master())

  observeEvent(reset(),{
    # str(reset)
    # reset()
    updateSliderInput(session,'filter',value=c(min(value),max(value)))
  })

  df <- reactive({

    vals <- v[input$filter+1]
    # update child data when filter changes
    if(inActive()|bypassed()){
      return(inputdf())
    } else {
      y <- select_(inputdf(),y=as.name(fld_name))$y
      i <- !is.na(y)&y>=(vals[1]*mod)&y<=(vals[2]*mod)
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inActive=inActive,bypassed=bypassed))
  # return(list(items=items))
}

initFilterSet <- function(input, output, session, set, data){

  # print(set@id)
  r <- reactive(input$resetFilter)
  m <- reactive(input$filterMaster)

  fl <- set@filterList

  fl_names <- lapply(fl, function(f) f$fld_name) %>%
    unlist() %>%
    setNames(names(fl))

  observe({
    active <- !lapply(fl, function(f) f$out$inActive()) %>% unlist()
    updateSelectInput(session,'filterMaster',choices = fl_names[active],
                      selected = fl_names[active])
  })

  fl[[1]]$out = fl[[1]]$Server(data,reset=r,master=m)
  # for(i in (2:length(fl))){
  lapply(2:length(fl), function(i){
    # reset <-
    fl[[i]]$out <<- fl[[i]]$Server(inputdf=fl[[i-1]]$out$data,
                                   # bypass=fl[[i-1]]$out$disableChild,
                                   reset=r,
                                   master=m)
  })



  list(fl,
       output = last(fl)$out$data)


}

#' initializeFilterSet
#'
#' @param set
#' @param data
#'
#' @return
#' @export
#'
#' @examples
initializeFilterSet <- function(set, data){

  callModule(initFilterSet,id = set@id, set, data)#paste0(set@id,id))

}
