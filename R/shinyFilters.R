
setClass('filterSet',
         representation(id = 'character',
                        filterList = 'list',
                        output = 'data.frame',
                        filterUINames = 'character')
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
  new('filterSet',id=id,filterList=list(),filterUINames = c('GF-filterMaster'))#)list(filterResetModule(id)))
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


#' filterUINames
#'
#' @param set
#'
#' @return
#' @export
#'
#' @examples
filterUINames <- function(filterSet){
  c(paste0(filterSet@id,'-',names(filterSet@filterList),'-filter'),
    paste0(filterSet@id,'-',names(filterSet@filterList),'-filterInvert'),
    paste0(filterSet@id,'-',names(filterSet@filterList),'-filterToggle'),
    paste0(filterSet@id,'-filterMaster')
    )
}

filterModuleInput <- function(id,label,invertOpt=F, hideOpt=F,...){
  ns <- NS(id)
  tl <- tagList(
    selectizeInput(ns('filter'),label = label,choices = character(0),multiple = T,...),
    if(invertOpt){
      checkboxInput(ns('filterInvert'),label = paste('Invert', label ,'Filter'),value = F)
    } else {
      NULL
    }
  )

  if(hideOpt){
    tagList(
      checkboxInput(ns('filterToggle'),label = paste('Show', label ,'Filter'),value = F),
      conditionalPanel(paste0("input.",'filterToggle'),tl,ns=ns)
    )
  } else {
    tl
  }

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
addSelectFilter <- function(set,id,fld_name=NULL,invertOpt=F, hideOpt=F,childCondition=NULL){
  # returns functions for creating server and UI components of filter
  force(id)
  force(fld_name)

  # reset = function() callModule(filterResetModule, set@id)#paste0(set@id,id))
  if(is.null(fld_name)) fld_name <- id
  # id <- paste0(set@id,id)
  newFilter <- list(UI=function(id,label,...) filterModuleInput(id,label,invertOpt, hideOpt,...),
                    Server=function(inputdf,reset=NULL, master = NULL,enabledByParent=NULL){
                      callModule(filterModule, id,inputdf, fld_name,
                                 reset, master,enabledByParent,childCondition=childCondition)},
                    # reset = function() callModule(filterResetModule, paste0(set@id,id)),
                    id=id,
                    fld_name=fld_name,
                    type='select',
                    invertOpt=invertOpt,
                    hideOpt=hideOpt)

  names <- names(set@filterList)
  set@filterList[[ length(set@filterList) + 1 ]] = newFilter
  names(set@filterList) <- c(names,id)

  uis <- head(set@filterUINames,length(set@filterUINames)-1)

  set@filterUINames <- c(uis,
                         c(paste(set@id,id,'filter',sep='-'),
                           if(hideOpt) paste(set@id,id,'filterToggle',sep='-'),
                           if(invertOpt) paste(set@id,id,'filterInvert',sep='-')),
                         paste(set@id,'filterMaster',sep='-'))


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
                    Server=function(inputdf,reset=NULL, master = NULL,enabledByParent=NULL){
                      callModule(filterSliderModule, id,inputdf, fld_name,
                                 reset, master, value,enabledByParent)},
                    # reset = function() callModule(filterResetModule, paste0(set@id,id)),
                    id=id,
                    fld_name=fld_name,
                    type='slider',
                    value=value)

  names <- names(set@filterList)
  set@filterList[[ length(set@filterList) + 1 ]] = newFilter
  names(set@filterList) <- c(names,id)

  uis <- head(set@filterUINames,length(set@filterUINames)-1)

  set@filterUINames <- c(uis,
                         paste(set@id,id,'filter',sep='-'),
                         paste(set@id,'filterMaster',sep='-'))

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
                    Server=function(inputdf,reset=NULL, master = NULL,enabledByParent=NULL){
                      callModule(filterSliderCustom, id,inputdf, fld_name,
                                 reset, master, value, mod,enabledByParent)},
                    # reset = function() callModule(filterResetModule, paste0(set@id,id)),
                    id=id,
                    fld_name=fld_name,
                    type='customSlider',
                    value=value,
                    mod=mod)

  names <- names(set@filterList)
  set@filterList[[ length(set@filterList) + 1 ]] = newFilter
  names(set@filterList) <- c(names,id)

  uis <- head(set@filterUINames,length(set@filterUINames)-1)

  set@filterUINames <- c(uis,
                         paste(set@id,id,'filter',sep='-'),
                         paste(set@id,'filterMaster',sep='-'))

  set

}

filterModule <- function(input, output, session, inputdf, fld_name,
                         reset, master, childCondition, enabledByParent){

  observe({
    # update select box when parent data frame changes
    sel <- isolate(input$filter)
    newchoices <-  select_(inputdf(),y=fld_name)$y %>% unique %>% sort
    if(!is.null(sel)){sel <- sel[sel%in%newchoices]}
    updateSelectInput(session,'filter',choices = newchoices,selected = sel)
  })

  disabledByParent <- reactive(!is.null(enabledByParent)&&!enabledByParent())
  inactive <- reactive(is.null(input$filter)||input$filter==''||disabledByParent())
  bypassed <- reactive(!is.null(master)&&!fld_name%in%master())
  # items <- reactive()
  invert <- reactive(!is.null(input$filterInvert)&&input$filterInvert)

  enableChild <- reactive({
    if(is.null(childCondition))
      return(TRUE)

    x <- input$filter

    eval(parse(text = childCondition))

  })

  observe({
    if(disabledByParent()){
      shinyjs::disable('filter')
    } else {
      shinyjs::enable('filter')
    }
  })

  observeEvent(reset(),{
    updateSelectInput(session,'filter',selected = character(0))
  })

  df <- reactive({
    # update child data when filter changes
    if(inactive()||bypassed()){
      return(inputdf())
    } else {
      i <- select_(inputdf(),y=fld_name)$y%in%input$filter
      if(invert()) i <- !i
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inactive=inactive,bypassed=bypassed,enableChild=enableChild))

}

filterSliderModule <- function(input, output, session, inputdf, fld_name,
                               reset, master, value,enabledByParent){

  disabledByParent <- reactive(!is.null(enabledByParent)&&!enabledByParent())
  inactive <- reactive(all(input$filter == value)||disabledByParent())
  bypassed <- reactive(!is.null(master)&&!fld_name%in%master())

  observeEvent(reset(),{
    # str(reset)
    # reset()
    updateSliderInput(session,'filter',value = value)
  })

  enableChild <- reactive(TRUE)

  observe({
    if(disabledByParent()){
      shinyjs::disable('filter')
    } else {
      shinyjs::enable('filter')
    }
  })

  df <- reactive({

    # update child data when filter changes
    if(inactive()||bypassed()){
      return(inputdf())
    } else {
      y <- select_(inputdf(),y=fld_name)$y
      i <- !is.na(y)&y>=(input$filter[1])&y<=(input$filter[2])
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inactive=inactive,bypassed=bypassed,enableChild=enableChild))

}

filterSliderCustom <- function(input, output, session, inputdf, fld_name,
                               reset, master, value,mod,enabledByParent){

  v <- c(value,Inf)
  items <- reactive(v[input$filter+1])

  disabledByParent <- reactive(!is.null(enabledByParent)&&!enabledByParent())
  inactive <- reactive(all(input$filter==c(0,length(value)))||disabledByParent())
  bypassed <- reactive(!is.null(master)&&!fld_name%in%master())

  observeEvent(reset(),{
    # str(reset)
    # reset()
    updateSliderInput(session,'filter',value=c(min(value),max(value)))
  })

  enableChild <- reactive(TRUE)

  observe({
    if(disabledByParent()){
      shinyjs::disable('filter')
    } else {
      shinyjs::enable('filter')
    }
  })

  df <- reactive({

    vals <- v[input$filter+1]
    # update child data when filter changes
    if(inactive()|bypassed()){
      return(inputdf())
    } else {
      y <- select_(inputdf(),y=as.name(fld_name))$y
      i <- !is.na(y)&y>=(vals[1]*mod)&y<=(vals[2]*mod)
      return(inputdf()[i,])
    }
  })

  return(list(name=fld_name,data=df,inactive=inactive,bypassed=bypassed,enableChild=enableChild))
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
    active <- !lapply(fl, function(f) f$out$inactive()) %>% unlist()
    updateSelectInput(session,'filterMaster',choices = fl_names[active],
                      selected = fl_names[active])
  })

  fl[[1]]$out = fl[[1]]$Server(data,reset=r,master=m)
  # for(i in (2:length(fl))){
  lapply(2:length(fl), function(i){
    # reset <-
    fl[[i]]$out <<- fl[[i]]$Server(inputdf=fl[[i-1]]$out$data,
                                   enabledByParent=fl[[i-1]]$out$enableChild,
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



#' printActiveFilters
#'
#' @param input
#' @param set
#'
#' @return
#' @export
#'
#' @examples
printActiveFilters <- function(set,input){

  fl <- set@filterList
  names(fl) <- lapply(fl, function(f) f$fld_name)

  af <- fl[input[[paste0(set@id,'-filterMaster')]]]


  print.filter <- function(f){
    fn <- paste(set@id,f$id,'filter',sep='-')
    vals <- input[[fn]]
    if(f$type=='select'){
      id = f$id
      if(f$invertOpt&&input[[paste(set@id,f$id,'filterInvert',sep='-')]]){
        id <- paste(id,'(inverted)')
      }
      paste0(id,': ',paste0(vals,collapse = '; '))
    } else if(f$type=='slider'){
      paste0(f$id,': ',paste0(vals,collapse = '; '))
    } else if(f$type=='customSlider'){
      vals <- f$value[vals+1]
      paste0(f$id,': ',paste0(vals,collapse = '; '))
    }
  }


  lapply(af, print.filter) %>% unlist

}
