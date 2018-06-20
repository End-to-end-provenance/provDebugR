debug.variable.type <- function(..., just.logical = F) {
  # Collect the arguments passed to the function
  args <- list(...)
  
  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }
  
  # Collect possible results the user could ask for
  # Remove any rows that aren't variables
  pos.vars <- get.data.nodes()
  pos.vars <- pos.vars[pos.vars$type == "Data" | pos.vars$type == "Snapshot", ]
  pos.vars <- as.list(unique(pos.vars$name))
  
  # Make sure all the results passed by the user are valid
  # this produces a list of logicals, where TRUES
  #correspond to valid inputs
  pos.args <- lapply(args, function(arg){
    if(arg %in% pos.vars) {
      return(TRUE)
    } else {
      warning(paste(arg, " is not a possible result"))
      return(FALSE)
    }
  })
  
  # Any non-valid inputs will be removed as the list is subset
  # by logicals, TRUE corresponding to valid inputs
  args <- args[unlist(pos.args)]
  
  # If they did not provide any results themselve, list them out for them
  if(length(args) == 0) {
    cat("Possible results:\n")
    print(unlist(pos.vars))
  } else {
    ret.val <- lapply(args, .grab.instances)
    names(ret.val) <- args
    return(ret.val)
  }
}

.grab.instances <- function(result) {
  # The data nodes have all the information on the variables
  data.nodes <- get.data.nodes()
  proc.nodes <- get.proc.nodes()
  proc.data <- get.proc.data()
  
  res.dat.nodes <- data.nodes[data.nodes$name == result, ]
  res.proc.labels <- proc.data[proc.data$entity %in% res.dat.nodes$label, ]$activity
  
  res.proc.rows <- proc.nodes[proc.nodes$label %in% res.proc.labels, ]
  
  script <- res.proc.rows$scriptNum
  line <- res.proc.rows$startLine
  scope <- res.dat.nodes$scope
  
  type <- unlist(lapply(res.dat.nodes$valType, function(type){
    valType <- jsonlite::fromJSON(type)
    if(valType$container == "data_frame") {
      return(paste(valType$container, ", with dimensions: ", valType$dimension[1], ",", valType$dimension[2], sep=""))
    } else {
      return(valType$type)
    }
    }))
  
  df <- as.data.frame(cbind(script, line, scope, type), stringsAsFactors = F)
}

debug.from.type <- function(variable, type) {
  cat("Not yet implemented")
}