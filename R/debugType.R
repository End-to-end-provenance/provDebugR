debug.variable.type <- function(..., just.logical = F) {
  # Collect the arguments passed to the function
  args <- list(...)

  # In case they also entered a list as an argument
  # the list should be extracted so that we're left with
  # only single elements
  flat.args <- list()

  # Extract everything and append it to the temp list
  # appending will be able to unnest any passed lists
  lapply(args, function(arg){
    flat.args <<- append(flat.args, arg)
  })

  args <- flat.args

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
    return(unlist(pos.vars))
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

  # These data node are the rows that math with the provided result
  res.dat.nodes <- data.nodes[data.nodes$name == result, ]

  # These procedure labels are those that have connections with the chosen result nodes
  res.proc.labels <- proc.data[proc.data$entity %in% res.dat.nodes$label, ]$activity

  # These procedure nodes are the ones that have connections with the chosen result nodes
  res.proc.rows <- proc.nodes[proc.nodes$label %in% res.proc.labels, ]

  # Now that the data and procedure nodes have been narrowed down to only the
  # requested information, the information that will be provided to the user
  # is grabbed
  script <- res.proc.rows$scriptNum
  line <- res.proc.rows$startLine
  scope <- res.dat.nodes$scope

  # Since the type is stored as a json in the data frame, the type needs to be extraced
  # If it is a data frame, inform the user of its dimensions
  type <- unlist(lapply(res.dat.nodes$valType, function(type){
    valType <- jsonlite::fromJSON(type)
    if(valType$container == "data_frame") {
      return(paste(valType$container,
                   ", with dimensions: ",
                   valType$dimension[1],
                   ",",
                   valType$dimension[2],
                   sep=""))
    } else {
      return(valType$type)
    }
    }))

  # Combine all the data and return to the user
  df <- as.data.frame(cbind(script, line, scope, type), stringsAsFactors = F)
}
