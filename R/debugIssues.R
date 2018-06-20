# This function is a simple wrapper around the data.lineage function to show
# how an error was produced
debug.error.trace <- function(stack.overflow = F) {
  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  if(stack.overflow) {
    warning("stack overflow functionality is currently not supported")
  }

  debug.lineage("error.msg")$error.msg
}

# This function operates similarily to debug.lineage; however,
# it uses just the warning messages. The whole debug.lineage function
# cannot be reused here as it relies on variables having unique
# names. warnings do not have unique names and there can be multiple
# warnings unlike error messages.
debug.warning.trace <- function(..., stack.overflow = F) {
  args <- list(...)

  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  # Grab all the warning rows from the provenance
  pos.vars <- get.data.nodes()
  pos.vars <- pos.vars[pos.vars$name == "warning.msg", ]
  row.names(pos.vars) <- 1:nrow(pos.vars)

  node.labels <- as.list(pos.vars$label)

  # Extract the warning messages to display to the user
  # as options, the length will help determine whether or
  # not a valid result was input as an arg
  pos.results <- as.list(pos.vars$value)
  num.results <- 1:length(pos.results)

  # Checks each arg to make sure it is valid
  # producing a vector of logicals corresponding with
  # valid/invalid input
  pos.args <- lapply(args, function(arg){
    if(arg %in% num.results) {
      return(TRUE)
    } else {
      warning(paste(arg, " is not a possible result"))
      return(FALSE)
    }
  })

  # Any non-valid inputs will be removed as the list is subset
  # by logicals, TRUE corresponding to valid inputs
  args <- args[unlist(pos.args)]

  # If they did not pass any arguments to the function
  # then print the possible arguments they can input
  if(length(args) == 0) {
    cat("Possible results: \n")
    print(pos.vars$value)
    cat("Pass the corresponding index value to the function for info on that warning")
  } else {
    # The procedure nodes are used in the .proccess.label fucntion
    # to find script and line numbers and code
    proc.nodes <- get.proc.nodes()

    # Each of the chosen warning message needs to be processed,
    dfs <- lapply(args, function(arg){
      .process.label(pos.vars[arg, ]$label, proc.nodes, forward = F)
    })

    if(stack.overflow) {
      warning("stack overflow functionality is currently not supported")
    }

    return(dfs)
  }
}


