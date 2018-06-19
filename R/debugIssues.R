debug.error.trace <- function(stack.overflow = F) {
  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }
  
  if(stack.overflow) {
    warning("stack overflow functionality is currently not supported")
  }
  debug.lineage("error.msg")
}

# This function operates similarily to debug.lineage; however,
# it uses just the warning messages. The debug.lineage function 
# cannot be reused here as it relies on variables having unique
# names. warnings do not have unique names and there can be multiple
# warnings unlike error messages
debug.warning.trace <- function(..., stack.overflow = F) {
  args <- list(...)
  
  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }
  
  # Grab all the warning rows from the provenance 
  pos.vars <- get.data.nodes()
  pos.vars <- pos.vars[pos.vars$name == "warning.msg", ]
  
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
  
  
  
  if(stack.overflow) {
    warning("stack overflow functionality is currently not supported")
  }
  
  
}

.process.label <- function(label) {
  # Grab the nodes that have connections to the chosen node from the adj graph
  spine <- get.spine(label)
  
  # Pull the lines from the proc nodes that are referenced
  # in the spine, each is stored as a row
  lines <- lapply(spine[grep("p[[:digit:]]", spine)], function(proc.node) {
    list(proc.nodes[proc.nodes$label == proc.node, ]$startLine,
         proc.nodes[proc.nodes$label == proc.node, ]$name)
  })
  
  # Since each line is stored as a row and the wanted result is a
  # data frame, the columns need to be extracted so they can
  # be covnerted to a data frame. Find how many columns there
  # are so mapply can index through the rows the correct amount of times
  col.length <- 1:length(lines[[1]])
  
  # Grab each column as a list from the rows
  # then extract the values so they're a vector
  # when those columns are returned as a list of vectors
  # create a data frame from it
  df <- data.frame(lapply(col.length, function(x) {
    col <- mapply(`[`, lines, x)
    return(mapply(`[`, col, 1))
  }), stringsAsFactors = F)
  
  
}
