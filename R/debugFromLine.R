debug.from.line <- function(..., state = F) {
  # Collect the arguments passed to the function
  args <- list(...)

  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  if (length(args) < 1) {
    # show the state of all variables at the end of execution
    state = TRUE # what if user sets state to false?
  } else if (length(args) == 1) {

  }

}

.grab.line <- function(lineNumber, state) {

  data.nodes <- get.data.nodes()
  proc.nodes <- get.proc.nodes()

  if (!state) {

    # doesn't account for multiple vars on one line
    ## multiple scripts
    ## multiple references
    # should check if line number is valid entry

    # Node
    node <- proc.nodes[proc.nodes$startLine == lineNumber, "label"]
    node <- node[!is.na(node)]

    # Script
    script <- proc.nodes[proc.nodes$startLine == lineNumber, "scriptNum"]
    script <- script[!is.na(script)]

    # Valw
    proc.data.edges <- get.proc.data()
    entity <- proc.data.edges[proc.data.edges$activity == node, "entity"]
    val <- data.nodes[data.nodes$label == entity, "value"]

    # Var
    var <- data.nodes[data.nodes$label == entity, "name"]

    # Type
    type <- NULL
    val.type <- fromJSON(data.nodes[data.nodes$label == entity, "valType"])
    if (val.type$container == "vector") {
      type <- val.type$type
      if (type == "numeric") {
        type <- typeof(as.numeric(val))
      }
    } else if (val.type$container == "data_frame") {
      type <- cat("Data Frame", val.type$dimension, sep = ", ")
    }

    line.reference <- as.data.frame(cbind(var, val, type, script))
    return(line.reference)

  } else {
    return(NULL)
  }

}
