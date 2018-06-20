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

  # Check if line number is valid entry
  pos.line <- proc.nodes[, "startLine"]
  pos.line <- pos.line[!is.na(pos.line)]
  pos.line <- as.list(unique(pos.line)) # do I want a list?

  pos.args <- lapply(args, function(arg, pos.line) {
    if (arg %in% pos.line) {
      return(TRUE)
    } else {
      warning(paste(arg, " is not a possible line"))
      return(FALSE)
    }
  }, pos.line = pos.line)

  args <- args[unlist(pos.args)]

  if (length(args) == 0) {
    # show list of all variables at end of execution
  } else {
    ls <- lapply(args, .grab.line, state)
    name(ls) <- args
    return(ls)
  }

}

.grab.line <- function(lineNumber, state) {

  data.nodes <- get.data.nodes()
  proc.nodes <- get.proc.nodes()

  if (!state) {

    # doesn't account for multiple vars on one line
    ## multiple scripts
    ## multiple references


    # Node
    node <- proc.nodes[proc.nodes$startLine == lineNumber, "label"]
    node <- node[!is.na(node)]

    # Script
    script <- proc.nodes[proc.nodes$startLine == lineNumber, "scriptNum"]
    script <- script[!is.na(script)]

    # Val
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
