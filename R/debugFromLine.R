# This file is a part of provDebugR
# Contact: Orenna Brand <o.brand@columbia.edu>

debug.from.line <- function(..., state = F) {
  # Collect the arguments passed to the function
  args <- list(...)

  # In case they also entered a list as an argument
  # the list should be extracted so that we're left with
  # only single elements
  flat.args <- list()

  # Extract everything and append it to the temp list
  # Appending will be able to unnest any passed lists
  lapply(args, function(arg){
    flat.args <<- append(flat.args, arg)
  })

  args <- flat.args

  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  # Procedure nodes have start line
  proc.nodes <- get.proc.nodes()
  ### NOTE: can I subset proc.nodes to only have rows with type operation?
  ### proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
  ### NA values are getting in the way

  # Check if line number is valid entry
  pos.line <- proc.nodes[, "startLine"]
  pos.line <- pos.line[!is.na(pos.line)]

  pos.args <- lapply(args, function(arg, pos.line) {
    if (arg %in% pos.line) {
      return(TRUE)
    } else {
      warning(paste(arg, " is not a possible line"))
      return(FALSE)
    }
  }, pos.line = pos.line)

  args <- args[unlist(pos.args)]

  # If parameter is blank, show state of all variables at end of execution
  # Otherwise, call helper function .grab.line over each line input
  if (length(args) == 0) {
    #max.node <- proc.nodes[proc.nodes$startLine == max(pos.line), "label"]
    #max.node <- max.node[!is.na(max.node)]
    #ret.val <- .process.node(max.node) # wouldn't work if multiple scripts have same max line
    print("State of all variables at end of execution")
    max.line <- max(pos.line)
    ret.val <- .grab.line(max.line, state = T)
    return(ret.val)
  } else {
    ret.val <- lapply(args, .grab.line, state)
    names(ret.val) <- args
    return(ret.val)
  }
}

.grab.line <- function(lineNumber, state) {

  # Procedure nodes have start line and script number
  # Data nodes have var, val, and type
  proc.nodes <- get.proc.nodes()
  data.nodes <- get.data.nodes()

  # Edges decribe connections between data and proc nodes
  # and between proc and data nodes
  data.proc.edges <- get.data.proc()
  proc.data.edges <- get.proc.data()

  if (!state) {
    # Nodes (possible to have more than one)
    nodes <- proc.nodes[proc.nodes$startLine == lineNumber, "label"]
    nodes <- nodes[!is.na(nodes)]

    # Add to list of nodes those that are referenced on the line
    ref.nodes <- NULL
    ref.nodes <- lapply(nodes, function(node) {
      ref.entity <- data.proc.edges[data.proc.edges$activity == node, "entity"]
      ref.node <- NA
      if (!length(ref.entity) == 0) {
        ref.node <- proc.data.edges[proc.data.edges$entity == ref.entity, "activity"]
      }
      return(ref.node)
    })

    nodes <- c(nodes, ref.nodes)
    nodes <- nodes[!is.na(nodes)]

    # Create row for each variable on the line, then rbind into a data frame
    line.df <- NULL
    line.df <- lapply(nodes, function(node) {

      # Extract data entity from procedure activity via procedure-to-data edges
      entity <- proc.data.edges[proc.data.edges$activity == node, "entity"]

      val <- var <- type <- NULL
      if (length(entity) == 0) {
        val <- var <- type <- NA # give some info (code? --> new column?)
      } else {
        # Var
        var <- data.nodes[data.nodes$label == entity, "name"]

        # Val
        val <- data.nodes[data.nodes$label == entity, "value"]

        # Type
        val.type <- fromJSON(data.nodes[data.nodes$label == entity, "valType"])
        if (val.type$container == "vector") {
          type <- val.type$type
          if (type == "numeric") {
            type <- typeof(as.numeric(val))
          }
          # Need to account for other types
        } else if (val.type$container == "data_frame") {
          type <- paste("data frame:", val.type$dimension[1], "x", val.type$dimension[2])
        }
      }

      # Script
      script <- proc.nodes[proc.nodes$label == node, "scriptNum"]

      line.row <- c(var, val, type, script)
      line.df <- cbind(line.df, line.row) ## cbind or rbind?
    })

    line.df <- as.data.frame(line.df)
    rownames(line.df) <- c("var", "val", "type", "script")
    colnames(line.df) <- c(1:length(nodes))
    line.df <- t(line.df)
    return(line.df)

  } else {

    node <- proc.nodes[proc.nodes$startLine == lineNumber, "label"]
    node <- node[!is.na(node)]

    # Extract data entity from procedure activity via procedure-to-data edges
    proc.data.edges <- get.proc.data()
    entity <- proc.data.edges[proc.data.edges$activity == node, "entity"]

    # why not using startLine at all?
    rname <- rownames(data.nodes[data.nodes$label == entity, ])
    nodes <- data.nodes["1":rname, "label"]

    line.df <- NULL
    line.df <- lapply(nodes, function(node) {

      val <- var <- type <- NULL
      if (length(node) == 0) {
        val <- var <- type <- NA # give some info (code? --> new column?)
      } else {
        # Var
        var <- data.nodes[data.nodes$label == node, "name"]

        # Val
        val <- data.nodes[data.nodes$label == node, "value"]

        # Type
        val.type <- fromJSON(data.nodes[data.nodes$label == node, "valType"])
        if (val.type$container == "vector") {
          type <- val.type$type
          if (type == "numeric") {
            type <- typeof(as.numeric(val))
          }
          # Need to account for other types
        } else if (val.type$container == "data_frame") {
          type <- paste("data frame:", val.type$dimension[1], "x", val.type$dimension[2])
        }
      }

      line.row <- c(var, val, type)
      line.df <- cbind(line.df, line.row) ## cbind or rbind?
    })

    line.df <- as.data.frame(line.df)
    rownames(line.df) <- c("var", "val", "type")
    colnames(line.df) <- c(1:length(nodes))
    line.df <- t(line.df)
    return(line.df)
  }
}


.process.node <- function(node) {
  # should create for generalizing
}
