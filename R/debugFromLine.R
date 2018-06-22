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
  if(!.debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  # Get procedure nodes (and thus startLine and scriptNum) from parser
  # Subset operation-type nodes to get rid of NA values
  proc.nodes <- get.proc.nodes()
  .debug.env$proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]

  # Get data nodes (and thus var, val, and type) from parser
  # Get edges that describe connections between nodes
  .debug.env$data.nodes <- get.data.nodes()
  .debug.env$proc.data.edges <- get.proc.data()
  .debug.env$data.proc.edges <- get.data.proc()

  # Check if line number is valid entry
  pos.line <- .debug.env$proc.nodes[, "startLine"]
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
    print("State of all variables at end of execution:")
    ret.val <- .grab.line(max(pos.line), state = T)
    return(ret.val)
  } else {
    ret.val <- lapply(args, .grab.line, state)
    names(ret.val) <- args
    return(ret.val)
  }
}

.grab.line <- function(lineNumber, state) {

  .debug.env$line.df <- data.frame()

  if (!state) { # REFERENCE
    nodes <- .debug.env$proc.nodes[.debug.env$proc.nodes$startLine == lineNumber, "label"]

    # Add to list of nodes those that are referenced on the line
    ref.nodes <- lapply(nodes, function(node) {
      ref.entity <- .debug.env$data.proc.edges[.debug.env$data.proc.edges$activity == node, "entity"]
      ref.node <- NA
      if (!length(ref.entity) == 0) {
        ref.node <- .debug.env$proc.data.edges[.debug.env$proc.data.edges$entity == ref.entity, "activity"]
      }
      return(ref.node)
    })
    nodes <- c(nodes, ref.nodes)
    nodes <- nodes[!is.na(nodes)]

    # Create row for each variable on the line, then rbind into a data frame
    lapply(nodes, .process.node)

    colnames(.debug.env$line.df) <- c("var", "val", "type", "script")
    rownames(.debug.env$line.df) <- c(1:length(nodes))

    return(.debug.env$line.df)

  } else { # STATE
    node <- .debug.env$proc.nodes[.debug.env$proc.nodes$startLine == lineNumber, "label"]

    # Extract data entity from procedure activity via procedure-to-data edges
    entity <- .debug.env$proc.data.edges[.debug.env$proc.data.edges$activity == node, "entity"]

    # why not using startLine at all?
    rname <- rownames(.debug.env$data.nodes[.debug.env$data.nodes$label == entity, ])
    nodes <- .debug.env$data.nodes["1":rname, "label"]

    line.df <- NULL
    line.df <- lapply(nodes, function(node) {
      val <- var <- type <- NULL
      if (length(node) == 0) {
        val <- var <- type <- NA # give some info (code? --> new column?)
      } else {
        # Var
        var <- .debug.env$data.nodes[.debug.env$data.nodes$label == node, "name"]

        # Val
        val <- .debug.env$data.nodes[.debug.env$data.nodes$label == node, "value"]

        # Type
        val.type <- fromJSON(.debug.env$data.nodes[.debug.env$data.nodes$label == node, "valType"])
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

    line.df <- t(as.data.frame(line.df))
    colnames(line.df) <- c("var", "val", "type")
    rownames(line.df) <- c(1:length(nodes))
    return(line.df)
  }
}

.process.node <- function(node) {
  # Extract data entity from procedure activity via procedure-to-data edges
  entity <- .debug.env$proc.data.edges[.debug.env$proc.data.edges$activity == node, "entity"]

  val <- var <- type <- NULL
  if (length(entity) == 0) {
    val <- var <- type <- NA # give some info (code? --> new column?)
  } else {
    # Var
    var <- .debug.env$data.nodes[.debug.env$data.nodes$label == entity, "name"]

    # Val
    val <- .debug.env$data.nodes[.debug.env$data.nodes$label == entity, "value"]

    # Type
    val.type <- fromJSON(.debug.env$data.nodes[.debug.env$data.nodes$label == entity, "valType"])
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
  script <- .debug.env$proc.nodes[.debug.env$proc.nodes$label == node, "scriptNum"]

  line.row <- c(var, val, type, script)
  .debug.env$line.df <- rbind(.debug.env$line.df, line.row, stringsAsFactors = FALSE) ## cbind or rbind?
}
