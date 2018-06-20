debug.from.line <- function(..., state = F) {
  # Collect the arguments passed to the function
  args <- list(...)

  # This function is useless unless the adj.graph exists
  if(!debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  # Check if line number is valid entry
  pos.line <- proc.nodes[, "startLine"]
  pos.line <- pos.line[!is.na(pos.line)]
  #pos.line <- as.list(unique(pos.line)) # do I want a list?

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
    ret.val <- lapply(args, .grab.line, state)
    names(ret.val) <- args
    return(ret.val)
  }
}


.grab.line <- function(lineNumber, state) {

  data.nodes <- get.data.nodes()
  proc.nodes <- get.proc.nodes()

  if (!state) {
    # doesn't account for multiple vars on one line
    ## multiple references

    # Nodes (possible to have more than one)
    nodes <- proc.nodes[proc.nodes$startLine == lineNumber, "label"]
    nodes <- nodes[!is.na(nodes)]

    # Create row for each variable on the line, then rbind into a data frame
    line.df <- NULL
    line.df <- lapply(nodes, function(node) {

      proc.data.edges <- get.proc.data()

      entity <- proc.data.edges[proc.data.edges$activity == node, "entity"]

      val <- var <- type <- NULL
      if (length(entity) == 0) {
        val <- var <- type <- NA #give some info (code? --> new column?)
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
          type <- paste("Data Frame:", val.type$dimension[1], "x", val.type$dimension[2])
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
    return(NULL)
  }
}
