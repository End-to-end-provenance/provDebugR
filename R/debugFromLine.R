#' State of variables given lines
#'
#' This function will either return all references to variables on
#' a given line, or the state of all variables up to that point in
#' execution.
#'
#' @param ... Line(s) to examine. Can be single lines or vectors/lines.
#' @param state If FALSE, returns the refereneces to variables on
#' inputed line(s). If TRUE, returns the state of all variables up to
#' that point in execution.
#' @param script.num If 0, only examine variables from the main script.
#' If 1 or higher, examine variables from further nested source scripts.
#' @return A list of one data frame per line, containing information about
#' the variables on that line. If no parameters were passed, returns a
#' data frame containing the state of all variables at the end of execution.
#' @export
#' @examples
#' \dontrun{
#' debug.from.line(4, state = F)
#' debug.from.line(4, 5:8, 10, state = T)
#' debug.from.line()
#' }

debug.from.line <- function(..., state = F, script.num = 0) {

  # Collect the arguments passed to the function
  args <- .flatten.args(...)
  
  # Remove 0s (for debugGadget())
  args <- args[!args %in% 0]

  # Get procedure nodes (and thus startLine and scriptNum) from parser
  # Subset by inputted script number
  # Subset operation-type nodes to get rid of NA values
  # (Gets rid of Start and Finish type)
  proc.nodes <- get.proc.nodes()
  proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
  proc.delete <- proc.nodes[proc.nodes$scriptNum != script.num, "label"]
  .debug.env$proc.nodes <- proc.nodes[proc.nodes$scriptNum == script.num, ]
  
  # Get proc-data edges
  # Subset out edges that correspond to proc.delete
  proc.data.edges <- get.proc.data()
  .debug.env$proc.data.edges <- proc.data.edges[!(proc.data.edges$activity %in% proc.delete), ]
  
  # Get data nodes (and thus var, val, and type) from parser
  # Subset file-type nodes to get rid of those with no corresponding procedure
  # Capture file-type nodes in delete.these for later subsetting
  data.nodes <- get.data.nodes()
  file.delete <- data.nodes[data.nodes$type == "File", "label"]
  .debug.env$data.nodes <- data.nodes[data.nodes$type != "File", ]

  # Get data-proc edges
  # Subset file-type nodes with file.delete
  data.proc.edges <- get.data.proc()
  data.proc.edges <- data.proc.edges[!(data.proc.edges$entity %in% file.delete), ]
  .debug.env$data.proc.edges <- data.proc.edges[!(data.proc.edges$activity %in% proc.delete), ]

  # Check if line number is valid entry
  # True if input is a possible line number
  pos.line <- .debug.env$proc.nodes[, "startLine"]
  pos.args <- lapply(args, function(arg, pos.line) {
    if (arg %in% pos.line) {
      return(TRUE)
    } else {
      cat(arg, "is not a possible line\n", sep = " ")
      return(FALSE)
    }
  }, pos.line = pos.line)

  args <- args[unlist(pos.args)]

  # If parameter is blank, show state of all variables at end of execution
  # Otherwise, call helper function .grab.line over each line input
  if (length(args) == 0) {
    print("State of all variables at end of execution:")
    ret.val <- .grab.line(max(pos.line), state = T, script.num = 0)
    return(ret.val)
  } else {
    ret.val <- lapply(args, .grab.line, state, script.num)
    names(ret.val) <- args
    return(ret.val)
  }
}

#' This helper function is used to find all procedure or data nodes
#' on each line that the user has passed. The nodes are then processed
#' in another helper function which modifies the data frame line.df
#'
#'
#' @name grab.line
#' @param lineNumber A numeric corresponding to the line that the
#' user wants to examine
#' @param state Determines if the variable references on that line
#' will be examined or the state of all variables up to that line's
#' execution
#' @param script.num If 0 examine only variables from the main script.
#' If 1, examine variables from first nested source script.
#'
#' @return A data frame in the debug environemnt, which contains the
#' columns var/code, val, type, and script. Each row is a variable.
.grab.line <- function(lineNumber, state, script.num) {

  # Clear line.df for subsequent function calls
  .debug.env$line.df <- data.frame()

  if (!state) { # REFERENCE - all variables referenced on the line

    # Find procedure nodes with the inputted line number
    nodes <- .debug.env$proc.nodes[.debug.env$proc.nodes$startLine == lineNumber, "label"]

    # Add to list of nodes those that are referenced on the line
    # by finding their corresponding data node (via data-to-proc edges)
    # and seeing if there's another proc node attached (via proc-to-data edges)
    ref.nodes <- unlist(lapply(nodes, function(node) {
      ref.entity <- .debug.env$data.proc.edges[.debug.env$data.proc.edges$activity == node, "entity"]
      ref.node <- NA
      if (length(ref.entity) != 0) {
        ref.node <- .debug.env$proc.data.edges[.debug.env$proc.data.edges$entity == ref.entity, "activity"]
        if(length(ref.node) == 0){
          ref.node <- ref.entity
        }
      }
      return(ref.node)
    }))
    nodes <- c(nodes, ref.nodes)
    nodes <- nodes[!is.na(nodes)]

    # Create row for each variable on the line, then rbind into a data frame
    lapply(nodes, .process.node, script.num)

    # Name columns and rows
    colnames(.debug.env$line.df) <- c("var/code", "val", "container", "dim", "type", "script")
    rownames(.debug.env$line.df) <- c(1:length(nodes))

    return(.debug.env$line.df)

  } else { # STATE - state of all variables up to that line in execution

    # Find procedure nodes with the inputted line number
    node <- .debug.env$proc.nodes[.debug.env$proc.nodes$startLine == lineNumber, "label"]

    # Extract data entity from procedure activity via procedure-to-data edges
    entity <- .debug.env$proc.data.edges[.debug.env$proc.data.edges$activity == node, "entity"]

    # If no corresponding entity (data node) exists, get next viable node
    # from the preceding line number. This accounts for source() calls to files.
    while (length(entity) == 0) {
      all.proc.nodes <- get.proc.nodes()[get.proc.nodes()$type == "Operation", ]
      all.proc.data <- get.proc.data()
      rownames(all.proc.nodes) <- 1:nrow(all.proc.nodes)
      new.node.index <- as.integer(rownames(all.proc.nodes[all.proc.nodes$label == node, ])) - 1
      if(length(new.node.index) == 0) {
        break
      }
      node <- all.proc.nodes[new.node.index, "label"]
      entity <- all.proc.data[all.proc.data$activity == node, "entity"]
      if(new.node.index == 1 & length(entity) == 0)
      {
        break
      }
    }

    # If entity is 0 that means there were no values
    # If it's not 0 create the data frame
    if(length(entity) > 0)
    {
      # Find number of preceding data nodes
      # Subset that out of data.nodes
      rownames(.debug.env$data.nodes) <- 1:nrow(.debug.env$data.nodes)
      rnum <- rownames(.debug.env$data.nodes[.debug.env$data.nodes$label == entity, ])
      nodes <- .debug.env$data.nodes["1":rnum[1], "label"]
      
      # Account for duplicates by removing all but the tail
      node.names <- .debug.env$data.nodes[.debug.env$data.nodes == nodes, "name"]
      temp.df <- cbind(as.data.frame(nodes, stringsAsFactors = FALSE), node.names, stringsAsFactors = FALSE)
      nodes <- temp.df[!duplicated(temp.df$node.names, fromLast = T), "nodes"]
      
      # Create row for each variable on the line, then rbind into a data frame
      lapply(nodes, .process.node, script.num)
      
      # Name columns and rows
      colnames(.debug.env$line.df) <- c("var/code", "val", "container", "dim", "type", "script")
      rownames(.debug.env$line.df) <- c(1:length(nodes))
    #If entity was 0, populate a data frame with NAs 
    } else {
      .debug.env$line.df <- rbind(rep(NA, 6))
      # Name columns and rows
      colnames(.debug.env$line.df) <- c("var/code", "val", "container", "dim", "type", "script")
      rownames(.debug.env$line.df) <- c(1:nrow(.debug.env$line.df))
    }
   

    return(.debug.env$line.df)
  }
}

#' This helper function is used to find information about the node
#' passed to it. A row with the information is created and appended
#' to a data frame in the debug environment.
#' @name process.node
#' @param node A character corresponding to a node name in the prov
#' @param script.num If 0, examine only variables from the main script.
#' If 1, examine variables from first nested source script.
#'
#' @return Nothing
.process.node <- function(node, script.num) {

  # Extract data entity from procedure activity via procedure-to-data edges
  # For reference, argument will be proc node
  # For state, argument will be data node
  if (grepl("p", node)) {
    # entity will be character(0) if there's no corresponding data node
    entity <- .debug.env$proc.data.edges[.debug.env$proc.data.edges$activity == node, "entity"]
    script <- script.num
  } else if (grepl("d", node)) {
    entity <- node
    entity.activity <- get.proc.data()[get.proc.data()$entity == entity, "activity"]
    
    # If there is no entity.activity then the variable came from envir before script ran
    # In this case there is no script #, populate with NA to indicate this
    if(length(entity.activity > 0)){
      script <- get.proc.nodes()[get.proc.nodes()$label == entity.activity, "scriptNum"]
    } else {
      script <- NA
    }
    
  }

  # Initialize variables to be returned
  val <- var <- type <- NULL
  if (length(entity) == 0) {

    # For state, val and type don't exist
    val <- container <- dim <- type <- NA

    # Set var to the code on the line (name in proc.nodes)
    var <- .debug.env$proc.nodes[.debug.env$proc.nodes$label == node, "name"]

  } else {

    # Var is entity name
    var <- .debug.env$data.nodes[.debug.env$data.nodes$label == entity, "name"]

    # Val is entity value
    val <- .debug.env$data.nodes[.debug.env$data.nodes$label == entity, "value"]

    # Type is string parsed from entity valType
    val.type <- jsonlite::fromJSON(.debug.env$data.nodes[.debug.env$data.nodes$label == entity, "valType"])
    
    container <- val.type$container
    
    # JSON formatted so that we can put a list in a single element of a data frame
    dim <- paste(val.type$dimension, collapse = ",")
    type <- paste("{ \"type\" : [",
                  paste("\"", paste(val.type$type, collapse= "\", \""), "\"", sep ="")
                  , "]}")
  }

  # Combine all info into a row
  # Append that row to the data frame in the environment
  line.row <- c(var, val, container, dim, type, script)
  .debug.env$line.df <- rbind(.debug.env$line.df, line.row, stringsAsFactors = FALSE)
}
