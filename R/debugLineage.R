#'Lineage of variables
#'
#'This function will return either the lines that led up to a variable's creation
#'or the other variables that the chosen variable was used to create
#'
#'@param ... Variables to find the lineage for. Can be single elements or vectors/lists.
#'@param forward Determines whether to look for lineage forward in the script or backward
#'@return Returns one of two things. If no parameters were passed to the function
#'then a vector of possible variables will be returned. If variables were passed to the
#'function then a list of data frames is returned. Each data frame corresponds to one
#'of the variables. The names of the list will correspond to the variable passed
#'@export
#'@examples
#'\dontrun{
#'debug.init("example.R")
#'debug.lineage("x")
#'l <- c("x", "y", "foo", "bar")
#'debug.lineage(l)
#'debug.lineage(l, "z")
#'}
debug.lineage <- function(..., forward = F) {

  args <- .flatten.args(...)

  # Collect possible results the user could ask for
  pos.vars <- get.data.nodes()
  pos.vars <- pos.vars[pos.vars$type == "Data" | pos.vars$type == "Snapshot" |  pos.vars$name == "error.msg", ]
  pos.vars <- as.list(unique(pos.vars$name))

  # Make sure all the results passed by the user are valid
  # this produces a list of logicals, where TRUES
  #correspond to valid inputs
  pos.args <- lapply(args, function(arg){
    if(arg %in% pos.vars) {
      return(TRUE)
    } else {
      cat(paste(arg, "is not a possible result\n"))
      return(FALSE)
    }
  })

  # Any non-valid inputs will be removed as the list is subset
  # by logicals, TRUE corresponding to valid inputs
  args <- args[unlist(pos.args)]

  # If they did not provide any results themselve, list them out for them
  if(length(args) == 0) {
    cat("Options:\n")
    print(unlist(pos.vars))
  } else {
    ret.val <- lapply(args, .grab.lineage, forward = forward)
    names(ret.val) <- args
    return(ret.val)
  }
}


#' This helper function is used to find the labels of each
#' result the user passed. The labels are then processed in
#' another helper function which returns a data frame.
#' @name grab.lineage
#' @param result A character that corresponds to a variable name
#' @param forward A logical determining whether or not to search forward
#'
#' @return A data frame that contains the lineage of a variable.
#' Each row is a line from the script with corresponding metadata,
#' script #, line #, etc.
.grab.lineage <- function(result, forward) {
  # The data nodes have all the information on the variables
  data.nodes <- get.data.nodes()
  proc.nodes <- get.proc.nodes()

  # Get all the nodes from the requested variable, but since
  # it's going either forward or backward grab the end
  node.label <- NULL
  if(!forward) {
    node.label <- utils::tail(n=1,data.nodes[data.nodes$name == result, ])$label
  } else {
    node.label <- utils::head(n=1,data.nodes[data.nodes$name == result, ])$label
  }

  # The assignment statement is inclusive when going backward, but not
  # forward. Therefore, it should be grabbed from the lineage separately
  if(forward) {
    # This code finds assignemnt statement grabbing first procedure node
    # The first procedure node with the variable in it, is where it is first assigned
    proc.data.edges <- get.proc.data()
    edges <- proc.data.edges[proc.data.edges$entity == node.label, ]
    assign.state <- NA
    if(nrow(edges) > 0){
      assign.state <- edges$activity[[1]]
    }
  }

  # Use the label helper function to produce a data frame of lineage to return
  .process.label(node.label, proc.nodes, forward, assign.state = assign.state)
}

#' This function uses the spine of connected nodes to return the data frame
#' This function is also used by debug.warning.trace()
#'
#' @param label A character corresponding to a node name in the prov
#' @param proc.nodes Data frame with the prov porcedure nodes
#' @param forward A logical determining whether or not to search forward
#' @param assign.state A possibly NULL character used if lineage is going forward.
#' It has the label for the assignment statement for the chosen variable.
#'
#' @return A data frame that contains the lineage of a variable.
#' Each row is a line from the script with corresponding metadata,
#' script #, line #, etc.
#'
#' @name process.label
.process.label <- function(label, proc.nodes, forward, assign.state = NA) {
  # Grab the nodes that have connections to the chosen node from the adj graph
  spine <- get.spine(label, forward)

  # If moving forward the procedure node that contains the assignment
  # statement should be placed at the front of the spine to keep the order accurate
  if(forward && !is.na(assign.state)) {
    spine <- append(spine, assign.state, 0)
  }

  # Pull the lines from the proc nodes that are referenced
  # in the spine, each is stored as a row
  lines <- lapply(spine[grep("p[[:digit:]]", spine)], function(proc.node) {
    list(proc.nodes[proc.nodes$label == proc.node, ]$scriptNum,
         proc.nodes[proc.nodes$label == proc.node, ]$startLine,
         proc.nodes[proc.nodes$label == proc.node, ]$name)
  })

  # Since each line is stored as a row and the wanted result is a
  # data frame, the columns need to be extracted so they can
  # be covnverted to a data frame. Find how many columns there
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

  # The colnames are automatically assigned and
  # are not descriptive values, replace them with
  # descriptive values
  colnames(df) <- c("script", "line", "code")

  # Order the "lines" column to ascending
  # This ensures the results always follow
  # the flow of control
  df <- df[with(df, order(line)), ]
  rownames(df) <- 1:nrow(df)
  return(df)
}
