#'Type changes
#'
#'This function show how a variable's type has changed throughout
#'execution of a script
#'
#'@param ... Variables to find the type for. Can be single elements or vectors/lists.
#'@param just.logical Determines whether or not to display only if the variable has changed
#'or what the actual values were throughout execution.
#'@return Returns one of two things. If no parameters were passed to the function
#'then a vector of possible variables will be returned. If variables were passed to the
#'function then a list of data frames is returned. Each data frame corresponds to one
#'of the variables. The names of the list will correspond to the variable passed. 
#'@export
#'@examples
#'\dontrun{
#'debug.init("example.R")
#'debug.variable.type("x")
#'l <- c("x", "y", "foo", "bar")
#'debug.variable.type(l)
#'debug.variable.type(l, "z", just.logical = T)
#'}
debug.variable.type <- function(..., just.logical = F) {

  args <- .flatten.args(...)

  # Collect possible results the user could ask for
  # Remove any rows that aren't variables
  pos.vars <- provParseR::get.data.nodes(.debug.env$prov)
  pos.vars <- pos.vars[pos.vars$type == "Data" | pos.vars$type == "Snapshot", ]
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

  # If they did not provide any results themselves, list them out for them
  if(length(args) == 0) {
    cat("Options:\n")
    return(unlist(pos.vars))
  } else {
    ret.val <- lapply(args, .grab.instances)

    # If the user wants to know only if the variables have changed replace the
    # data frame with a list of logicals corresponding to if there was a change
    # or not based off the data returned from before
    if(just.logical) {
      ret.val <- lapply(ret.val, function(df) {
        return.value <- FALSE
        # If there is more than a single unique value in the column
        # that means there were at least two types
        if(length(unique(df$type)) > 1) {
          return.value <- TRUE
        }
        return(return.value)
      })
    }

    names(ret.val) <- args
    return(ret.val)
  }
}

#' This helper function takes a variable name and finds each
#' instance of it in a script and what type it was. Optionally
#' the user can request a list that states whether or not the
#' variable changed type.
#'
#' @param result A character that corresponds to a variable
#'
#' @return A data frame that has each assignment statmenent of result as a row
#'
#' @name grab.instances
#' @noRd
.grab.instances <- function(result) {
  # The data nodes have all the information on the variables
  data.nodes <- provParseR::get.data.nodes(.debug.env$prov)
  proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
  proc.data <- provParseR::get.proc.data(.debug.env$prov)

  # These data node are the rows that math with the provided result
  res.dat.nodes <- data.nodes[data.nodes$name == result, ]

  # These procedure labels are those that have connections with the chosen result nodes
  res.proc.labels <- proc.data[proc.data$entity %in% res.dat.nodes$id, ]$activity

  # These procedure nodes are the ones that have connections with the chosen result nodes
  res.proc.rows <- proc.nodes[proc.nodes$id %in% res.proc.labels, ]

  # Now that the data and procedure nodes have been narrowed down to only the
  # requested information, the information that will be provided to the user
  # is grabbed
  script <- res.proc.rows$scriptNum
  line <- res.proc.rows$startLine
  scope <- res.dat.nodes$scope
  container <- NULL
  dim <- NULL
  type <- NULL

  # valType is a string that contains the container, dim, and type info
  # This string is stored (almost always) as a JSON object that needs 
  # to be parsed before the information can be grabbed
  for (val in res.dat.nodes$valType) {
    # The instances where it's not a JSON string is as follows,
    # object, environment, language, and function
    # For now make the type equal to what is in valType and NA the rest
    if(val == "\"object\""){
      container <- append(container, NA)
      dim <- append(dim, NA)
      type <- append(type, "object")
    } else if (val == "\"environment\"") {
      container <- append(container, NA)
      dim <- append(dim, NA)
      type <- append(type, "environment")
    } else if (val == "\"language\"") {
      container <- append(container, NA)
      dim <- append(dim, NA)
      type <- append(type, "language")
    } else if (val == "\"function\"") {
      container <- append(container, NA)
      dim <- append(dim, NA)
      type <- append(type, "function")
    } else {
      parsed.val <- jsonlite::fromJSON(val)
      container <- append(container, parsed.val$container)
      
      # Dimensions and type can be vectors when there are multiple 
      # dimensions to a variable. Type will have each columns type.
      dim <- append(dim, paste(parsed.val$dimension, collapse = ","))
      type <- append(type, paste(parsed.val$type, collapse = ","))
    }
  }

  # Combine all the data and return to the user
  df <- as.data.frame(cbind(script, line, scope, container, dim, type), stringsAsFactors = F)
}
