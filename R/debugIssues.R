# This function is a simple wrapper around the data.lineage function to show
# how an error was produced

#'Warnings and errors
#'
#'This function will trace the lineage of an error message, returning the lines
#'that led up to a script stopping
#'
#'@name debug.error.trace
#'@param stack.overflow Determines whether or not the
#'error message should be searched for automatically on stack overflow
#'@return Debug.error: Returns a data frame with all the lines that led to an error's creation
#'@export
#'@examples
#'\dontrun{
#'debug.init("example.R")
#'debug.error.trace()
#'debug.error.trace(stack.overflow = T) 
#'}
debug.error.trace <- function(stack.overflow = F) {
  # This function is useless unless the adj.graph exists
  if(!.debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  # Since the user only wants to know about an error msg
  # and there can only be one we can grab the message to
  # print to the user and if it doesn't exist then we can
  # return gracefully after telling them
  data.nodes <- provParseR::get.data.nodes(.debug.env$prov)
  message <- data.nodes[data.nodes$name == "error.msg", ]$value

  if(length(message) > 0){
    cat(paste("Your Error: ", message, "\n", sep = ""))

    if(stack.overflow) {

      # The error message should be stripped of information that is
      # unique to this script and be more generalized
      error.message <- .process.error(message)

      # Query the Stack Exchange API for similar results
      result <- .debug.search(error.message)

      # Grab the titles and links to the questions
      pos.urls <- head(result$items)[, c("title", "link")]

      # This serves as a "menu" of sorts since it will print the row number
      # of each title
      print(pos.urls[, "title"])

      # They can either choose none or an index that will be matched to a row
      cat("\nChoose a numeric value that matches your error the best or q to quit: \n")
      chosen.result <- readline()

      if(!chosen.result == "q") {

        chosen.result <- as.integer(chosen.result)

        # The input needs to be an integer so it can be used to
        # index into the rows of the data frame
        if(is.na(chosen.result)){
          stop("Invalid Input")
        } else if (chosen.result > 6 || chosen.result < 1) {
          stop ('Choose an option between 1 - 6')
        }

        # Open up the requested link in the default web browser
        browseURL(pos.urls[chosen.result ,]$link)
        cat("\nCode that led to error message:\n")
      }
    }

    return(debug.lineage("error.msg")$error.msg)
  } else {
    cat("There were no errors in this script!")
  }
}

# This function operates similarily to debug.lineage; however,
# it uses just the warning messages. The whole debug.lineage function
# cannot be reused here as it relies on variables having unique
# names. warnings do not have unique names and there can be multiple
# warnings unlike error messages.

#' This function shows the lineage of the warning messages in a script
#'
#'@rdname debug.error.trace
#'@param ... A number representing the error message the function should return.
#'To see the possibilities call the function with no argument passed
#'@return Debug.warning: Returns one of two things. If no parameters were passed to the function
#'then a list of possible warnings will be returned. The number next to the warning
#'is the number that should be passed to grab that lineage. If variables were passed to the
#'function then a list of data frames is returned. Each data frame corresponds to one
#'of the warnings.
#'@export
#'@examples
#'\dontrun{
#'debug.init("example.R")
#'debug.warning.trace() # returns a list of possible warnings
#'debug.warning.trace(1, 4) # returns warnings 1 and 4
#'debug.warning.trace(1:4, 7) # returns warnings 1 through 4 and 7
#'}
debug.warning.trace <- function(...) {

  args <- .flatten.args(...)

  # Grab all the warning rows from the provenance
  pos.vars <- provParseR::get.data.nodes(.debug.env$prov)
  pos.vars <- pos.vars[pos.vars$name == "warning.msg", ]
  
  if(nrow(pos.vars) == 0){
    cat("There were no warnings in this script!")
  } else {
    row.names(pos.vars) <- 1:nrow(pos.vars)

    node.labels <- as.list(pos.vars$id)

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
        cat(paste(arg, "is not a possible result\n"))
        return(FALSE)
      }
    })

    # Any non-valid inputs will be removed as the list is subset
    # by logicals, TRUE corresponding to valid inputs
    args <- args[unlist(pos.args)]
    
    # If they did not pass any arguments to the function
    # then print the possible arguments they can input
    if (length(args) == 0) {
      cat("Possible results: \n")
      results.df <- as.data.frame(pos.vars$value)
      colnames(results.df) <- NULL
      print(results.df)
      cat("\nPass the corresponding numeric value to the function for info on that warning\n")
    } else {
      # The procedure nodes are used in the .proccess.label fucntion
      # to find script and line numbers and code
      proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)

      # Each of the chosen warning message needs to be processed,
      dfs <- lapply(args, function(arg){
        .process.label(pos.vars[arg, ]$id, proc.nodes, forward = F)
      })
  
      return(dfs)
    }
  }
}

#' Process error message strings
#'
#' This function removes *most* local information about a
#' script by removing all characters between quotes (single or
#' double, inclusive)
#'
#' @param error.message a character vector to be cleaned
#' @name process.error
#' @return character
#' @noRd
.process.error <- function(error.message) {

  split <- strsplit(error.message, ":")[[1]]

  # Error messages from the prov.json will
  #typically have an uneeded prefix followed
  # by a colon ":"
  if(length(split) > 1) {
    error.message <- split[-1]
  }

  # This complicated mess of regex i=actually checks for 4 things (all inclusive):
  # Matches to characters surronded by quotes "dog"
  # Matches to characters surronded by escaped quotes \"dog\"
  # Matches to characters surronded by single quotes 'dog'
  # Matches to characters surronded by escaped quotes \'dog\'
  exp <- "\\\"[^\"\r]*\\\"|\"[^\"\r]*\"|\'[^\"\r]*\'|\\\'[^\"\r]*\\\'"

  gsub(exp, "", error.message, perl = T)
}


