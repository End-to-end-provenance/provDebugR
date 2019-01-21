# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2018.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

#' Interactive time traveling script browser
#'
#' This function calls up an interactive debugging interface
#' similar to the browser() in core R. It has the functionality
#' to "step line-by-line" through a script that has finished 
#' execution.
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' debug.browser()
#' }
debug.browser <- function() {
  if(!.debug.env$has.graph) {
    stop("debug.init must be run first")
  }
  
  # The procedure nodes provide lines of code
  # var.env allows the user to get their variables back from their script
  proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
  var.env <- new.env(parent = emptyenv())
  var.env$call.stack <- list()
  
  # This is used when stepping through scripts to print the name
  # of the script the execution has jumped to
  scripts <- provParseR::get.scripts(.debug.env$prov)
  
  #The script name isn't an operation so will be removed
  # later on, but is needed to print to the user
  #script.name <- proc.nodes[1,]$name
  script.name <- scripts$script[1]
  
  # A table needs to be created to inform the debugger when it is possible for a 
  # user to be able to step into a sourced script
  # Finish nodes provide the necesssary information for when it is
  # possible to step-in and where to step back out to 
  finish.nodes <- proc.nodes[proc.nodes$type == "Finish", ]
  # The indexes are where in proc.nodes the debugger needs to look around 
  f.node.indexes <- as.integer(rownames(finish.nodes))
  step.in <- data.frame()
  
  # Each instance of finish can identify what script the step-in will
  # occur from, where it goes, and which line it's possible to step on
  for(f.index in f.node.indexes) {
    if(!f.index + 1 > nrow(proc.nodes)){
      curr.script <- proc.nodes[f.index + 1, ]$scriptNum
      line.number <- proc.nodes[f.index + 1, ]$startLine
      next.script <- proc.nodes[f.index - 1, ]$scriptNum

      step.in <- rbind(step.in, c(curr.script, line.number, next.script))
    }
  }
  if(length(step.in) != 0){
    names(step.in) <- c("cur.script", "line.number", "next.script")
  } 
    
  # The main script is script 1, and flow of control starts there
  current.script = 1
  
  proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
  proc.nodes <- proc.nodes[proc.nodes$scriptNum == current.script, ]
  
  # All the possible lines so the debugger can move through
  # debug.from.line without throwing any warnings
  pos.lines <- stats::na.omit(proc.nodes$startLine)
  var.env$lineIndex <- 1
  
  # Instructions for how to use the debugger
  cat("Debugger initialized, type \"help\" for more information or Q to quit\n")
  cat(paste(script.name), "\n", sep="")
  
  #Each line will print the code for the line
  cat(paste(pos.lines[var.env$lineIndex],
            ": ",
            proc.nodes[!is.na(proc.nodes$startLine) & proc.nodes$startLine == pos.lines[var.env$lineIndex], ]$name,
            "\n",
            sep=""))

  # Loads variables that may have been present before the script started into 
  # the recontructed environment
  pre.data.nodes <- debug.from.line(pos.lines[1], state = T)[[1]]
  pre.data.nodes <- pre.data.nodes[is.na(pre.data.nodes$script), ]
  if(nrow(pre.data.nodes) > 0) {
    .load.variables(pre.data.nodes, var.env)
  } else {
    var.env$vars <- NA
  }
  
  
  # This loop is the "interactive console" of the program
  # it will repeatedly prompt for an input until the user quits
  # It operates similarly to the R browser() function
  while(TRUE) {
    input <- strsplit(readline(prompt = "Debug> "), "\\s+")[[1]]

    # If they enter a Q the loop breaks
    if (input[1] == "Q") { 
      print("Quitting")
      break
    # lists variables present in "execution"
    # THey can enter one as an input and it will print it's value
    } else if(input[1] == "ls") { 
      print(sort (var.env$vars))
      
    # advances a line, or if a number is specified, advances
    # by the number of lines specified
    } else if (input[1] == "n")  {
      num.lines <- 
          if (length(input) == 1) 1
          else as.integer(input[2])
      new.info <- .moveForward(num.lines, var.env, current.script, pos.lines, proc.nodes, script.name, scripts)
      current.script <- new.info$new.script
      proc.nodes <- new.info$new.nodes
      pos.lines <- new.info$new.lines
      
    # moves "execution" to the end of the script
    } else if (input[1] == "c") { 
      # Continue until the "end" of execution
      var.env$lineIndex <- length(pos.lines)
      
      .change.line(var.env, pos.lines, proc.nodes,  current.script)
      
    # Moves back by the number of line specified by the user
    # If no lines are specified, moves back one line
    } else if (input[1] == "b") { 
      num.lines <- 
          if (length(input) == 1) 1
          else as.integer(input[2])
      new.info <- .moveBackward(num.lines, var.env, current.script, pos.lines, proc.nodes, script.name, scripts)
      current.script <- new.info$new.script
      proc.nodes <- new.info$new.nodes
      pos.lines <- new.info$new.lines
      
    # This function will print the line that the "execution" is 
    # currently on or if they specify a number will jump to that line
    } else if (input[1] == "s") {
      new.info <- .stepIn(var.env, current.script, pos.lines, proc.nodes, step.in, scripts)
      current.script <- new.info$new.script
      proc.nodes <- new.info$new.nodes
      pos.lines <- new.info$new.lines
      
    } else if (input[1] == "l") {
      # Clear out the command, if a number is left then 
      # modify behavior to use the number
      new.in <- gsub("l", "", input)
      if(grepl("[[:digit:]]", new.in)) {
        var.env$lineIndex <- findInterval(as.integer(new.in), pos.lines)
        .change.line(var.env, pos.lines, proc.nodes, current.script)
      } else {
        #Each line will print the code for the line
        cat(paste(pos.lines[var.env$lineIndex],
                  ": ",
                  proc.nodes[!is.na (proc.nodes$startLine) & proc.nodes$startLine == pos.lines[var.env$lineIndex], ]$name,
                  "\n",
                  sep=""))
      }
    # moves the reconstructed execution environment over to the global environment
    } else if (input[1] == "mv") { 
      #transfer environment
      if(!is.na(var.env$vars[1])){
        lapply(var.env$vars, function(var){
          # n = 3 sends to environment that called debug.browser
          assign(var, get(var, envir = var.env), envir = .GlobalEnv)
        })
      } else {
        cat("Environment empty, nothing to move\n")
      }
    # print information on how to use the debugger
    } else if (input[1] == "help") { 
      cat(paste("This is a time-traveling debugger for R \n", 
                "n - Move forward one line\n",
                "n* - Move forward * number of times (where * is an integer) \n",
                "b - Move backward one line\n",
                "b* - Move backward * number of times (where * is an integer)\n",
                "s - step into a source() call \n",
                "c - moves to end of \'execution\'\n",
                "ls - prints name of variables at the current point of \'execution\'\n",
                "l - print the current line\n",
                "l* - Move to line * (where * is an integer)\n",
                "mv - moves the current debugging environment to the Global Environment\n",
                "help - brings up this dialog \n",
                "Q - quits the debugger\n"
                ))
    # if they supplied a variable in script, print value  
    } else if(input[1] == "p" && input[2] %in% var.env$vars){ 
      value <- get(input[2], envir = var.env)
      if (is.character (value)) {
        # writeLines converts \n to a newline; print does not
        # but writeLines only works for strings
        writeLines(value)
      }
      else {
        print (value)
      }
    } 
    
    # Ignore a line containing only whitespace
    else if (gsub ("[[:space:]]", "", input) != ""){ 
      print ("Unable to evaluate expressions within the debugger")
      # pass their code to interpreter   
      # This is not a good idea.  It will use the final values of the variables, not
      # the values that the variables have at this point in stepping with the debugger.
#      tryCatch({
#        print (paste ("Evaluating ", input))
#        testthat::capture_output(ret.val <- eval(parse(text = input), envir = parent.frame(3)))
#        print(ret.val)
#      }, error = function(error.message) {
#        cat(paste("Error: \n", error.message, "\n", sep = ""))
#      })
      
    }
  }
}

#' This function uses debug.from.line to reconstruct the execution environment 
#' at a specific line of the script. It stores variable names and values 
#' in the var.env
#'
#' @name change.line
#'
#' @param var.env the environment that stores the variables and current line
#' of "execution"
#' @param pos.lines A vector of line numbers corresponding to lines of the script that had code
#' @param proc.nodes The procedure nodes, used for extracting the code on a line
#' @param current.script in the even of sourced scripts this will change to reflect the current
#' @importFrom utils read.csv
#' @return nothing
#' @noRd
#'
.change.line <- function(var.env, pos.lines, proc.nodes, current.script) {
  
  # if they choose to go past the amount of lines for execution
  # set it to the end and print they've reached the end
  if(var.env$lineIndex > length(pos.lines)) {
    print("End of Script")
    var.env$lineIndex <- length(pos.lines) + 1
  # If they've gone before executions starts, print they have reached the beginning 
  } else if (var.env$lineIndex < 1) {
    print("Start of Script")
    
    var.env$lineIndex <- 0
  } else {
    # Print the line number they are on as well as 
    # the line of code 
    cat(paste(pos.lines[var.env$lineIndex],
              ": ",
              proc.nodes[!is.na(proc.nodes$startLine) & proc.nodes$startLine == pos.lines[var.env$lineIndex], ]$name,
              "\n",
              sep=""))
  }
  # The environment will likely have variables from past line of execution 
  # clear it but keep the index of lines
  .clear.environment(var.env)
  
  # The first line won't have any values 
  # unless some were present in environment beforehand
  if((var.env$lineIndex == 1 | var.env$lineIndex == 0) & current.script == 1){
    # debug from line will return variables that were present pre-execution 
    # when state is true, but their script nubmer is NA, that's how it is 
    # possible to tell pre-execution variables
    pre.data.nodes <- debug.from.line(pos.lines[1], state = T)[[1]]
    pre.data.nodes <- pre.data.nodes[is.na(pre.data.nodes$script), ]
    if (nrow (pre.data.nodes) > 0) {
      .load.variables(pre.data.nodes, var.env)
    }
  } else {
    # This seems redundant, but if index is 1 but we're NOT in the main script
    # (as checked above) then this MUST be a source()ed script. Therefor we have
    # to check the call.stack for the previous line, as line.index -1 will produce an error
    if (var.env$lineIndex == 1) {
      # Since this is a source script, if the very first call in the main script was the call
      # to source(), then the environment will be NA except for previously initialized vars
      # Check up the "call.stack" to see if any of the lines are not zero, because if one is
      # not zero then other code was run that can be sent to debug.from.line. Otherwise,
      # having line.df be a matrix will prevent the var.env vars being set a few lines down
      line.df <- matrix()
      for(call.back in var.env$call.stack){
        
        if(length(call.back$prev.line) != 0) {
          # If there is a previous line to grab variables from, do it 
          line.df <- debug.from.line(call.back$prev.line,
                                     state = T, 
                                     script.num = call.back$script)[[1]]
          break
        }
      }
    } else {
      line.df <- debug.from.line(pos.lines[var.env$lineIndex - 1], state = T, script.num = current.script)[[1]]
    }
    
    .load.variables(line.df, var.env)
  }
}


#' Clears an environment but keeps the line index
#' as that is needed even when all variables are gone
#'
#' @name clear.environment
#'
#' @param var.env The environment to be cleared
#' 
#' @return nothing
#'
#' @noRd

.clear.environment <- function(var.env) {
  temp.index <- var.env$lineIndex
  temp.stack <- var.env$call.stack
  rm(list=ls(var.env), envir = var.env)
  var.env$lineIndex <- temp.index
  var.env$call.stack <- temp.stack
  var.env$vars <- NA
}

.load.variables <- function(line.df, var.env) {
  #store the name of all variables for printing out at a user's input
  var.env$vars <- line.df$`var/code`
    
  load.env <- new.env()
  # Assign each variable and its value to the created environment
  apply(line.df, 1, load.variable, var.env, load.env)
  rm(list=ls(load.env), envir = load.env)
}

load.variable <- function(row, var.env, load.env){
  if (is.na(row["val"][[1]])){
    assign(row["var/code"][[1]], NA , envir = var.env)
    return()
  }

  # Check if the value is a snapshot, regex here checks for starting with
  # data and is a file with an extension
  if(grepl("^data", row["val"][[1]]) & grepl("^.*\\.[^\\]+$", row["val"][[1]])){

    if(is.na(.debug.env$prov.folder)) {
      assign(row["var/code"][[1]], "UNKNOWN PROVENANCE FOLDER" , envir = var.env)
      return ()
    } 
    
    # If the provenance folder was found the snapshots can be grabbed
    # from the file system
    
    # The file ext indicates what type of data will be stored and how to 
    # read it back in for the user, the file name is also used to complete
    # the path to the final file
    file.name <- row["val"][[1]]
    file.parts <- strsplit(file.name, "\\.")
    file.ext <- file.parts[[1]][[length(file.parts[[1]])]]
    file.name <- sub (paste0(".", file.ext, "$"), "", file.name)
    file.ext <- tolower(file.ext)
    
    # A text file means that the data has been stored as an RObject
    # this can be loaded back in simply using load()
    if(file.ext == "txt") {
      .load.RObject (file.name, row, var.env, load.env)
    } 
    
    # csv could be matrix, array, or data_frame
    else if (file.ext == "csv") { 
      .load.csv (file.name, row, var.env)
    } 
    
    # R function
    else if (file.ext == "r") { 
      .load.r (file.name, row, var.env, load.env)
    } 
    
    # No identifiable file extension
    else {
      .load.RObject (file.name, row, var.env, load.env)
    }

    #If the data is not a snapshot it can be loaded directly into the variable env
  } else {
    
    # ... indicates a partial value; don't try to coerce to the actual type
    if (endsWith (row["val"][[1]], "...")) {
      assign(row["var/code"][[1]], row["val"][[1]], envir = var.env)
    }
    
    # vectors and lists might have the entire value, so coerce
    else if (row[["container"]] %in% c("vector", "list")) {
      if (row["dim"][[1]] == 0) {
        # An empty vector or list is stored as a string, like "integer(0)" so don't
        # try to coerce to an integer
        type <- jsonlite::fromJSON(row["type"])$type
        coerced.values <- 
          if (row[["container"]] == "vector") vector (mode=type)
          else list()
      }
      else {
        values <- 
            if (row["dim"][[1]] > 1) strsplit (trimws (row["val"][[1]]), " +")[[1]]
            else row["val"][[1]]
        type <- jsonlite::fromJSON(row["type"])$type
        coerced.values <-
            # Remove starting and ending \" for strings
          if (type == "character") sub ("\\\"$", "", sub ("^\\\"", "", values))
          else if (type == "NA") values
          else methods::as(values, type)

        if (length(coerced.values) == 1 && is.na (coerced.values)) {
          coerced.values <- values
        }
      }
      
      if (row[["container"]] == "vector") {
        assign(row["var/code"][[1]], as.vector (coerced.values), envir = var.env)
      }
      else {
        assign(row["var/code"][[1]], coerced.values, envir = var.env)
      }
    }

    else {
      # data_frame, matrix, array, factor, environment, function, language
      # Dont' try to coerce to the right type.  The string is almost certainly
      # incomplete.
      assign(row["var/code"][[1]], row["val"][[1]], envir = var.env)
    }
  }
}

.load.RObject <- function (file.name, row, var.env, load.env) {
  full.path <- paste(.debug.env$prov.folder,
      "/", file.name,
      ".RObject", sep = "")
  # Don't try and read in a file that could possibly not exist
  if(file.exists(full.path)){
    var.name <- load(full.path, envir = load.env)
    assign(row["var/code"][[1]], get(var.name, load.env), envir = var.env)
  } else {
    assign(row["var/code"][[1]], paste0 ("MISSING SNAPSHOT ", file.name, ".RObject"), envir = var.env)
  }
}

.load.csv <- function (file.name, row, var.env) {
  # a data frame can be read in using the read.csv function
  # which creates a data frame
  if(row[["container"]] == "data_frame"){
    
    full.path <- paste(.debug.env$prov.folder,
        "/", file.name,
        ".csv", sep = "")
    if(file.exists(full.path)){
      temp.var <- utils::read.csv(full.path, stringsAsFactors = F)
      
      assign(row["var/code"][[1]], temp.var, envir = var.env)
    } else {
      assign(row["var/code"][[1]], paste0 ("MISSING SNAPSHOT ", file.name, ".csv"), envir = var.env)
    }
    # A vector can be read in using read.csv but then needs the vectors extracted
  } else if (row[["container"]] == "vector" | row[["container"]] == "matrix") {
    
    full.path <- paste(.debug.env$prov.folder,
        "/", file.name,
        ".csv", sep = "")
    # Grab each column out of the data frame and then bind to create matrix
    # or just a single vector.
    if(file.exists(full.path)){
      temp.var <- utils::read.csv(full.path, stringsAsFactors = F)
      if(nrow(temp.var) == 0 ){
        if (row[["container"]] == "vector") {
          temp.var <- rep("", length.out = as.integer(row[["dim"]]))
        }
        
      } else {
        indexes <- 1:ncol(temp.var)
        temp.var <- cbind(sapply(indexes, function(index) {
                  temp.var[[index]]
                }))
        # a single vector is formatted differently than a single column of a matrix
        if(ncol(temp.var) == 1 & row[["container"]] == "vector" ){
          temp.var <- as.vector(temp.var)
        }
      }
      
      assign(row["var/code"][[1]], temp.var, envir = var.env)
      # No file found
    } else {
      assign(row["var/code"][[1]], paste0 ("MISSING SNAPSHOT ", file.name, ".csv"), envir = var.env)
    }
    # An array is very similar to a vector but can be coerced into an array post read
  } else if (row[["container"]] == "array") {
    if(file.exists(full.path)){
      temp.var <- read.csv(full.path, stringsAsFactors = F)
      indexes <- 1:ncol(temp.var)
      temp.var <- cbind(sapply(indexes, function(index) {
                temp.var[[index]]
              }))
      temp.var <- as.array(temp.var)
    } else {
      assign(row["var/code"][[1]], paste0 ("MISSING SNAPSHOT ", file.name, ".csv"), envir = var.env)
    } 
    #No identifiable container
  } else {
    assign(row["var/code"][[1]], paste ("UNKNOWN TYPE CONTAINER", row[["container"]]), envir = var.env)
  } 
}

.load.r <- function (file.name, row, var.env, load.env) {
  full.path <- paste(.debug.env$prov.folder,
      "/", file.name,
      ".Robject", sep = "")
  # Don't try and read in a file that could possibly not exist
  if(file.exists(full.path)){
    var.name <- load(full.path, envir = load.env)
    r.func <- get (var.name, load.env)
    assign(row["var/code"][[1]], r.func, envir = var.env)
  } else {
    assign(row["var/code"][[1]], paste0 ("MISSING SNAPSHOT ", file.name, ".RObject"), envir = var.env)
  }
}



.moveForward <- function(forw.by, var.env, current.script, pos.lines, proc.nodes, script.name, scripts) {
  print (paste ("Moving forward", forw.by, "lines"))
  # Clear out the command, if a number is left then 
  # modify behavior to use the number
#  new.in <- gsub("n", "", input)
#  if(grepl("[[:digit:]]", new.in)){
#    forw.by <- as.integer(new.in)
    # Find the index closest value to what was specified 
    # in lines and then change execution to go there 
    var.env$lineIndex <- findInterval(pos.lines[var.env$lineIndex] + forw.by, pos.lines)
    print(paste("var.env$lineIndex =", var.env$lineIndex))
#  } else {
#    var.env$lineIndex <- var.env$lineIndex + 1
#  }
  
  # In the event they are ending a source()ed script
  # then the exectuion needs to shift to a new set of proc.nodes
  if(var.env$lineIndex > length(pos.lines) & length(var.env$call.stack) != 0 ) {
    
    # Use the information from var.env's call stack to reset proc.nodes
    # to the previous script to right after where the source call was 
    current.script <- var.env$call.stack[[1]]$script
    proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
    proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
    proc.nodes <- proc.nodes[proc.nodes$scriptNum == current.script, ]
    
    # All the possible lines so the debugger can move through
    # debug.from.line without throwing any warnings
    pos.lines <- stats::na.omit(proc.nodes$startLine)
    
    # If NA the source call was the last line and can just set lineIndex 
    # to the length of lines +1 which will trigger the end of script message
    if(!is.na(var.env$call.stack[[1]]$next.line)){
      var.env$lineIndex <- findInterval(var.env$call.stack[[1]]$next.line, pos.lines)
    } else {
      var.env$lineIndex <- length(pos.lines) + 1
    }
    
    # the main script doesn't appear in the sourced scripts vector
    cat(paste(scripts[current.script]), "\n", sep="")
    
    # "pop" the stack now that execution has jumped back to the previous script
    var.env$call.stack <- var.env$call.stack[-1]
  }
  
  .change.line(var.env, pos.lines, proc.nodes,  current.script)
  
  return (list (new.script=current.script, new.nodes=proc.nodes, new.lines=pos.lines))
}

.moveBackward <- function(back.by, var.env, current.script, pos.lines, proc.nodes, script.name, scripts) {
  # Clear out the command, if a number is left then 
  # modify behavior to use the number
#  new.in <- gsub("b", "", input)
#  if(grepl("[[:digit:]]", new.in)) {
#    back.by <- as.integer(new.in)
    # Find the index closest value to what was specified 
    # in lines andthen change execution to go there 
    var.env$lineIndex <- findInterval(pos.lines[var.env$lineIndex] - back.by, pos.lines)
#  } else {
#    var.env$lineIndex <- var.env$lineIndex - 1
#  }
  
  # In the event they are ending a source()ed script
  # then the exectuion needs to shift to a new set of proc.nodes
  if(var.env$lineIndex < 1 & length(var.env$call.stack) != 0 ) {
    
    # Use the information from var.env's call stack to reset proc.nodes
    # to the previous script to right after where the source call was 
    current.script <- var.env$call.stack[[1]]$script
    proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
    proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
    proc.nodes <- proc.nodes[proc.nodes$scriptNum == current.script, ]
    
    # All the possible lines so the debugger can move through
    # debug.from.line without throwing any warnings
    pos.lines <- stats::na.omit(proc.nodes$startLine)
    
    # If 0 the source call was the first line and can just set lineIndex 
    # to 1, if not NA than the (index value for it) + 1 is the source call
    if(length(var.env$call.stack[[1]]$prev.line) != 0){
      var.env$lineIndex <- findInterval(var.env$call.stack[[1]]$prev.line, pos.lines)
      var.env$lineIndex <- var.env$lineIndex + 1
    } else {
      var.env$lineIndex <- 1
    }
    
    cat(paste(scripts[current.script]), "\n", sep="")
    
    # "pop" the stack now that execution has jumped back to the previous script
    var.env$call.stack <- var.env$call.stack[-1]
  }
  
  .change.line(var.env, pos.lines, proc.nodes, current.script)
  return (list (new.script=current.script, new.nodes=proc.nodes, new.lines=pos.lines))
}

.stepIn <- function(var.env, current.script, pos.lines, proc.nodes, step.in, scripts) {
  if(length(step.in) != 0) {
    
    # Grab the possible step-in places for the current script only
    script.steps <- step.in[step.in$cur.script == current.script, ]
    
    # Only if the current line the execution is at can be stepped into
    # should we attempt to. Otherwise just go to next line.
    # Scripts are 'stepped into" by simply swapping the procedure nodes out with 
    # the nodes from another script
    if(pos.lines[var.env$lineIndex] %in% script.steps$line.number){
      
      # Grab the exact script it is possible to step into and corresponding information
      step.info <- script.steps[script.steps$line.number == pos.lines[var.env$lineIndex], ]
      
      # When stepping back out, the debugger needs to know where to go - next.line
      # When grabbing environment for the first line on the new script, sending it 
      # straight to debug.from.line will return the execution AFTER that line has 
      # been executed. We want before, and the line executed before that is the line
      # before the source script - prev.line
      # When stepping back out, need to know which script to step to - script
      call.back <- stats::setNames(list(step.info$cur.script,
                                        pos.lines[var.env$lineIndex + 1],
                                        pos.lines[var.env$lineIndex - 1] ),
                                   c("script", "next.line", "prev.line"))
      
      # call.stack is a list used here as a stack object to allow nesting source() calls
      # When one script is stepped out of, that call.back object is removed from the list
      var.env$call.stack <- append(var.env$call.stack, list(call.back), after = 0)
      
      # Switch execution to the new script, done by re-subsetting proc.nodes
      current.script <- step.info$next.script
      
      proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
      proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
      proc.nodes <- proc.nodes[proc.nodes$scriptNum == current.script, ]
      
      # All the possible lines so the debugger can move through
      # debug.from.line without throwing any warnings
      pos.lines <- stats::na.omit(proc.nodes$startLine)
      
      # print to the screen so the user know which script they're in 
      cat(paste(scripts[current.script]), "\n", sep="")
      
      # All scripts start at the first line of code, which the index will point to
      var.env$lineIndex <- 1
      
      .change.line(var.env, pos.lines, proc.nodes, current.script)
    } else {
      # if they try and step into a script with no source call move forward one line
      var.env$lineIndex <- var.env$lineIndex + 1
      
      .change.line(var.env, pos.lines, proc.nodes, current.script)
    }
  } else {
    var.env$lineIndex <- var.env$lineIndex + 1
    
    .change.line(var.env, pos.lines, proc.nodes,  current.script)
  }
  
  return (list (new.script=current.script, new.nodes=proc.nodes, new.lines=pos.lines))
}
