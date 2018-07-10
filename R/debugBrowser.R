#' Interactive post-mortem script browser
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
  proc.nodes <- get.proc.nodes()
  var.env <- new.env(parent = emptyenv())
  
  #The script name isn't an opertion so will be removed
  # on the next line, but is needed to print to the user
  script.name <- proc.nodes[1,]$name
  proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
  
  # All the possible lines so the debugger can move through
  # debug.from.line without throwing any warnings
  lines <- proc.nodes$startLine
  var.env$lineIndex <- 1
  currentScript = 0
  
  # Instructions for how to use the debugger
  cat("Debugger initialized, type \"help\" for more information or Q to quit\n")
  cat(paste(script.name), "\n", sep="")

  # Each line will print the code for the line 
  cat(paste(lines[var.env$lineIndex], ": ", proc.nodes[lines[var.env$lineIndex], ]$name, sep=""))
  # This is used to determine which variables to print to the screen
  # if their name is input instead of being passed to the interpreter
  var.env$vars <- NA
  
  # This loop is the "interactive console" of the program
  # it will repeatedly prompt for an input until the user quits
  # It operates similarly to the R browser() function
  while(TRUE) {
    input <- readline(prompt = "Debug> ")
    
    if (input == "Q") { # Quits the program
      print("Quitting")
      break
    } else if(input == "ls") { # lists variables present in "execution"
      print(var.env$vars)
    } else if (input == "n")  { # advances a line
      var.env$lineIndex <- var.env$lineIndex + 1
      
      .change.line(var.env, lines, proc.nodes)
    } else if (input == "c") { # moves to the end of the script
      # Continue until the "end" of execution
      var.env$lineIndex <- length(lines)
      
      .change.line(var.env, lines, proc.nodes)
    } else if (input == "b") { # moves back a line
      var.env$lineIndex <- var.env$lineIndex - 1
      
      .change.line(var.env, lines, proc.nodes)
    } else if (input == "mv") { # moves environment over to the global environment
      #transfer environment
      if(!is.na(var.env$vars[1])){
        lapply(var.env$vars, function(var){
          #assign(var, get(var, envir = var.env), envir = .GlobalEnv)
        })
      } else {
        cat("Environment empty, nothing to move\n")
      }
      
    } else if (input == "help") { # print information 
      cat(paste("This is a post-mortem debugger for R \n", 
                "n - Move forward one line\n",
                "b - Move Backward one line\n",
                "c - moves to end of \'execution\'\n",
                "ls - prints name of variables at the current point of \'execution\'\n",
                "help - brings up this dialouge\n",
                "Q - quits the debugger\n"
                ))
    } else if(input %in% var.env$vars){ # if they supplied a variable in script, print value
      print(get(input, envir = var.env))
    } else { # pass their code to interpreter 
      tryCatch({
        source(exprs = parse(text = input))
      }, error = function(error.message) {
        cat(paste("Error: \n", error.message, "\n", sep = ""))
      })
      
    }
  }
}

#' This function uses debug.from.line to simulate the execution environment 
#' at a specific line of the script. It stores variable names and values 
#' in the var.env
#'
#' @name change.line
#'
#' @param var.env the environment that stores the variables and current line
#' of "execution"
#' @param lines A vector of line numbers corresponding to lines of the script that had code
#' @param proc.nodes The procedure nodes, used for extracting the code on a line
#' @return nothing
#'
.change.line <- function(var.env, lines, proc.nodes) {
  # if they choose to go past the amount of lines for execution
  # set it to the end and print they've reached the end
  if(var.env$lineIndex > length(lines)) {
    print("End of Script")
    var.env$lineIndex <- length(lines) + 1
  # If they've gone before executions starts, print they have reached the beginning 
  } else if (var.env$lineIndex < 1) {
    print("Start of Script")
    var.env$lineIndex <- 0
  } else {
    # The environment will likely have variables from past line of execution 
    # clear it but keep the index of lines
    .clear.environment(var.env)

    # Print the line number they are on as well as 
    # the line of code 
    cat(paste(lines[var.env$lineIndex],
              ": ",
              proc.nodes[var.env$lineIndex, ]$name,
              "\n",
              sep=""))
    
    # if they are past the first line, set the environment to be 
    # where execution was at before their current line was executed
    if(var.env$lineIndex > 1) {
      line.df <- debug.from.line(lines[var.env$lineIndex - 1], state = T)[[1]]
      #store the name of all variables for printing out at a user's input
      var.env$vars <- line.df$`var/code`
      # Assign each variable and it's value to the created environment
      apply(line.df, 1, function(row){
        if(!is.na(row["val"][[1]])){
          if(grepl("^data", row["val"][[1]]) & grepl("^.*\\.[^\\]+$", row["val"][[1]])){
            assign(row["var/code"][[1]], "SNAPSHOT" , envir = var.env)
          } else {
            assign(row["var/code"][[1]], methods::as(row["val"][[1]],row["type"][[1]]) , envir = var.env)
          }
        }
      })
    }
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
.clear.environment <- function(var.env) {
  temp.index <- var.env$lineIndex
  rm(list=ls(var.env), envir = var.env)
  var.env$lineIndex <- temp.index
  var.env$vars <- NA
}
