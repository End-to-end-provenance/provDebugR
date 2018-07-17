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
  
  
  current.script = 0
  
  #The script name isn't an opertion so will be removed
  # on the next line, but is needed to print to the user
  script.name <- proc.nodes[1,]$name
  
  proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
  proc.nodes <- proc.nodes[proc.nodes$scriptNum == current.script, ]
  
  # All the possible lines so the debugger can move through
  # debug.from.line without throwing any warnings
  lines <- stats::na.omit(proc.nodes$startLine)
  var.env$lineIndex <- 1
  
  
  # Instructions for how to use the debugger
  cat("Debugger initialized, type \"help\" for more information or Q to quit\n")
  cat(paste(script.name), "\n", sep="")

  #Each line will print the code for the line
  cat(paste(lines[var.env$lineIndex],
            ": ",
            proc.nodes[proc.nodes$startLine == lines[var.env$lineIndex], ]$name,
            "\n",
            sep=""))
  
  # This is used to determine which variables to print to the screen
  # if their name is input instead of being passed to the interpreter
  var.env$vars <- NA
  
  # This loop is the "interactive console" of the program
  # it will repeatedly prompt for an input until the user quits
  # It operates similarly to the R browser() function
  while(TRUE) {
    input <- readline(prompt = "Debug> ")
    
    # If they enter a Q the loop breaks
    if (input == "Q") { 
      print("Quitting")
      break
    # lists variables present in "execution"
    # THey can enter one as an input and it will print it's value
    } else if(input == "ls") { 
      print(var.env$vars)
      
    # advances a line, or if a number is specified, advances
    # by the number of lines specified
    } else if (input == "n" | grepl("^n[[:digit:]]", input))  { 
      # Clear out the command, if a number is left then 
      # modify behavior to use the number
      new.in <- gsub("n", "", input)
      if(grepl("[[:digit:]]", new.in)){
        forw.by <- as.integer(new.in)
        # Find the index closest value to what was specified 
        # in lines andthen change execution to go there 
        var.env$lineIndex <- findInterval(lines[var.env$lineIndex] + forw.by, lines)
      } else {
        var.env$lineIndex <- var.env$lineIndex + 1
      }
      
      .change.line(var.env, lines, proc.nodes)
    
    # moves "execution" to the end of the script
    } else if (input == "c") { 
      # Continue until the "end" of execution
      var.env$lineIndex <- length(lines)
      
      .change.line(var.env, lines, proc.nodes)
      
    # Moves back by the number of line specified by the user
    # If no lines are specified, moves back one line
    } else if (input == "b" | grepl("^b[[:digit:]]", input)) { 
      # Clear out the command, if a number is left then 
      # modify behavior to use the number
      new.in <- gsub("b", "", input)
      if(grepl("[[:digit:]]", new.in)) {
        back.by <- as.integer(new.in)
        # Find the index closest value to what was specified 
        # in lines andthen change execution to go there 
        var.env$lineIndex <- findInterval(lines[var.env$lineIndex] - back.by, lines)
      } else {
        var.env$lineIndex <- var.env$lineIndex - 1
      }
     
      .change.line(var.env, lines, proc.nodes)
    # This function will print the line that the "execution" is 
    # currently on or if they specify a number will jump to that line
    } else if (input == "l" | grepl("^l[[:digit:]]", input)) {
      # Clear out the command, if a number is left then 
      # modify behavior to use the number
      new.in <- gsub("l", "", input)
      if(grepl("[[:digit:]]", new.in)) {
        var.env$lineIndex <- findInterval(as.integer(new.in), lines)
        .change.line(var.env, lines, proc.nodes)
      } else {
        #Each line will print the code for the line
        cat(paste(lines[var.env$lineIndex],
                  ": ",
                  proc.nodes[proc.nodes$startLine == lines[var.env$lineIndex], ]$name,
                  "\n",
                  sep=""))
      }
    # moves the simulated execution environment over to the global environment
    } else if (input == "mv") { 
      #transfer environment
      if(!is.na(var.env$vars[1])){
        lapply(var.env$vars, function(var){
          # n = 3 sends to environment that called debug.browser
          assign(var, get(var, envir = var.env), envir = parent.frame(n = 3))
        })
      } else {
        cat("Environment empty, nothing to move\n")
      }
    # print information on how to use the debugger
    } else if (input == "help") { 
      cat(paste("This is a post-mortem debugger for R \n", 
                "n - Move forward one line\n",
                "n* - Move forward * number of times (where * is an integer) ",
                "b - Move backward one line\n",
                "b* - Move backward * number of times (where * is an integer)",
                "c - moves to end of \'execution\'\n",
                "ls - prints name of variables at the current point of \'execution\'\n",
                "l - print the current line",
                "l* - Move to line * (where * is an integer)",
                "mv - moves the current debugging environment to the Global Environment\n",
                "help - brings up this dialog \n",
                "Q - quits the debugger\n"
                ))
    # if they supplied a variable in script, print value  
    } else if(input %in% var.env$vars){ 
      print(get(input, envir = var.env))
    # pass their code to interpreter   
    } else { 
      tryCatch({
        testthat::capture_output(ret.val <- eval(parse(text = input), envir = parent.frame(3)))
        print(ret.val)
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
#' @importFrom utils read.csv
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
    # Print the line number they are on as well as 
    # the line of code 
    cat(paste(lines[var.env$lineIndex],
              ": ",
              proc.nodes[proc.nodes$startLine == lines[var.env$lineIndex], ]$name,
              "\n",
              sep=""))
  }
  # The environment will likely have variables from past line of execution 
  # clear it but keep the index of lines
  .clear.environment(var.env)
  # if they are past the first line, set the environment to be 
  # where execution was at before their current line was executed
  if(var.env$lineIndex > 1) {
    line.df <- debug.from.line(lines[var.env$lineIndex - 1], state = T)[[1]]
    
    #store the name of all variables for printing out at a user's input
    var.env$vars <- line.df$`var/code`
    
    load.env <- new.env()
    # Assign each variable and it's value to the created environment
    apply(line.df, 1, function(row){
      if(!is.na(row["val"][[1]])){
        # Check if the value is a snapshot, regex here checks for starting with
        # data and is a file with an extension
        if(grepl("^data", row["val"][[1]]) & grepl("^.*\\.[^\\]+$", row["val"][[1]])){
          # If the provenance folder was found the snapshots can be grabbed
          # from the file system
          if(!is.na(.debug.env$ddg.folder)) {
            # The file ext indicates what type of data will be stored and how to 
            # read it back in for the user, the file name is also used to complete
            # the path to the final file
            file.parts <- strsplit(row["val"][[1]], "\\.")
            file.ext <- tolower(file.parts[[1]][[length(file.parts[[1]])]])
            file.name <- file.parts[[1]][1]
            
            # A text file means that the data has been stored as an RObject
            # this can be loaded back in simply using load()
            if(file.ext == "txt") {
              full.path <- paste(.debug.env$ddg.folder,
                                 "/", file.name,
                                 ".RObject", sep = "")
              # Don't try and read in a file that could possibly not exist
              if(file.exists(full.path)){
                var.name <- load(full.path, envir = load.env)
                assign(row["var/code"][[1]], get(var.name, load.env), envir = var.env)
              } else {
                assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = var.env)
              }
            # csv could be matrix, array, or data_frame
            } else if (file.ext == "csv") { 
              
              # a data frame can be read in using the read.csv function
              # which creates a data frame
              if(row[["container"]] == "data_frame"){
          
                full.path <- paste(.debug.env$ddg.folder,
                                   "/", file.name,
                                   ".csv", sep = "")
                if(file.exists(full.path)){
                  temp.var <- utils::read.csv(full.path, stringsAsFactors = F)
                  
                  assign(row["var/code"][[1]], temp.var, envir = var.env)
                } else {
                  assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = var.env)
                }
              # A vector can be read in using read.csv but then needs the vectors extracted
              } else if (row[["container"]] == "vector" | row[["container"]] == "matrix") {
              
                full.path <- paste(.debug.env$ddg.folder,
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
                    # } else if (row[["container"]] == "matrix") {
                    #   temp.var <- matrix(data ="", nrow = , ncol = 5)
                    # }
                    
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
                  assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = var.env)
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
                  assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = var.env)
                } 
                #No identifiable container
              } else {
                assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT", envir = var.env)
              } 
            # No identifiable file extension
            } else {
              assign(row["var/code"][[1]], "INCOMPLETE SNAPSHOT" , envir = var.env)
            }
          # If the ddg.folder was not located
          } else {
            assign(row["var/code"][[1]], "SNAPSHOT/MISSING PROVENANCE" , envir = var.env)
          }
        #If the data is not a snapshot it can be loaded directly into the variable env
        } else {
          type <- jsonlite::fromJSON(row["type"])$type
          assign(row["var/code"][[1]], methods::as(row["val"][[1]], type), envir = var.env)
        }
      }
    })
    rm(list=ls(load.env), envir = load.env)
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
