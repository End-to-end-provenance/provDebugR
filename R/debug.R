# These environment will be used throughout the package as a way to communicate between functions
.debug.env <- new.env(parent = emptyenv())
.debug.env$has.graph = FALSE

#'Initialization functions
#'
#'Intialize the package by running RDataTracker on a script
#'and/or parsing provenance into a useable format.
#'Debug.init must be run before anything else
#'
#'@param input.data A path to an R script, a prov.json file,
#'provenance from memory, or nothing.
#'@param dir A path to where to save the prov folder
#'@return nothing
#'@import RDataTracker
#'@import provParseR
#'@import provGraphR
#'@import igraph
#'@export
#'@examples
#'\dontrun{
#'debug.init() # if there RDataTracker has already run and
#'there is provenance in memory
#'debug.init("test.R")
#'debug.init("prov.json")
#'debug.init(ddg.json()) # ddg.json is an RDataTracker function
#'}
debug.init <- function(input.data = NA, dir = NULL) {
  # If the warn option is not set to 1 the warnings in a user's script
  # will not appear until after the script it is
  # AND another command is run in the console
  def.warn <- options()$warn
  if(!def.warn > 1){
    options(warn = 1)
  }
  
  # Extract what the file type is to make sure that it is an R file
  # Also grab the name of the file (minus extension) for locating ddg folder
  if(!is.na(input.data)){
    file <- gsub("^.*[/\\]", "", input.data)
    file.parts <- strsplit(file, "\\.")
    file.ext <- tolower(file.parts[[1]][[length(file.parts[[1]])]])
    file.name <- file.parts[[1]][1]
    file.path <- gsub("([^/]+$)", "", input.data)
  }
  
  ddg.folder <- paste(file.path, file.name, "_ddg", sep ="")
  
  if(!dir.exists(ddg.folder)) {
    ddg.folder <- NA
    .debug.env$ddg.folder <- NA
  } else {
    .debug.env$ddg.folder <- ddg.folder
  }
  
  # Run the script and if it error'd let the user know
  # and let them know how to find lineage of the error
  if (is.na(input.data)) {
    tryCatch({
      .debug.prov(ddg.json(), is.file = F)
    }, warning = function(warning.message) {
      cat("\nNo provenance in memory\n")
    }, error = function(error.message) {
      cat("\nNo provenance in memory\n")
    })
  } else if (file.ext == "r" || file.ext == "rmd") {
    try.result = tryCatch({
      ddg.run(input.data, ddgdir = dir)
    }, error = function(error_condition) {
      cat(paste("\nThis script had an error:\n",
                error_condition,
                "\nTo learn more run: \ndebug.error.trace() \n\n"))
    }, finally={
      cat("RDataTracker is finished running \n")
    })
    .debug.prov(ddg.json(), is.file = F)
  } else if (file.ext == "json") {
    .debug.prov(input.data)
  } else {
    .debug.prov(input.data, is.file = F)
  }

  # Set the warning options back to whatever the user origianlly had
  options(warn = def.warn)
}

#'Debug.init Helper
#'@name debug.prov
#'@param input.prov A prov.json compliant file from the system or a string from memory
#'@param is.file Logical stating whether or not input.prov needs to be read in from the system or not
.debug.prov <- function(input.prov, is.file = T) {
  prov.parse(input.prov, isFile = is.file)
  create.graph()
  .debug.env$has.graph = TRUE
}

#' This helper function is used in almost all functions of the interface
#' to make sure only a list of un-nested elements
#'
#' @param ... A list (possibly of lists) that the user input as arguments
#' to one of the functions
#'
#' @return A list of unnested elements
#'
#' @name flatten.args
.flatten.args <- function(...) {
  # This function is useless unless the adj.graph exists
  if(!.debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  args <- unlist(list(...))
}
