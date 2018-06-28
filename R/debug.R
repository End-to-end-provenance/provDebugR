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
debug.init <- function(input.data = NA) {
  # If the warn option is not set to 1 the warnings in a user's script
  # will not appear until after the script it is
  # AND another command is run in the console
  def.warn <- options()$warn
  options(warn = 1)

  # Extract what the file type is to make sure that it is an R file
  file.parts <- strsplit(input.data, "\\.")
  file.ext <- tolower(file.parts[[1]][[length(file.parts[[1]])]])


  # Run the script and if it error'd let the user know
  # and let them know how to find lineage of the error
  if (file.ext == "r" || file.ext == "rmd") {
    try.result = tryCatch({
      ddg.run(input.data)
    }, error = function(error_condition) {
      cat(paste("\nThis script had an error:\n", error_condition, "\nTo learn more run: \ndebug.error.trace() \n\n"))
    }, finally={
      cat("RDataTracker is finished running \n")
    })
    .debug.prov(ddg.json(), is.file = F)
  } else if (is.na(input.data)) {
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

.flatten.args <- function(...) {
  # This function is useless unless the adj.graph exists
  if(!.debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  # Collect the arguments passed to the function
  args <- list(...)

  # In case they also entered a list as an argument
  # the list should be extracted so that we're left with
  # only single elements
  flat.args <- list()

  # Extract everything and append it to the temp list
  # appending will be able to unnest any passed lists
  lapply(args, function(arg){
    flat.args <<- append(flat.args, arg)
  })

  args <- flat.args
}
