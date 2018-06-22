# These environment will be used throughout the package as a way to communicate between functions
.debug.env <- new.env(parent = emptyenv())
.debug.env$has.graph = FALSE

# One of these two follwing function must be run before anything else

#'Initialization functions
#'
#'Intialize the package by running RDataTracker on a script
#'and/or parsing the resulting provenance into a useable format
#'Debug.init or debug.prov must be run before anything else
#'
#'@param input.data A path to an R script
#'@return There is no return value
#'@name debug.init
#'@export
#'@examples
#'\dontrun{
#'debug.init("test.R")
#'}
debug.init <- function(input.data) {

  # Extract what the file type is to make sure that it is an R file
  file.parts <- strsplit(input.data, "\\.")
  file.ext <- file.parts[[1]][[length(file.parts[[1]])]]

  if (file.ext == "R" || file.ext == "Rmd") {
    try.result = tryCatch({
      ddg.run(input.data)
    }, error = function(error_condition) {
      cat("This script had an error, to learn more run: \ndebug.error.trace() \n")
    }, finally={
      cat("RDataTracker is finished running \n")
    })
  } else {
    warning("Please enter a valid R script")
  }
  debug.prov(ddg.json(), is.file = F)

}

#'@rdname debug.init
#'@export
#'@param input.prov A prov.json compliant file from the system or a string from memory
#'@param is.file Logical stating whether or not input.prov needs to be read in from the system or not
#'@examples
#'\dontrun{
#'debug.prov("prov.json")
#'}
debug.prov <- function(input.prov, is.file = T) {
  prov.parse(input.prov, isFile = is.file)
  create.graph()
  .debug.env$has.graph = TRUE
}
