library(RDataTracker)
library(provParseR)
library(provGraphR)

debug.env <- new.env(parent = emptyenv())
debug.env$has.graph = FALSE

# One of these two function must be run before anything else

# This function intializes the package by running rdt on a script
# it then calls the prov function
debug.init <- function(input.data) {

  # Extract what the file type is to make sure that it is an R file
  file.parts <- strsplit(input.data, "\\.")
  file.ext <- file.parts[[1]][[length(file.parts[[1]])]]

  if (file.ext == "R" || file.ext == "Rmd") {
    # OPENISSUE currently not supported due to RDataTracker JSON limitations
    # ddg.run(input.data)
    stop("passing of scripts is currently not supported due to RDataTracker JSON limitations")
  } else {
    warning("Please enter a valid R script")
  }
  debug.prov(ddg.json(), is.file = F)
}

# This function parses the json passed by the user or debug.init
# It the produces the adj matrix that is used in tracking lineage
debug.prov <- function(input.prov, is.file = T) {
  prov.parse(input.prov, isFile = is.file)
  create.graph()
  debug.env$has.graph = TRUE
}
