library(RDataTracker)
library(provParseR)
library(provGraphR)

debug.env <- new.env(parent = emptyenv())
debug.env$hasGraph = FALSE

debug.init <- function(input.data) {
  file.parts <- strsplit(input.data, "\\.")
  file.ext <- file.parts[[1]][[length(file.parts[[1]])]]

  if (file.ext == "R" || file.ext == "Rmd") {
    # OPENISSUE currently not supported due to RDataTracker JSON limitations
    #ddg.run(input.data)
    stop("passing of scripts is currently not supported due to RDataTracker JSON limitations")
  } else {
    warning("Please enter a valid script or prov.json name")
  }
  debug.prov(ddg.json(), isFile = F)
}

debug.prov <- function(input.prov, isFile = T) {
  prov.parse(input.prov, isFile = isFile)
  create.graph()
  debug.env$hasGraph = TRUE
}
