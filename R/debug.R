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

# These environment will be used throughout the package as a way to communicate between functions
.debug.env <- new.env(parent = emptyenv())
.debug.env$has.graph = FALSE
.debug.env$graph <- NULL
.debug.env$prov.folder <- NA

#'Initialization functions
#'
#'Intialize the package by running rdt or rdtLite on a script
#'and/or parsing provenance into a useable format.
#'Debug.init must be run before anything else
#'
#'@param input.data A path to an R script, a prov.json file,
#'provenance from memory, or nothing.
#'@param dir A path to where to save the prov folder
#'@return nothing
#'@import igraph
#'@export
#'@examples
#'\dontrun{
#'debug.init() # if rdt or rdtLite has already run and
#'there is provenance in memory
#'debug.init("test.R")
#'debug.init("prov.json")
#'debug.init(prov.json()) # prov.json is a function in rdt/rdtLite
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
  # Also grab the name of the file (minus extension) for locating prov folder
  if(!is.na(input.data)){
    file <- gsub("^.*[/\\]", "", input.data)
    file.parts <- strsplit(file, "\\.")
    file.ext <- tolower(file.parts[[1]][[length(file.parts[[1]])]])
    file.name <- file.parts[[1]][1]
    file.path <- gsub("([^/]+$)", "", input.data)
  }

  # Determine where to load prov.json and prov.run from
  loaded <- loadedNamespaces()
  if ("rdtLite" %in% loaded) {
    tool <- "rdtLite"
  }
  else if ("rdt" %in% loaded) {
    tool <- "rdt"
  }
  else {
    installed <- utils::installed.packages ()
    if ("rdtLite" %in% installed) {
      tool <- "rdtLite"
    }
    else if ("rdt" %in% installed) {
      tool <- "rdt"
    }
    else {
      stop ("One of rdtLite or rdt must be installed.")
    }
  }

  if (tool == "rdt") {
    prov.run <- rdt::prov.run
    prov.json <- rdt::prov.json
  }
  else {
    prov.run <- rdtLite::prov.run
    prov.json <- rdtLite::prov.json
  }
  
  
  # If no data is input, look for json in memory
  if (is.na(input.data)) {
    tryCatch({
      .debug.prov(prov.json(), is.file = F)
    }, warning = function(warning.message) {
      cat("\nNo provenance in memory\n")
    }, error = function(error.message) {
      cat("\nNo provenance in memory\n")
    })
  # If it's a script, run it, and if it errors let the user know
  # and let them know how to find lineage of the error
  } else if (file.ext == "r" || file.ext == "rmd") {
    try.result = tryCatch({
      prov.run(input.data, prov.dir = dir)
    }, error = function(error_condition) {
      cat(paste("\nThis script had an error:\n",
                error_condition,
                "\nTo learn more run: \ndebug.error.trace() \n\n"))
    }, finally={
      cat(paste (tool, "is finished running \n"))
    })
    
    .debug.prov(prov.json(), is.file = F)
    
  # If the file was a json file, it has provenance and 
  # can be passed right to debug.prov to be parsed
  } else if (file.ext == "json") {
    .debug.prov(input.data)
  } else {
    .debug.prov(input.data, is.file = F)
  }
  
  # Set the warning options back to whatever the user originally had
  options(warn = def.warn)
}

#'Debug.init Helper
#'@name debug.prov
#'@param input.prov A prov.json compliant file from the system or a string from memory
#'@param is.file Logical stating whether or not input.prov needs to be read in from the system or not
.debug.prov <- function(input.prov, is.file = T) {
  .debug.env$prov <- provParseR::prov.parse(input.prov, isFile = is.file)
  .debug.env$graph <- provGraphR::create.graph(input.prov, isFile = is.file)
  .debug.env$has.graph = TRUE
  
   # Check for the prov folder which will have information for scripts and 
   # snapshot data later on
   prov.env <- provParseR::get.environment(.debug.env$prov)
   prov.folder <- prov.env [prov.env$label == "provDirectory", ]$value
   
   # If it was found save it's location in the environment to be used later
   # Otherwise save an NA value to indicate it is missing to prevent
   # reading in from a file that does not exist
   if(dir.exists(prov.folder)) {
     .debug.env$prov.folder <- prov.folder
   }
  
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
#' @noRd
.flatten.args <- function(...) {
  # This function is useless unless the adj.graph exists
  if(!.debug.env$has.graph) {
    stop("debug.init must be run first")
  }

  args <- unlist(list(...))
}
