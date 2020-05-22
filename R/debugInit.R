# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2020.

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

###############################################################################

# The debug environment
.debug.env <- new.env(parent = emptyenv())

# parsed provenance and adjacency graph
.debug.env$prov <- NULL
.debug.env$graph <- NULL
.debug.env$has.graph <- FALSE

# procedure nodes, data nodes
.debug.env$proc.nodes <- NULL
.debug.env$data.nodes <- NULL

# data-to-procedure edges, procedure-to-data edges
.debug.env$data.proc <- NULL
.debug.env$proc.data <- NULL

# environment for loaded variables
.debug.env$var.env <- NULL

# path to provenance directory
.debug.env$prov.dir <- NULL

# === INIT =================================================================== #

#' A Time-Travelling Debugger for R - Debugger Initialization
#'
#' prov.debug uses the provenance from the last execution of prov.run to 
#' initialise the debugger.
#'
#' Provenance is a detailed record of the execution of a script which includes
#' information about the steps that were excecuted and the intermediate data values
#' that were used and/or created. After it is collected, it can be used in a
#' variety of ways to better understand the execution.
#'
#' This package, provDebugR, is one such application, using provenance post-execution
#' to help the user understand and debug their script by providing functions to
#' look at intermediate steps and data values, as well as their forwards or backwards 
#' lineage. These functions may be used only after provDebugR has been initialised using 
#' one of the initialisation functions above.
#'
#' The forwards lineage of a data object is the list of steps showing how the data object
#' was used. The backwards lineage of a data object is the list of steps showing how the
#' data object was produced.
#'
#' provDebugR uses provenance produced by rdtLite (a provenance collection package
#' available on CRAN), stored in PROV-JSON format.
#'
#' @seealso Other provDebugR Functions (non-initialisation):
#' @seealso \code{\link{debug.error}}: Returns the backwards lineage of the error, if any.
#'              The error may be queried on StackOverflow.
#' @seealso \code{\link{debug.line}}: Returns all immediate inputs and outputs
#'              for the line(s) queried.
#' @seealso \code{\link{debug.lineage}}: Returns the forwards or backwards lineage
#'              of the data object(s) queried. The forwards lineage shows how the
#'              data object was used, and the backwards lineage shows how it was produced. 
#' @seealso \code{\link{debug.state}}: Returns the state at the line(s) queried,
#'              after the line had been executed. The state is the list of all 
#'              variables and their values in the environment at the queried line.
#' @seealso \code{\link{debug.type.changes}}: Returns a data frame for each variable in
#'              the execution containing the instances where the data type changed.
#' @seealso \code{\link{debug.variable}}: Returns a data frame showing all instances
#'              of the variable(s) queried.
#' @seealso \code{\link{debug.view}}: Opens and displays the contents of each file or variable
#'              or variable queried.
#' @seealso \code{\link{debug.warning}}: Returns the backwards lineage of the queried
#'              warning(s), if any.
#'
#' @seealso Other tools that use provenance: 
#'          \url{https://github.com/End-to-end-provenance}
#'
#' @references rdtLite (Provenance Collection Tool): 
#'             \url{https://cran.r-project.org/web/packages/rdtLite/index.html}
#' @references PROV-JSON output produced by rdtLite: 
#'             \url{https://github.com/End-to-end-provenance/ExtendedProvJson/blob/master/JSON-format.md}
#' @references PROV-JSON standard: 
#'             \url{https://www.w3.org/Submission/2013/SUBM-prov-json-20130424/}
#'
#' @examples
#' \dontrun{
#' rdtLite::prov.run("test.R")
#' prov.debug()}
#'
#' @export
#' @rdname prov.debug
prov.debug <- function()
{
	# clear debug environment first!
	.clear()
	
	# determine which provenance collection tool to use
	tool <- .get.tool()
	
	if(tool == "rdtLite")
		prov.json <- rdtLite::prov.json
	else
		prov.json <- rdt::prov.json
	
	# pass json to helper function to initialise debugger
	.debug.init(prov.json(), is.file = FALSE)
}

#' A Time-Travelling Debugger for R - Debugger Initialization
#'
#' prov.debug.file reads a PROV-JSON file to initialise the debugger.
#'
#' @param prov.file Path to a PROV-JSON file.
#'
#' @examples
#' \dontrun{
#' prov.debug.file("prov_test/prov.json")}
#'
#' @export
#' @rdname prov.debug
prov.debug.file <- function(prov.file)
{
	# clear debug environment first!
	.clear()
	
	.debug.init(prov.file, is.file = TRUE)
}

#' A Time-Travelling Debugger for R - Debugger Initialization
#'
#' prov.debug.run executs a R or Rmd script, collects provenance, and
#' initialises the debugger using the collected provenance.
#'
#' @param script Path to an R script.
#' @param ... extra parameters are passed to the provenance collector.  See rdt's prov.run function
#'    or rdtLites's prov.run function for details.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R", snapshot.size = 100)}
#'
#' @export
#' @rdname prov.debug
prov.debug.run <- function(script, ...)
{
	# clear debug environment first!
	.clear()
	
	# determine which provenance collection tool to use
	tool <- .get.tool()
	
	if(tool == "rdtLite") {
		prov.run <- rdtLite::prov.run
		prov.json <- rdtLite::prov.json
	}
	else {
		prov.run <- rdt::prov.run
		prov.json <- rdt::prov.json
	}
	
	# collect provenance
	tryCatch({
		prov.run(script, ...)
	}, finally = {
		cat(paste (tool, "is finished running.\n"))
		
		# initialise debugger
		.debug.init(prov.json(), is.file = FALSE)
	})
}

# === HELPER FUNCTIONS ======================================================= #

#' Initialises the debugger.
#'
#' @param json Path to a PROV-JSON file, or a PROV-JSON string.
#' @param is.file If TRUE, this indicates that the value given in the parameter
#'                'json' is a file.
#'
#' @return N/A
#' @noRd
.debug.init <- function(json, is.file)
{
	# get parsed provenance, adjacency graph
	.debug.env$prov <- provParseR::prov.parse(json, isFile = is.file)
	.debug.env$graph <- provGraphR::create.graph(json, isFile = is.file)
	
	# procedure nodes, data nodes
	.debug.env$proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
	.debug.env$data.nodes <- provParseR::get.data.nodes(.debug.env$prov)
	
	# data-to-procedure edges, procedure-to-data edges
	.debug.env$data.proc <- provParseR::get.data.proc(.debug.env$prov)
	.debug.env$proc.data <- provParseR::get.proc.data(.debug.env$prov)
	
	# var.env (for loading variables for viewing)
	.debug.env$var.env <- new.env(parent = .debug.env)
	
	# path to provenance directory
	environment <- provParseR::get.environment(.debug.env$prov)
	.debug.env$prov.dir <- environment$value[environment$label == "provDirectory"]
	
	
	# empty case
	if(is.null(.debug.env$graph)) {
		.debug.env$has.graph <- FALSE
		stop("There is no provenance.")
	}
	
	.debug.env$has.graph <- TRUE
	
	# get full code for each procedure node
	.debug.env$proc.nodes$name <- .get.full.code()
}

#' .get.tool determines whether to use rdt or rdtLite to get the provenance.
#' 
#' If rdtLite is loaded, "rdtLite" is returned. If rdtLite is not loaded, but rdt
#' is, "rdt" is returned. If neither is loaded, it then checks to see if either
#' is installed, favoring "rdtLite" over "rdt".
#' 
#' Stops if neither rdt nor rdtLite is available.
#' 
#' @return "rdtLite" or "rdt"
#' @noRd 
.get.tool <- function()
{
	# choose provenance collector from loaded libraries
	libs <- loadedNamespaces()
	
	if ("rdtLite" %in% libs)
		return("rdtLite")
	if ("rdt" %in% libs)
		return("rdt")
	
	# neither rdtLite nor rdt are loaded,
	# choose provenance collector from installed libraries
	libs <- utils::installed.packages()
	
	if ("rdtLite" %in% libs)
		return("rdtLite")
	if ("rdt" %in% libs)
		return("rdt")
	
	# nether rdt nor rdt are available: stop
	stop("One of rdtLite or rdt must be installed.")
}

#' Returns the full code for each Operation procedure node. A vector of strings.
#' Uses a helper function for testing purposes.
#'
#' @return The full code for each Operation procedure node. A vector of strings.
#' @noRd
.get.full.code <- function()
{
	# get list of saved scripts
	scripts <- provParseR::get.saved.scripts(.debug.env$prov)$script
	
	# call helper function
	return(.get.full.code.helper(.debug.env$proc.nodes, scripts))
}

#' Helper function for .get.full.code
#' Given a table of procedure nodes and saved scripts, returns the full code 
#' for each Operation procedure node as a vector of strings.
#' Separated from .get.full.code for testing purposes.
#'
#' @param proc.nodes Table of procedure nodes.
#' @param scripts List of paths to saved scripts.
#'
#' @return The full code for each Operation procedure node. A vector of strings.
#' @noRd
.get.full.code.helper <- function(proc.nodes, scripts)
{	
	# vector to store script paths that could not be found
	inaccessible <- c()
	
	lines <- lapply(scripts, function(script) 
	{
		# check if file exists
		if(!file.exists(script)) {
			inaccessible <<- append(inaccessible, script)
			return(NA)
		}
		
		# read file
		file <- file(script, "r", encoding = getOption("encoding"))
		line.list <- readLines(file, warn = FALSE)
		close(file)
		
		return(line.list)
	})
	
	# case: throw warning if there are inaccessible files
	if(length(inaccessible) > 0) 
	{
		warn.msg <- paste(inaccessible, collapse="\n")
		warn.msg <- paste("Unable to access the following files:",
						  warn.msg, sep="\n")
		warning(warn.msg, call. = FALSE)
	
		# case: if all files are inaccessible
		# return the name column of proc nodes as is
		if(length(inaccessible) == length(scripts)) {
			return(proc.nodes$name)
		}
	}
	
	# get full code for each proc node
	codes <- sapply(1:nrow(proc.nodes), function(i)
	{
		node <- proc.nodes[i, ]
		script.num <- node$scriptNum
		
		# case: not an operation
		# case: script file was not found
		script.lines <- lines[[script.num]]
		
		if(length(script.lines) == 1 && is.na(script.lines)) {
			return(node$name)
		}
		
		if(node$type != "Operation") {
			return(node$name)
		}
		
		# get full code
		# if procedure has more than 1 line, 
		# collapse the lines into 1 before returning
		if(node$endLine - node$startLine == 0) {
			return(script.lines[node$startLine])
		}
		
		code <- script.lines[node$startLine:node$endLine]
		return(paste(code, sep="", collapse = "\n"))
	})
	
	return(unname(codes))
}

# === FOR TESTING ONLY ======================================================= #

# This function returns provDebugR's debug environment to its initial state. 
#
#' @noRd
.clear <- function()
{
	# parsed provenance and adjacency graph
	.debug.env$prov <- NULL
	.debug.env$graph <- NULL
	.debug.env$has.graph <- FALSE

	# procedure nodes, data nodes
	.debug.env$proc.nodes <- NULL
	.debug.env$data.nodes <- NULL

	# data-to-procedure edges, procedure-to-data edges
	.debug.env$data.proc <- NULL
	.debug.env$proc.data <- NULL
	
	# environment for loaded variables
	.debug.env$var.env <- NULL
	
	# path to provenance directory
	.debug.env$prov.dir <- NULL
}
