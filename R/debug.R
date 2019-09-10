
# The debug environment
.debug.env <- new.env(parent = emptyenv())

.debug.env$has.graph <- FALSE
.debug.env$graph <- NULL
.debug.env$prov <- NULL
.debug.env$prov.folder <- NA

.debug.env$proc.nodes <- NULL
.debug.env$data.nodes <- NULL
.debug.env$proc.data <- NULL
#.debug.env$data.proc <- NULL


# === INIT =================================================================== #

# uses last json generated
prov.debug <- function()
{
	# determine which provenance collector to use
	tool <- .get.tool()
	
	if(tool == "rdtLite")
		prov.json <- rdtLite::prov.json
	else
		prov.json <- rdt::prov.json
	
	# pass json to helper
	# TODO error catching
	.debug.init(prov.json(), is.file = FALSE)
}

# uses json from file
prov.debug.file <- function(prov.file)
{
	# pass json to helper
	.debug.init(prov.file, is.file = TRUE)
}

# script is passed in: run prov collection first
prov.debug.run <- function(script)
{
	# TODO: ensure that it is an R file
	
	# determine which provenance collector to use
	tool <- .get.tool()
	
	if(tool == "rdtLite")
		prov.run <- rdtLite::prov.run
	else
		prov.run <- rdt::prov.run
	
	# run to generate json
	tryCatch(
		prov.run()
	)
}

.debug.init <- function(json, is.file)
{
	.debug.env$prov <- provParseR::prov.parse(json, isFile = is.file)
	.debug.env$graph <- provGraphR::create.graph(json, isFile = is.file)
	
	.debug.env$proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
	.debug.env$data.nodes <- provParseR::get.data.nodes(.debug.env$prov)
	
	.debug.env$proc.data <- provParseR::get.proc.data(.debug.env$prov)
	
	# empty case
	if(is.null(.debug.env$graph))
	{
		.debug.env$has.graph <- FALSE
		stop("The provenance is empty.")
	}
	
	.debug.env$has.graph <- TRUE
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


# === DEBUG FUNCTIONS ======================================================== #

# currently just enables query for one script at a time
#' @param ... Line(s) to examine. Can be single lines or vectors/lines.
debug.line <- function(..., script.num = 1)
{
	if(!.debug.env$has.graph)
		stop("No provenance is available.")
	
	# collect line numbers from arguments
	args <- .flatten.args(...)
	
	# proc nodes of interest: subset by type = Operation, scriptNum
	proc.nodes <- .debug.env$proc.nodes[.debug.env$proc.nodes$type == "Operation", ]
	proc.nodes <- proc.nodes[proc.nodes$scriptNum == script.num, ]
	
	# extract valid line numbers
}

debug.state <- function()
{
	
}

# TODO - start.line NA for now
debug.lineage <- function(..., start.line = NA, forward = FALSE)
{
	if(!.debug.env$has.graph)
		stop("No provenance is available.")
	
	args <- .flatten.args(...)
	
	# get the list of all possible variables the user could query
	pos.vars <- as.list(unique(.debug.env$data.nodes$name))
	
	# get valid arguments
	valid.args <- .get.valid.args(pos.vars, args)
	
	if(is.null(valid.args))
		return(invisible(NULL))
	
	# get lineage
	lineages <- lapply(valid.args, .get.lineage, forward = forward)
	names(lineages) <- valid.args
	
	# TODO - form into data frame
	return(lineages)
}

debug.type.changes <- function()
{
	
}

debug.variable <- function()
{
	
}

debug.error.trace <- function()
{
	# null case
	if(!.debug.env$has.graph)
		stop("No provenance is available.")
	
	# 
}

#' Process error message strings
#'
#' This function removes *most* local information about a
#' script by removing all characters between quotes (single or
#' double, inclusive)
#'
#' @param error.message a character vector to be cleaned
#' @name process.error
#' @return character
#' @noRd
.process.error <- function(error.message) {

  split <- strsplit(error.message, ":")[[1]]

  # Error messages from the prov.json will
  #typically have an uneeded prefix followed
  # by a colon ":"
  if(length(split) > 1) {
    error.message <- split[-1]
  }

  # This complicated mess of regex i=actually checks for 4 things (all inclusive):
  # Matches to characters surronded by quotes "dog"
  # Matches to characters surronded by escaped quotes \"dog\"
  # Matches to characters surronded by single quotes 'dog'
  # Matches to characters surronded by escaped quotes \'dog\'
  exp <- "\\\"[^\"\r]*\\\"|\"[^\"\r]*\"|\'[^\"\r]*\'|\\\'[^\"\r]*\\\'"

  gsub(exp, "", error.message, perl = T)
}

debug.warning.trace <- function()
{
	
}

# === UTILITY ================================================================ #

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
.flatten.args <- function(...) 
{
	# TODO - do i need this function?
	return(unlist(list(...)))
}

.get.valid.args <- function(pos.args, ...)
{
	# unnest list of arguments
	args <- unique(unlist(list(...)))
	
	# if there are no arguments, get all possible arguments.
	if(is.null(args))
		return(pos.args)
	
	# Make sure all the results passed by the user are valid
	# this produces a list of logicals, where TRUES
	#correspond to valid inputs
	pos.args <- lapply(args, function(arg)
	{
		if(arg %in% pos.args) {
			return(TRUE)
		} else {
			cat(paste(arg, "is not a possible result\n"))
			return(FALSE)
		}
	})

	# Any non-valid inputs will be removed as the list is subset
	# by logicals, TRUE corresponding to valid inputs
	args <- args[unlist(pos.args)]
	
	# list out possible results if none are valid
	if(length(args) == 0)
	{
		cat("Options:\n")
		print(unlist(pos.args))
		return(NULL)
	}
	
	return(args)
}


# TODO (enable specification of startLine)
.get.lineage <- function(arg, forward = FALSE)
{
	data.nodes <- .debug.env$data.nodes
	
	# get node id - TODO (enable specification of startLine)
	# get first node if forward == T, last node if forward == FALSE
	node.id <- NULL
	if(forward)
		node.id <- utils::head(n=1,data.nodes[data.nodes$name == arg, ])$id
	else
		node.id <- utils::tail(n=1,data.nodes[data.nodes$name == arg, ])$id
		
	# get lineage
	lineage <- provGraphR::get.lineage(.debug.env$graph, node.id, forward = forward)
	lineage <- lineage[grep('^p[[:digit:]]+', lineage)]
	
	# if getting the forward lineage, get the proc node which first assigned the variable
	if(forward)
	{
		proc.data <- .debug.env$proc.data
		edge <- proc.data[proc.data$entity ==  node.id, ]
		
		if(nrow(edge) > 0)
			lineage <- append(lineage, edge$activity[[1]], 0)
	}
	
	return(lineage)
}
