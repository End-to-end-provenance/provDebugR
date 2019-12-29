# The debug environment
.debug.env <- new.env(parent = emptyenv())

# parsed provenance and adjacency graph
.debug.env$prov <- NULL
.debug.env$graph <- NULL
.deubg.env$has.graph <- FALSE

# procedure nodes, data nodes
.debug.env$proc.nodes <- NULL
.debug.env$data.nodes <- NULL

# data-to-procedure edges, procedure-to-data edges
.debug.env$data.proc <- NULL
.debug.env$proc.data <- NULL

# === INIT =================================================================== #

# uses last json generated
#' @export
prov.debug <- function()
{
	# determine which provenance collection tool to use
	tool <- .get.tool()
	
	if(tool == "rdtLite")
		prov.json <- rdtLite::prov.json
	else
		prov.json <- rdt::prov.json
	
	# pass json to helper function to initialise debugger
	.debug.init(prov.json(), is.file = FALSE)
}

# uses json from file
#' @export
prov.debug.file <- function(prov.file)
{
	.debug.init(prov.file, is.file = TRUE)
}

# runs provenance-collection first
#' @export
prov.debug.run <- function(script)
{
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
		prov.run(script)
	}, finally = {
		cat(paste (tool, "is finished running.\n"))
		
		# initialise debugger
		.debug.init(prov.json(), is.file = FALSE)
	})
}

# === HELPER FUNCTIONS ======================================================= #

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
	
	# empty case
	if(is.null(.debug.env$graph)) {
		.debug.env$has.graph <- FALSE
		stop("The provenance is empty.")
	}
	
	.deubg.env$has.graph <- TRUE
	
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

# returns the full code for each Operation procedure node
#' @noRd
.get.full.code <- function()
{
	proc.nodes <- .debug.env$proc.nodes
	
	# get code lines for each script
	scripts <- provParseR::get.saved.scripts(.debug.env$prov)$script
	
	lines <- lapply(scripts, function(script) 
	{
		# read file
		file <- file(script, "r", encoding = getOption("encoding"))
		line.list <- readLines(file, warn = FALSE)
		close(file)
		
		return(line.list)
	})
	
	# get full code for each proc node
	codes <- sapply(1:nrow(proc.nodes), function(i)
	{
		node <- proc.nodes[i, ]
		
		# case: not an operation
		if(node$type != "Operation")
			return(node$name)
		
		# get full code
		# if procedure has more than 1 line, collapse the lines into 1 before returning
		if(node$endLine - node$startLine == 0)
			return(lines[[node$scriptNum]][node$startLine])
		
		code <- lines[[node$scriptNum]][node$startLine:node$endLine]
		return(paste(code, sep="", collapse = "\n"))
	})
	
	return(unname(codes))
}
