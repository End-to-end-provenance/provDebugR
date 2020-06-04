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

# === VIEW ================================================================== #

#' Viewing Files and Variables
#'
#' debug.view displays the contents of each file or variable queried.
#' For snapshots or files with the file extension of .csv or .txt, the data
#' will be loaded into the debugger environment before it is viewed. Otherwise,
#' the data will be viewed using the system's default program for that type of file.
#' Additionally, a data frame showing what the function has opened will be returned,
#' which contains the following columns:
#' \itemize{
#'  	\item name: The name of the variable or file being viewed.
#'  	\item startLine: The line number the variable or file is associated with. 
#'  	\item scriptNum: The script number the variable or file is associated with.
#'  	\item scriptName: The name of the script the variable or file is associated with.
#'  	\item title: The title of the variable or file when viewed.
#'  	\item notes: Will display PARTIAL if the variable is a partial snapshot, or
#'  	             indicate that the provenance directory or a file is not found.
#'  	             NA otherwise.
#' }
#'
#' debug.variable belongs to provDebugR, a debugger which utilises provenance 
#' collected post-execution to facilitate understanding of the execution and aid 
#' in debugging.
#'
#' This function may be used only after the debugger has been initialised using
#' one its initialisation functions (listed below).
#'
#' @param ... The variable names or file names to be queried.
#' @param start.line The line number of the queried variables or files. Optional.
#' @param script.num The script number of the queried variables or files.
#'                    Defaults to 1.
#'
#' @seealso provDebugR Initialisation Functions: 
#' @seealso \code{\link{prov.debug}}
#' @seealso \code{\link{prov.debug.file}} 
#' @seealso \code{\link{prov.debug.run}}
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
#' @seealso \code{\link{debug.warning}}: Returns the backwards lineage of the queried
#'              warning(s), if any.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R")
#' debug.view()
#' debug.view(x)
#' debug.view("x", y, start.line = 5, script.num = 2)
#' }
#'
#' @export
#' @rdname debug.view
debug.view <- function(..., start.line = "all", script.num = "all")
{
	# CASE: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: get all possible variables & files
	# data nodes must have type = "Data" or "Snapshot" to be considered a variable
	data.nodes <- .debug.env$data.nodes
	data.nodes <- data.nodes[data.nodes$type == "Data" | 
							 data.nodes$type == "Snapshot" |
							 data.nodes$type == "File", ]
	data.nodes <- .remove.na.rows(data.nodes)
	
	pos.nodes <- .get.pos.var(data.nodes)
	
	# case: no variables
	if(nrow(pos.nodes) == 0) {
		cat("There are no variables.\n")
		return(invisible(NULL))
	}
	
	# STEP: Get user's valid queries
	# resulting cols: d.id, name, startLine, scriptNum
	queries <- .get.query.var(unique(.flatten.args(...)),
							  start.line = start.line,
							  script.num = script.num)
	valid.queries <- .get.valid.query.var(pos.nodes, queries)
	
	# case: no valid queries
	if(is.null(valid.queries)) {
		.print.pos.options(pos.nodes[ , c("name", "startLine", "scriptNum", "scriptName")])
		return(invisible(NULL))
	}
	
	# STEP: get pointer to var.env and clear it before loading/viewing variables
	var.env <- .debug.env$var.env
	rm(list = ls(var.env), envir = var.env)
	
	# STEP: get path to data directory
	data.dir <- paste0(.debug.env$prov.dir, "/data")
	
	# STEP: for every valid query, load variables into var.env before viewing
	results <- lapply(c(1:nrow(valid.queries)), function(i)
	{
		query <- valid.queries[i, ]
		
		# get row from pos.nodes table
		node <- pos.nodes[pos.nodes$d.id == query$d.id, ]
		
		# since there could be multiple of the same var names, lines, script num,
		# form a new var name the data is loaded into
		# this becomes the title when View is called
		var.name <- paste(node$name, "_", "line", node$startLine, "_",
						  "script", node$scriptNum, sep="")
		
		status <- .view.var(var.env, var.name, node$value,
							node$valType, data.dir)
		
		return(cbind(node[ , c("name", "startLine", "scriptNum", "scriptName")], 
					 title = var.name, 
					 notes = status, 
					 stringsAsFactors = FALSE))
	})
	
	# STEP: bind results into single data frame and return
	results <- .form.df(results)
	
	cat("Viewing:\n\n")
	return(results)
}

#' Views the contents of a variable or file.
#'
#' @param var.env The variable environment, the environment into which the
#'            data should be loaded into.
#' @param var.name The name of the variable which the data, once loaded, is
#'            assigned to.
#' @param var.value The value of the variable, as stored in the prov.json file.
#' @param val.type The valType of the data.
#' @param data.dir The path to the data folder of the provenance directory.
#'
#' @return NA
#' @noRd
.view.var <- function(var.env, var.name, var.value, val.type, data.dir)
{
	# keep a variable for returning the status of the view to the user.
	# default is NA for successful load/view
	status <- NA
	
	# Check that var.value is a file and the the data directory exists
	# for snapshots/files, var.value could be: 
	# <path>/data/<name>.<ext>
	# data/<name>.<ext>
	is.file <- grepl("^.*[^\\]*data/.*\\.[^\\]+$", var.value)
	
	if(is.file && dir.exists(data.dir))
	{		
		# Split into file extension
		file.parts <- strsplit(var.value, "\\.")[[1]]
		file.ext <- tolower(file.parts[length(file.parts)])
		
		# Extract file name
		# var.value could be: <path>/data/<name>.<ext> or data/<name>.<ext>
		file.name <- paste(file.parts[-length(file.parts)], collapse = ".")
		file.name <- strsplit(file.name, "/")[[1]]
		file.name <- file.name[length(file.name)]
		
		# Check if it is a partial snapshot.
		# If it is, return PARTIAL instead of NA on successful view.
		if(grepl("PARTIAL", file.name))
			status <- "PARTIAL"
		
		# CASES: .RObject, .txt, .csv
		# for these cases, load value into var.env before viewing
		if(file.ext == "RObject" || file.ext == "csv" || file.ext == "txt")
		{
			path.RObject <- paste0(data.dir, "/", file.name, ".RObject")
			path.csv <- paste0(data.dir, "/", file.name, ".csv")
			path.txt <- paste0(data.dir, "/", file.name, ".txt")
			
			if(file.exists(path.RObject)) {
				.load.RObject(path.RObject, var.env, var.name)	
			}
			else if(file.exists(path.csv)) {
				.load.csv(path.csv, var.env, var.name, var.value, val.type)
			}
			else if(file.exists(path.txt)) {
				.load.txt(path.txt, var.env, var.name)
			}
			else {
				# reaching here means file can not be found
				assign(var.name, var.value, envir = var.env)
				status <- "File not found."
			}
			
			View(get(var.name, envir = var.env), title = var.name)
		}
		else  # everything else (not .RObject, .txt, or .csv)
		{
			# Use system to view file using system default application
			path <- paste0(data.dir, "/", file.name, ".", file.ext)
			
			if(file.exists(path)) {
				cmd <- paste0('open "', path, '"')
				system(cmd)
			}
			else {
				assign(var.name, var.value, envir = var.env)
				View(get(var.name, envir = var.env), title = var.name)
				status <- "File not found."
			}
		}
	}
	else  # Not a snapshot or path to data folder does not exist.
	{
		# assign the value directly to var.env
		# don't try to change its type as it will not work for anything
		# but simple values.
		assign(var.name, var.value, envir = var.env)
		View(get(var.name, envir = var.env), title = var.name)
		
		if(!dir.exists(data.dir))
			status <- "Provenance directory not found."
	}
	
	return(status)
}

#' Loads the contents of a .RObject file into the debugger environment.
#'
#' @param full.path The full path to the file.
#' @param var.env The variable environment, the environment into which the
#'            data should be loaded into.
#' @param var.name The name of the variable which the data, once loaded, is
#'            assigned to.
#'
#' @return NA
#' @noRd
.load.RObject <- function(full.path, var.env, var.name)
{
	# use separate environment to load value into
	# this is to prevent overwriting anything in .GlobalEnv
	load.env <- new.env()
	var <- load(full.path, envir = load.env)
	assign(var.name, get(var, envir = load.env), envir = var.env)
	
	# clear load.env before returning
	rm(list = ls(load.env), envir = load.env)
}

#' Loads the contents of a .csv file into the debugger environment.
#' This function will coerce the data into the type specified by the 'container'
#' attribute of its valType before it is assigned to var.env.
#'
#' @param full.path The full path to the file.
#' @param var.env The variable environment, the environment into which the
#'            data should be loaded into.
#' @param var.name The name of the variable which the data, once loaded, is
#'            assigned to.
#' @param var.value The value of the variable, as stored in the prov.json file.
#' @param val.type The valType of the data.
#'
#' @return NA
#' @noRd
.load.csv <- function(full.path, var.env, var.name, var.value, val.type)
{	
	# split valtype into parts to get container
	val.type <- jsonlite::fromJSON(val.type)
	container <- val.type$container
	dim <- val.type$dimension
	type <- val.type$type
	
	# use read.csv
	var <- utils::read.csv(full.path, colClasses = type, stringsAsFactors = FALSE)
	
	# data frame
	if(container == "data_frame") 
	{
		assign(var.name, var, envir = var.env)
	}
	else if(container == "vector")
	{
		var <- var[,1]
		assign(var.name, var, envir = var.env)
	}
	else   # multi-dimensional
	{
		# extract each column and bind together for multi-dimensional objects
		cols <- c(1:ncol(var))
		var <- cbind(sapply(cols, function(i) {
			return(var[[i]])
		}))
	
		# matrix
		if(container == "matrix")
		{
			var <- as.matrix(var)
			assign(var.name, var, envir = var.env)
		}
		else if(container == "array")
		{
			var <- as.array(var)
			assign(var.name, var, envir = var.env)
		}
		else  # unidentifiable container
		{
			assign(var.name, var.value, envir = var.env)
		}
	}
}

#' Loads the contents of a .txt file into the debugger environment.
#'
#' @param full.path The full path to the file.
#' @param var.env The variable environment, the environment into which the
#'            data should be loaded into.
#' @param var.name The name of the variable which the data, once loaded, is
#'            assigned to.
#'
#' @return NA
#' @noRd
.load.txt <- function(full.path, var.env, var.name)
{
	file <- file(full.path)
	lines <- readLines(full.path, warn = FALSE)
	lines <- paste(lines, collapse = "\n")
	close(file)

	assign(var.name, lines, envir = var.env)
}
