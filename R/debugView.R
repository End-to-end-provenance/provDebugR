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

#' @export
debug.view <- function(..., start.line = NA, script.num = 1)
{
	# CASE: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: get all possible variables
	# data nodes must have type = "Data" or "Snapshot" to be considered a variable
	pos.vars <- .extract.vars(.debug.env$data.nodes)
	
	# case: no variables
	if(nrow(pos.vars) == 0) {
		cat("There are no variables.\n")
		return(invisible(NULL))
	}
	
	# STEP: Get user's query
	# cols: name, valType, startLine, scriptNum
	query.vars <- .flatten.args(...)
	queries <- .get.query.var(query.vars, val.type = NA, 
							  start.line = start.line, script.num = script.num)
	
	# STEP: Get valid queries
	# remove valType column (col 3)
	# resulting cols: d.id, name, startLine, scriptNum
	valid.queries <- .get.valid.query.var(pos.vars, queries, forward = FALSE)
	valid.queries <- valid.queries[ ,-3]
	
	# case: no valid queries
	if(is.null(valid.queries)) {
		.print.pos.options(pos.vars[ , c("name", "startLine", "scriptNum")])
		return(invisible(NULL))
	}
	
	# STEP: get pointer to var.env and clear it before loading variables
	var.env <- .debug.env$var.env
	rm(list = ls(var.env), envir = var.env)
	
	# STEP: get path to prov directory
	prov.dir <- .debug.env$prov.dir
	
	# STEP: for every valid query, load variables into var.env before viewing
	results <- lapply(c(1:nrow(valid.queries)), function(i)
	{
		query <- valid.queries[i, ]
		
		# get row from data nodes table
		data.node <- pos.vars[pos.vars$id == query$d.id, ]
		
		# since there could be multiple of the same var names, lines, script num,
		# form a new var name the data is loaded into
		# this becomes the title when View is called
		var.name <- paste(query$name, "_", "line", query$startLine, "_",
						  "script", query$scriptNum, sep="")
		
		# load variable into var.env
		load <- load.var(var.env, var.name, data.node$value, 
						 data.node$valType, prov.dir)
		
		# form and return row showing load status
		# view those with load == TRUE
		if(load)
		{
			View(get(var.name, envir = var.env), title = var.name)
			
			return(cbind(query[ ,-1], 
						 title = var.name, 
						 notes = NA, 
						 stringsAsFactors = FALSE))
		}
		else
		{
			return(cbind(query[ ,-1], 
						 title = var.name, 
						 notes = "INCOMPLETE", 
						 stringsAsFactors = FALSE))
		}
	})
	
	# STEP: bind results into single data frame and return
	results <- form.df(results)
	return(results)
}

# EDITS
# what must happen for all variables, regardless of how i load the rest of the vars
load.var <- function(var.env, var.name, var.value, val.type, prov.dir)
{
	# check for snapshot
	# data/<name>.<ext>
	if(grepl("^data/.*\\.[^\\]+$", var.value))
	{
		# split into file name and file extension
		file.parts <- strsplit(var.value, "\\.")[[1]]
		file.ext <- tolower(file.parts[length(file.parts)])
		file.name <- paste(file.parts[-length(file.parts)], collapse = ".")
		
		# text file
		# has been stored as an RObject
		if(file.ext == "txt")
		{
			full.path <- paste(prov.dir, "/", file.name, ".RObject", sep = "")
			
			# if file exists, use separate environment to load into
			# to prevent overwriting anything in .GlobalEnv
			if(file.exists(full.path)) 
			{
				load.env <- new.env()
				var <- load(full.path, envir = load.env)
				assign(var.name, get(var, envir = load.env), envir = var.env)
				
				# clear load.env before returning
				rm(list = ls(load.env), envir = load.env)
				return(TRUE)
			}
			
			assign(var.name, var.value, envir = var.env)
			return(FALSE)
		}
		
		# csv
		if(file.ext == "csv")
		{
			full.path <- paste(prov.dir, "/", file.name, ".csv", sep = "")
			
			# file does not exist
			if(!file.exists(full.path)) {
				assign(var.name, var.value, envir = var.env)
				return(FALSE)
			}
			
			# split valtype into parts to get container
			val.type <- jsonlite::fromJSON(val.type)
			container <- val.type$container
			dim <- val.type$dimension
			
			# use read.csv
			var <- utils::read.csv(full.path, stringsAsFactors = FALSE)
			
			# data frame
			if(container == "data_frame") 
			{
				assign(var.name, var, envir = var.env)
				return(TRUE)
			}
			
			# vector
			if(container == "vector")
			{
				var <- rep("", length.out = as.integer(dim))
				assign(var.name, var, envir = var.env)
				return(TRUE)
			}
			
			# extract each column and bind together for multi-dimensional objects
			cols <- c(1:ncol(var))
			var <- cbind(sapply(cols, function(i){
				return(var[[i]])
			}))
			
			# matrix
			if(container == "matrix")
			{
				var <- as.matrix(var)
				assign(var.name, var, envir = var.env)
				return(TRUE)
			}
			
			# array
			if(container == "array")
			{
				var <- as.array(var)
				assign(var.name, var, envir = var.env)
				return(TRUE)
			}
			
			# no identifiable container
			assign(var.name, var.value, envir = var.env)
			return(FALSE)
		}
		
		# no identifiable file extension
		assign(var.name, var.value, envir = var.env)
		return(FALSE)
	}
	
	# not a snapshot
	# this works on simple values only. does not work for lists
	val.type <- jsonlite::fromJSON(val.type)
	assign(var.name, methods::as(var.value, val.type$type), envir = var.env)
	return(TRUE)
}
