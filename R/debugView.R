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
debug.view <- function(var, start.line = NA, script.num = 1)
{
	# Get all possible variables
	
	# Get user's query
	# remove column 2 (valType)
	# resulting cols: name, startLine, scriptNum
	
	
	
	# for every query
}

# EDITS
load.variables <- function(d.id, prov.folder)
{
	data.nodes <- .debug.env$data.nodes
	
	load.env <- new.env()
	
	sapply(c(1:nrow(d.id)), function(i)
	{
		#d.node <- 
	})
}

# EDITS
# what must happen for all variables, regardless of how i load the rest of the vars
load.var <- function(var.env, var.name, var.value, val.type, prov.folder)
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
			full.path <- paste(prov.folder, "/", file.name, ".RObject", sep = "")
			
			# if file exists, use separate environment to load into
			# to prevent overwriting anything in .GlobalEnv
			if(file.exists(full.path)) 
			{
				load.env <- new.env()
				var <- load(full.path, envir = load.env)
				assign(var.name, get(var, envir = load.env), envir = var.env)
				
				# clear load.env before returning
				rm(list=ls(load.env), envir = load.env)
				return(TRUE)
			}
			
			assign(var.name, var.value, envir = var.env)
			return(FALSE)
		}
		
		# csv
		if(file.ext == "csv")
		{
			full.path <- paste(prov.folder, "/", file.name, ".csv", sep = "")
			
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
	return
	(TRUE)
}
