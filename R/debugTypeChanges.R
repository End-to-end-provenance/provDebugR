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

# === TYPE CHANGES =========================================================== #

#' Tracking Type Changes
#' 
#' Returns a data frame for each variable in the execution containing the 
#' instances where the data type changed.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item value: The value of the variable.
#'		\item container: The type of the container of the variable.
#'		\item dimension: The size of the container.
#'		\item type: The data type(s) contained within the container.
#'		\item code: The line of code associated with the variable.
#'		\item scriptNum: The script number associated with the variable.
#'		\item startLine: The line number associated with the variable.
#' }
#'
#' @param var Optional. Variable name(s) to be queried. If not NA, the results will
#'            be filtered to show only those with the given variable name.
#'
#' @return A list of data frames for each variable with at least 1 data type change.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R")
#' debug.type.changes()
#' debug.type.changes("x")
#' debug.type.changes(var = c("a", "b"))
#' }
#'
#' @export
#' @rdname debug.type.changes
debug.type.changes <- function(var = NA)
{
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# Get all data nodes with type "Data" or "Snapshot"
	data.nodes <- .debug.env$data.nodes
	data.nodes <- data.nodes[data.nodes$type == "Data" | data.nodes$type == "Snapshot", ]
	data.nodes <- .remove.na.rows(data.nodes)
	
	# case: no variables
	if(nrow(data.nodes) == 0) {
		cat("There are no variables.\n")
		return(invisible(NULL))
	}
	
	vars.names <- unique(data.nodes$name)
	
	# Find all variables with type changes
	# This functions differs from others that accept queries in that
	# checking the validity of user's query is done after all variables
	# with type changes is found.
	remove.indices <- c()
	
	vars <- lapply(c(1:length(vars.names)), function(i)
	{
		# get data nodes with that name
		nodes <- data.nodes[data.nodes$name == vars.names[i], ]
		
		if(nrow(nodes) == 1) {
			remove.indices <<- append(remove.indices, i)
			return(NULL)
		}
		
		# number of nodes > 1 (can compare valTypes)
		# keep indices of nodes with type change
		type.changes <- c()
		
		lapply(c(2:nrow(nodes)), function(i) {
			if(nodes$valType[i] != nodes$valType[i-1])
				type.changes <<- append(type.changes, c(i-1, i))
		})
		
		type.changes <- unique(type.changes)
		
		if(length(type.changes) == 0) {
			remove.indices <<- append(remove.indices, i)
			return(NULL)
		}
		
		# extract specified nodes with type changes
		nodes <- nodes[type.changes, ]
		
		return(.get.output.type.changes(nodes))
	})
	
	if(length(remove.indices) > 0) {
		vars.names <- vars.names[-remove.indices]
		vars <- vars[-remove.indices]
	}
	
	names(vars) <- vars.names
	
	# if the user has specified variable(s) to be queried, get the valid ones
	# for this function, this process is much simpler than get.valid.var
	# first, remove repeated user queries
	var <- unique(var)
	
	if(!(is.na(var[1]) && length(var) == 1)) 
	{
		valid.queries <- var[var %in% vars.names]
		
		# no valid variables
		if(length(valid.queries) == 0) {
			cat("No valid variables.\n\n")
			.print.pos.options(vars.names)
			return(invisible(NULL))
		}
		
		# extract queried results from list of all possible type changes
		vars <- lapply(valid.queries, function(query) {
			return(vars[[grep(query, vars.names)]])
		})
		
		names(vars) <- valid.queries
	}
	
	return(vars)
}

#' Forms user output.
#' columns: value, container, dimension, type, code, scriptNum, startLine
#'
#' @param data.nodes The data nodes to be displayed to the user.
#'
#' @return The data frame of type changes to be returned to the user.
#'         columns: value, container, dimension, type, code, scriptNum, startLine
#' @noRd
.get.output.type.changes <- function(data.nodes)
{
	# script num, line num, full code, value, valType
	# for each data node (row), get required fields for output
	rows <- lapply(c(1:nrow(data.nodes)), function(i)
	{
		# from data nodes (parameter), extract id, value
		data.id <- data.nodes$id[i]
		data.value <- data.nodes$value[i]
		
		# get valType columns (remove id column)
		val.type <- provParseR::get.val.type(.debug.env$prov, node.id = data.id)
		val.type <- val.type[ , c("container", "dimension", "type")]
		
		# get proc node which either set or first used the data node
		proc.id <- .get.p.id(data.id)[1]
		
		# extract script num, line num, code from proc nodes
		proc.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes$id == proc.id, 
											 c("name", "scriptNum", "startLine")]
		
		# combine fields
		fields <- cbind(data.value, val.type, proc.fields, stringsAsFactors = FALSE)
		names(fields) <- c("value", 
						   "container", "dimension", "type", 
						   "code", "scriptNum", "startLine")
		return(fields)
	})
	
	return(.form.df(rows))
}
