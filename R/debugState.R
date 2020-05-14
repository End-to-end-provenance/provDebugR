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

# === STATE ================================================================== #

#' The State at a Line
#'
#' For each queried line, debug.state returns a data frame showing the state
#' at that line, after it has been executed.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item name: The names of variables in the state.
#'		\item value: The value of each variable.
#'		\item container: The type of the container of each variable.
#'		\item dimension: The size of the container.
#'		\item type: The data type(s) contained within the container.
#'		\item scriptNum: The script number associated with each variable.
#'		\item startLine: The line number associated with each variable.
#' }
#' If no paramters are given, debug.state will return the state at the end of
#' execution.
#'
#' debug.state belongs to provDebugR, a debugger which utilises provenance collected
#' post-execution to facilitate understanding of the execution and aid in debugging.
#'
#' This function may be used only after the debugger has been initialised using
#' one its initialisation functions (listed below).
#'
#' @param ... The line numbers to be queried.
#' @param script.num The script number of the queried line numbers. This is ignored
#'                   if no line numbers are given.
#'                   Allows for only 1 script number to be queried per function call.
#'                   Defaults to script number 1 (main script).
#'
#' @return A list of data frames of states for each queried line number, or the state
#'         at the end of execution if no parameters are given to the function. 
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
#' @seealso \code{\link{debug.type.changes}}: Returns a data frame for each variable in
#'              the execution containing the instances where the data type changed.
#' @seealso \code{\link{debug.variable}}: Returns a data frame showing all instances
#'              of the variable(s) queried.
#' @seealso \code{\link{debug.view}}: Opens and displays the contents of each file or variable
#'              or variable queried.
#' @seealso \code{\link{debug.warning}}: Returns the backwards lineage of the queried
#'              warning(s), if any.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R")
#' debug.state()
#' debug.state(5)
#' debug.state(10, 20, script.num = 2)
#' }
#'
#' @export
#' @rdname debug.state
debug.state <- function(..., script.num = 1)
{
	# CASE: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: Get valid query
	# This checks that all queries are integers, 
	# and there is only 1 (integer) script number specified.
	query <- .get.valid.query.state(..., script.num = script.num)
	
	# If query is null, that means there are no valid queries or the query itself is empty.
	# In this case, we want to show the state at the end of execution.
	# We want to keep a variable to track that the end of execution is automatically shown.
	end.of.execution <- FALSE
	
	if(is.null(query)) {
		end.of.execution <- TRUE
		cat("State at the end of execution:\n")
		
		proc.nodes <- .debug.env$proc.nodes[.debug.env$proc.nodes$type == "Operation", ]
		query <- proc.nodes[nrow(proc.nodes), c("startLine", "scriptNum")]
	}
	
	# STEP: Get state for each query
	# Keep a vector to keep track of indicies where there are queries with no state
	# There should be much fewer cases of no state than those with state.
	no.state <- c()
	
	states <- lapply(c(1:nrow(query)), function(i)
	{
		# Get the closest procedure node with line number <= queried line number
		# this could be the procedure from a previous script,
		# or 'p0' indicating the beginning of the execution.
		query.line <- query[i,1]
		query.script <- query[i,2]
		
		p.id <- .get.closest.proc(query.line, query.script)
		
		# loop up proc until get one with output node that is a variable
		d.id <- .get.last.var(p.id)
		
		# get state
		d.list <- .get.state(d.id)
		
		# case: no state
		if(is.null(d.list)) {
			no.state <<- append(no.state, i)
			
			cat("There is no state for line ", query.line, " in script ", 
				query.script, ".\n", sep='')
			return(invisible(NULL))
		}
		
		# get output for all variables in the state
		return(.get.output.state(d.list))
	})
	
	# Remove, if any, elements with no state.
	if(length(no.state) > 0) {
		query <- query[-no.state, ]
		states <- states[-no.state]
	}
	
	# CASE: no state to display at all
	if(length(states) == 0)
		return(invisible(NULL))
	
	# re-number rows of the table of queries
	row.names(query) <- c(1:nrow(query))
	
	# If not directly showing the end of execution, print table of queries with state.
	if(!end.of.execution) {
		cat("Results for:\n")
		print(query)
		cat('\n')
	}
	
	# Label output with indices of queries, return.
	names(states) <- row.names(query)
	return(states)
}

#' Returns a table of valid queries.
#' columns: startLine, scriptNum
#'
#' @param ... The user's line queries.
#' @param script.num The script number to be queried.
#' 
#' @return The table of valid queries.
#'         columns: startLine, scriptNum
#'
#' @noRd
.get.valid.query.state <- function(..., script.num = 1)
{
	# Get user's query
	query <- .flatten.args(...)
	
	if(is.null(query))
		return(invisible(NULL))
	
	# Check if script number is valid
	if(length(script.num) > 1) {
		warning("Please query only 1 script number.", call. = FALSE)
		return(invisible(NULL))
	}
	
	script.num <- .to.int(script.num)
	
	if(is.null(script.num) || is.na(script.num)) {
		warning("Script number must be a single integer.", call. = FALSE)
		return(invisible(NULL))
	}
	
	# check if there are proc nodes in that script
	pos.proc <- .debug.env$proc.nodes
	pos.proc <- pos.proc[pos.proc$scriptNum == script.num, ]
	pos.proc <- .remove.na.rows(pos.proc)
	
	if(nrow(pos.proc) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n"))
		return(invisible(NULL))
	}
	
	# For each query, ensure that it is an integer
	query <- lapply(query, function(line) 
	{
		line <- .to.int(line)
		
		if(is.null(line) || is.na(line))
			return(NULL)
		
		return(line)
	})
	
	# remove invalid queries
	query <- .remove.null(query)
	
	if(length(query) == 0) {
		cat("No valid queries.\n")
		return(invisible(NULL))
	}
	
	# get unique queries
	query <- unique(as.vector(unlist(query)))
	
	# Bind valid queries to script num
	queries <- data.frame(query, rep(script.num, length(query)),
						  stringsAsFactors = FALSE)
	colnames(queries) <- c("startLine", "scriptNum")
	return(queries)
}

#' Get closest procedure node id with line number <= queried line number
#' Returns NULL if a procedure node is not found.
#'
#' @param line The queried line number.
#' @param script.num The queried script number. Guarenteed to have associated procedure nodes.
#'
#' @return The procedure node id of the closest procedure node with line number less
#'		   than or equal to the given line number, or NULL if none are found.
#'
#' @noRd
.get.closest.proc <- function(line, script.num)
{
	# Get list of all possible lines for the specified script
	proc.nodes <- .debug.env$proc.nodes
	proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
	
	script.proc <- proc.nodes[proc.nodes$scriptNum == script.num, ]
	
	# Find the index of where the queried line falls right after in the list of
	# line numbers of the script, or 0 if the queried line falls before them. 
	script.index <- .find.num.loc(script.proc$startLine, line)
	
	# Case: 0 (line falls before the list of numbers in the script)
	# Returns the p.id of the node before the first node of the script,
	# or NULL if the top of the procedure nodes table has been reached.
	if(script.index == 0)
	{
		proc.index <- c(1:nrow(proc.nodes))[proc.nodes$id == script.proc$id[1]]
		proc.index <- proc.index - 1
		
		if(proc.index == 0)
			return(NULL)
		
		return(proc.nodes$id[proc.index])
	}
	
	# Case: line falls somewhere in the list or after it.
	return(script.proc$id[script.index])
}

#' From the given procedure node id, loop up the table of procedure nodes until
#' an output variable is found. Returns the data node id of the last variable,
#' or NULL if none are found. Also returns NULL if the procedure node id given is NULL.
#'
#' @param The procedure node id where the search for an variable begins.
#'
#' @return The data node id of the variable found, or NULL if none are found.
#' @noRd
.get.last.var <- function(p.id)
{
	# Case: p.is is NULL
	if(is.null(p.id))
		return(NULL)
	
	# Starting from the given procedure node, loop up the table of
	# operations until an output edge to a variable is found.
	proc.nodes <- .debug.env$proc.nodes
	proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
	
	start.index <- c(1:nrow(proc.nodes))[proc.nodes$id == p.id]
	
	for(i in c(start.index:1))
	{
		# check if there are output data nodes.
		p.id <- proc.nodes$id[i]
		d.nodes <- .debug.env$proc.data$entity[.debug.env$proc.data$activity == p.id]
		
		# case: no output edges found. check next procedure node
		if(length(d.nodes) == 0)
			next
		
		# for each data node found, check if it is a variable.
		# extract the variables
		vars <- sapply(d.nodes, function(d.id)
		{
			node <- .debug.env$data.nodes[.debug.env$data.nodes$id == d.id, ]
			return(node$type == "Data" || node$type == "Snapshot")
		})
		
		vars <- d.nodes[vars]
		
		# if there are variables, return the id of the last one found 
		# (there could be multiple)
		if(length(vars) > 0)
			return(vars[length(vars)])
	}
	
	# reaching here means that no output variables could be found. return NULL
	return(NULL)
}

#' Returns the state for a given data node as a vector of data node id.
#' This is done by obtaining all data nodes up to the specified data node id
#' and obtaining the last occurance of each unique variable name.
#'
#' @param d.id The data node id for the data node where the state should be
#'             obtained for.
#'
#' @return The state. A vector of data node id.
#' @noRd
.get.state <- function(d.id)
{
	# Extract variables from data nodes table.
	data.nodes <- .debug.env$data.nodes
	data.nodes <- data.nodes[(data.nodes$type == "Data" | data.nodes$type == "Snapshot"), ]
	
	# Get the id and name of data nodes where fromEnv is TRUE, if any.
	# These are guarenteed to be variables.
	vars <- data.nodes[data.nodes$fromEnv, c("id", "name")]
	
	# Then, get the variables up until the specified data node id.
	# This is appended to the table of fromEnv nodes before
	# obtaining the last occurnce of each unique variable.
	# fromEnv nodes will always have unique variable names.
	if(!is.null(d.id))
	{
		# Get all data nodes up to specified d.id
		max.index <- c(1:nrow(data.nodes))[data.nodes$id == d.id]
		vars <- rbind(vars, data.nodes[c(1:max.index), c("id", "name")], stringsAsFactors = FALSE)
		
		# For each unique variable name, get the last data node id,
		# if there are multiple occurrences of it.
		unique.names <- unique(vars$name)
		
		if(length(unique.names) < nrow(vars))
		{
			state <- sapply(unique.names, function(var.name)
			{
				# get all data nodes for the specified variable name
				id.list <- vars$id[vars$name == var.name]
				
				# sort by increasing data node number
				id.nums <- as.integer(sub('d', '', id.list))
				id.list <- id.list[order(id.nums)]
				
				# return the last node
				return(id.list[length(id.list)])
			})
			
			return(state)
		}
	}
	
	# If there are no variables, there is no state. Return NULL.
	if(nrow(vars) == 0)
		return(NULL)
	
	# Reaching here means all unique variable names have 1 data node associated
	# Sort table by increasing d.id, return.
	id.nums <- as.integer(sub('d', '', vars$id))
	return(vars$id[order(id.nums)])
}

#' From the given list of data node id, form user output.
#' columns: name, value, container, dimension, type, scriptNum, startLine
#'
#' @param id.list A vector of data node id which forms the state.
#' 
#' @return The state. A data frame.
#'         columns: name, value, container, dimension, type, scriptNum, startLine
#'
#' @noRd
.get.output.state <- function(id.list)
{
	# For each variable in the state, obtain fields:
	# var name, value, valType, scriptNum, startLine
	rows <- lapply(id.list, function(d.id)
	{
		d.node <- .debug.env$data.nodes[.debug.env$data.nodes$id == d.id, ]
		
		# Get fields from data node table
		# columns: name, value
		d.fields <- d.node[ , c("name", "value")]
		
		# Get valType (remove id column)
		val.type <- provParseR::get.val.type(.debug.env$prov, d.id)
		val.type <- val.type[ , c("container", "dimension", "type")]
		
		# Get start line and script number
		# For fromEnv variables, these are NA.
		if(d.node$fromEnv)
		{
			p.fields <- data.frame(scriptNum = NA, startLine = NA, stringsAsFactors = FALSE)
		}
		else
		{
			# For non-fromEnv variables, find the procedure node associated with it.
			# As the variables in the state are either fromEnv nodes or were produced
			# by an operation within the script, non-fromEnv variables will always
			# have 1 procedure-to-data edge linking it to an Operation procedure node.
			p.id <-  .get.p.id(d.id)
			
			# Get fields from procedure node (scriptNum, startLine)
			p.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes == p.id, ]
			p.fields <- p.fields[ , c("scriptNum", "startLine")]
			p.fields <- .remove.na.rows(p.fields)
		}
		
		# Combine fields into a row
		return(cbind(d.fields, val.type, p.fields, stringsAsFactors = FALSE))
	})
	
	# Combine rows into a data frame.
	return(.form.df(rows))
}
