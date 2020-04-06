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

# === LINE =================================================================== #

#' Tracking the Inputs and Outputs at a Line
#' 
#' For each line number queried, debug.line returns a data frame of the data 
#' that the procedure in that line inputs and outputs.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item name: The name of the data.
#'		\item value: The value of the data.
#'		\item container: The type of the container of the data.
#'		\item dimension: The size of the container.
#'		\item type: The data type(s) contained within the container.
#' }
#'
#' debug.line belongs to provDebugR, a debugger which utilises provenance collected
#' post-execution to facilitate understanding of the execution and aid in debugging.
#'
#' This function may be used only after the debugger has been initialised using
#' one its initialisation functions (listed below).
#'
#' @param ... The line numbers to be queried.
#' @param script.num The script number of the queried line numbers.
#'                   Allows for only 1 script number to be queried per function call.
#'                   Defaults to script number 1 (main script).
#' @param all If TRUE, the inputs and outputs for all lines in the specified script
#'            will be returned.
#'
#' @return A list of data frames showing the inputs and outputs for the procedure
#'         in each line queried.
#'
#' @seealso provDebugR Initialisation Functions: 
#' @seealso \code{\link{prov.debug}}
#' @seealso \code{\link{prov.debug.file}} 
#' @seealso \code{\link{prov.debug.run}}
#'
#' @seealso Other provDebugR Functions (non-initialisation):
#' @seealso \code{\link{debug.error}}: Returns the backwards lineage of the error, if any.
#'              The error may be queried on StackOverflow.
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
#' debug.line(5)
#' debug.line(all = TRUE)
#' debug.line(5, 10, script.num = 2)
#' }
#'
#' @export
#' @rdname debug.line
debug.line <- function(..., script.num = 1, all = FALSE)
{
	# CASE: no provenance
	if(! .debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: get all possible options
	# extract columns: id, startLine, scriptNum
	pos.nodes <- .debug.env$proc.nodes[.debug.env$proc.nodes$type == "Operation", ]
	pos.nodes <- pos.nodes[ , c("id", "startLine", "scriptNum", "name")]
	names(pos.nodes) <- c("id", "startLine", "scriptNum", "code")
	
	# STEP: get user's query
	# case: more than 1 script number queried
	if(length(script.num) > 1) {
		warning("Please query only 1 script number.", call. = FALSE)
		.print.pos.options(pos.nodes[ , -1])
		return(invisible(NULL))
	}
	
	if(all)
		query <- unique(pos.nodes$startLine)
	else
		query <- unique(.flatten.args(...))
	
	# case: ensure script.num is an integer or can be coerced into an integer
	script.num.int <- .to.int(script.num)
	
	if(is.null(script.num.int)) {
		warning("Script number must be a single integer.", call. = FALSE)
		.print.pos.options(pos.nodes[ , -1])
		return(invisible(NULL))
	}
	
	script.num <- script.num.int
	
	# STEP: get valid queries
	# columns: id, startLine, scriptNum, code
	valid.queries <- .get.valid.query.line(pos.nodes, query, script.num)
	
	# CASE: no valid queries
	if(is.null(valid.queries)) {
		.print.pos.options(pos.nodes[ , -1])
		return(invisible(NULL))
	}
	
	# STEP: Form user output
	# Make sure to keep track of indices with neither input nor output data nodes
	# It's likely the shorter list
	remove.indices <- c()
	
	output <- lapply(c(1:nrow(valid.queries)), function(i) 
	{
		# get user output for the specified proc node id
		p.id <- valid.queries$id[i] 
		user.output <- .get.output.line(p.id)
		
		# case: no input or output data nodes (keep track of index!)
		if(is.null(user.output)) {
			remove.indices <<- append(remove.indices, i)
			return(NULL)
		}
		
		# return user output
		return(user.output)
	})
	
	# CASE: there are valid line queries with neither input nor output data nodes
	if(length(remove.indices) > 0)
	{
		# print out table of queries with no output
		no.output <- valid.queries[remove.indices, -1]
		
		cat("No input or output data nodes associated with:\n")
		print(no.output)
		
		# Case: all queries have neither input nor output nodes
		if(length(remove.indices) == nrow(valid.queries))
			return(invisible(NULL))
		
		# There is output to be shown to user,
		# make sure to put a line gap.
		cat("\n")
		
		# from table of valid queries, remove rows and re-number them
		valid.queries <- valid.queries[-remove.indices, ]
		row.names(valid.queries) <- c(1:nrow(valid.queries))
		
		# remove all null cells from output
		output <- .remove.null(output)
	}
	
	# STEP: Print out valid queries before returning output
	cat('Results for:\n')
	print(valid.queries[ , -1])
	cat('\n')
	
	names(output) <- row.names(valid.queries)
	return(output)
}

#' Returns a data frame of valid line number queries. Used in debug.line .
#' columns: id, startLine, scriptNum, code
#'
#' @param pos.nodes Table of all possible options. In this case this
#'                  refers to the table of procedure nodes.
#' @param query A list of the user's line number queries.
#' @param script.num The queried script number. Only 1 may be queried per call.
#'
#' @return A data frame of valid line number queries
#'         columns: id, startLine, scriptNum, code
#'
#' @noRd
.get.valid.query.line <- function(pos.nodes, query, script.num)
{
	# CASE: no queries
	if(is.null(query))
		return(invisible(NULL))
	
	# QUERY: subset by script number
	# since there can only be 1 script number queried by call,
	# check for its validity first.
	pos.nodes <- pos.nodes[pos.nodes$scriptNum == script.num, ]
	pos.nodes <- .remove.na.rows(pos.nodes)
	
	# CASE: invalid script number
	if(nrow(pos.nodes) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n\n"))
		return(invisible(NULL))
	}
	
	# QUERY: for each query, return TRUE if valid, FALSE otherwise
	valid.cells <- sapply(query, function(line)
	{
		# first, convert into an int
		line <- .to.int(line)
		
		# Case: line is NULL (can't be converted to an int or is invalid)
		if(is.null(line))
			return(FALSE)
		
		# search for queried line in list of possible start lines
		return(line %in% pos.nodes$startLine)
	})
	
	# Extract valid queries, return NULL if there are none.
	valid.lines <- query[valid.cells]
	
	if(length(valid.lines) == 0) 
	{
		cat("There are no valid queries.\n\n")
		return(NULL)
	}
	
	# For each valid line, extract appropriate row from pos.nodes
	# bind into data frame and return.
	rows <- lapply(valid.lines, function(line) {
		return(pos.nodes[pos.nodes$startLine == line, ])
	})
	
	return(unique(.form.df(rows)))
}

#' Returns a list of 2 data frames for the input and output data nodes of the
#' given procedure node.
#' columns: name, value, container, dimension, type
#'
#' @param p.id The procedure node id.
#'
#' @return A list of 2 data frames for the input and output data nodes of the
#'         given procedure node.
#'         columns: name, value, container, dimension, type
#'
#' @noRd
.get.output.line <- function(p.id)
{
	# This is a list which stores tables for input and output data nodes
	result <- list(input = NA, output = NA)
	
	# INPUT data nodes
	# get all input data node id associated with activity id
	# then form user output table
	input.dnum <- .debug.env$data.proc$entity[.debug.env$data.proc$activity == p.id]
	
	if(length(input.dnum) > 0)
		result$input <- .get.output.line.helper(input.dnum)
	
	# OUTPUT data nodes
	# same as for input data nodes, but for output data nodes instead
	output.dnum <- .debug.env$proc.data$entity[.debug.env$proc.data$activity == p.id]
	
	if(length(output.dnum) > 0)
		result$output <- .get.output.line.helper(output.dnum)
	
	# CASE: no input or output nodes
	if(length(input.dnum) == 0 && length(output.dnum) == 0)
		return(NULL)
	
	# return
	return(result)
}

#' A helper function for .get.output.line .
#' For each data node in the list given, extract node information to be returned to
#' the user and binds the resulting list of lists into a data frame.
#' columns: name, value, container, dimension, type
#'
#' @param dnum.list A vector of data node id.
#'
#' @return A data frame of information about the given data nodes to be returned to the user.
#'         columns: name, value, container, dimension, type
#'
#' @noRd
.get.output.line.helper <- function(dnum.list)
{
	rows <- lapply(dnum.list, function(dnum)
	{
		data.node <- .debug.env$data.nodes[.debug.env$data.nodes$id == dnum, c("name", "value")]
		val.type <- provParseR::get.val.type(.debug.env$prov, dnum)[c("container", "dimension", "type")]
		return(cbind(data.node, val.type, stringsAsFactors = FALSE))
	})
	
	# form data frame, return
	return(.form.df(rows))
}
