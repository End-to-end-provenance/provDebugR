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

# === VARIABLE =============================================================== #

#' Tracking Changes to a Variable
#' 
#' For each variable queried, debug.variable returns a data frame of all
#' instances (data nodes) of that variable.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item value: The value of the variable.
#'		\item container: The type of the container of the variable.
#'		\item dimension: The size of the container.
#'		\item type: The data type(s) contained within the container.
#'		\item scriptNum: The script number the variable is associated with.
#'		\item startLine: The line number the variable is associated with.
#' }
#'
#' debug.variable belongs to provDebugR, a debugger which utilises provenance 
#' collected post-execution to facilitate understanding of the execution and aid 
#' in debugging.
#'
#' This function may be used only after the debugger has been initialised using
#' one its initialisation functions (listed below).
#'
#' @param ... The variable names to be queried.
#' @param val.type Optional. If not NA, this filters the results to contain
#'                 only instances where the valType (container or type) has the
#'                 queried type. Only one type may be queried per function call.
#' @param script.num The script number of the queried variables.
#'                   Defaults to script number 1 (main script).
#' @param all If TRUE, results for all variables of the specified script will be
#'            returned.
#'
#' @return A list of data frames showing all instances of each variable queried.
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
#' @seealso \code{\link{debug.view}}: Opens and displays the contents of each file or variable
#'              or variable queried.
#' @seealso \code{\link{debug.warning}}: Returns the backwards lineage of the queried
#'              warning(s), if any.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R")
#' debug.variable("x")
#' debug.variable(all = TRUE)
#' debug.variable("a", "b", "x", val.type = "logical")
#' debug.variable("a", "b", "x", script.num = 3)
#' }
#'
#' @export
#' @rdname debug.variable
debug.variable <- function(..., val.type = NA, script.num = 1, all = FALSE)
{
	# CASE: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: get all possible variables
	# data nodes must have type = "Data" or "Snapshot" to be considered a variable
	# columns: d.id, p.id, name, valType, startLine, scriptNum
	data.nodes <- .extract.vars(.debug.env$data.nodes)
	pos.vars <- .get.pos.var(data.nodes)
	
	# CASE: no variables
	if(is.null(pos.vars)) {
		cat("There are no variables.\n")
		return(invisible(NULL))
	}
	
	# STEP: get user's query
	# columns: name, valType, startLine, scriptNum
	if(all)
		query.vars <- unique(pos.vars$name)
	else
		query.vars <- .flatten.args(...)
	
	query <- .get.query.var(query.vars, val.type = val.type, script.num = script.num)
	
	# STEP: get valid queries
	valid.queries <- .get.valid.query.var(pos.vars, query, forward = FALSE)
	
	# CASE: no valid queries
	if(is.null(valid.queries)) {
		.print.pos.options(pos.vars[ , c("name", "startLine", "scriptNum")])
		return(invisible(NULL))
	}
	
	# STEP: for each valid query, form table for user output
	output <- lapply(c(1:nrow(valid.queries)), function(i) {
		return(.get.output.var(valid.queries[i, ]))
	})
	
	names(output) <- valid.queries$name
	return(output)
}

#' For each possible data node find its corresponding procedure node.
#' Function shared with debug.lineage
#' columns: d.id, p.id, name, valType, startLine, scriptNum
#'
#' @param data.nodes A table of all possible data nodes
#'
#' @return The table of all possible data nodes, with the necessary fields found.
#'         columns: d.id, p.id, name, value, valType, startLine, scriptNum
#'
#' @noRd
.get.pos.var <- function(data.nodes)
{
	# CASE: no data nodes/variables
	if(nrow(data.nodes) == 0)
		return(NULL)
	
	# from data nodes, keep columns: id, name, value, valType
	# rename id column to d.id
	data.nodes <- data.nodes[ , c("id", "name", "value", "valType")]
	colnames(data.nodes) <- c("d.id", "name", "value", "valType")
	
	# for each data node, get the corresponding procedure node
	# some may have multiple proc nodes associated with this (files, url, fromEnv nodes etc.)
	rows <- lapply(c(1:nrow(data.nodes)), function(i)
	{
		# get id and row from data nodes table
		d.fields <- data.nodes[i, ]
		d.id <- d.fields$`d.id`
		
		# try to get the procedure node that produced/used the data node (could be multiple)
		p.id <- .get.p.id(d.id)
		
		# get startLine and scriptNum from proc nodes table, 
		# cbind with row from data nodes table
		row <- lapply(p.id, function(id) {
			p.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes$id == id,
											  c("id", "startLine", "scriptNum")]
			return(cbind(d.fields, p.fields, stringsAsFactors = FALSE))
		})
		
		# if there are multiple rows, combine into data frame
		if(length(row) == 1)
			row <- row[[1]]
		else
			row <- .form.df(row)
		
		return(row)
	})
	
	# bind into a single data frame
	rows <- .form.df(rows)
	
	# rename and rearrange columns
	colnames(rows) <- c("d.id", "name", "value", "valType", "p.id", "startLine", "scriptNum")
	rows <- rows[ , c("d.id", "p.id", "name", "value", "valType", "startLine", "scriptNum")]
	
	return(rows)
}

#' Get the user's queries, bound into a data frame.
#' Function shared with debug.lineage and debug.view
#' columns: name, valType, startLine, scriptNum
#'
#' @param query.vars The queried variables/data node names.
#' @param val.type valType queries, if any.
#' @param start.line line number queries, if any.
#' @param script.num The script number query.
#'
#' @return A data frame of the user's queries.
#'         columns: name, valType, startLine, scriptNum
#'
#' @noRd
.get.query.var <- function(query.vars, val.type = NA, start.line = NA, script.num = 1)
{
	# CASE: no queried variables
	if(is.null(query.vars))
		return(NULL)
	
	# CASE: more than 1 script number queried
	if(length(script.num) > 1) {
		warning("Please query only 1 script number.", call. = FALSE)
		return(NULL)
	}
	
	# CASE: more than 1 valType queried
	if(length(val.type) > 1) {
		warning("Please query only 1 valType.", call. = FALSE)
		return(NULL)
	}
	
	# Generate user's queries 
	# each query is a different possible combination of parameters
	query.types <- c()
	query.lines <- c()
	query.scripts <- c()
	
	if(length(start.line) == 1)
	{
		# queried only 1 start line (could be NA)
		query.lines <- rep(start.line, length(query.vars))
		query.types <- rep(val.type, length(query.vars))
		query.scripts <- rep(script.num, length(query.vars))
	}
	else if(length(query.vars) == 1)
	{
		# there's only 1 variable queried
		# there could be multiple start lines queried
		query.lines <- start.line
		query.vars <- rep(query.vars, length(query.lines))
		query.types <- rep(val.type, length(query.lines))
		query.scripts <- rep(script.num, length(query.lines))
	}
	else if(length(query.vars) == length(start.line))
	{
		# equal numbers of start lines and queried variables
		query.lines <- start.line
		query.types <- rep(val.type, length(query.vars))
		query.scripts <- rep(script.num, length(query.vars))
	}
	else
	{
		warning("Please query either:\n
				1 object (e.g. \"x\"),\n
				1 start line number, or\n
				an equal number of objects and start lines.",
				call. = FALSE)
		return(NULL)
	}
	
	# combine each column into a table
	query.table <- data.frame(query.vars, query.types, query.lines, query.scripts, stringsAsFactors = FALSE)
	names(query.table) <- c("name", "valType", "startLine", "scriptNum")
	
	# return unique rows (queries)
	return(unique(query.table))
}

#' Get valid queries.
#' Function shared with debug.lineage and debug.view
#'
#' @param pos.nodes Table of possible data nodes.
#'                  columns: d.id, p.id, name, valType, startLine, scriptNum
#' @param query The user's queries.
#'              columns: name, valType, startLine, scriptNum
#' @param forward For lineage queries. This determines which d.id is returned.
#'
#' @return The valid queries.
#'         columns: d.id, name, valType, startLine, scriptNum
#'
#' @noRd
.get.valid.query.var <- function(pos.nodes, query, forward = FALSE)
{
	# CASE: no queries
	if(is.null(query))
		return(invisible(NULL))
	
	# QUERY: subset by script number
	# Since there can only be 1 script number queried per call,
	# check for its validity first.
	script.num <- .to.int(query$scriptNum[1])
	
	if(is.null(script.num)) {
		warning("Script number must be a single integer.", call. = FALSE)
		return(invisible(NULL))
	}
	
	# case: script number could be NA
	if(is.na(script.num)) {
		pos.nodes <- pos.nodes[is.na(pos.nodes$scriptNum), ]
		pos.nodes <- .remove.na.rows(pos.nodes)
	}
	else {
		pos.nodes <- pos.nodes[pos.nodes$scriptNum == script.num, ]
		pos.nodes <- .remove.na.rows(pos.nodes)
	}
	
	# CASE: invalid script num
	if(nrow(pos.nodes) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n\n"))
		return(invisible(NULL))
	}
	
	# STEP: check validity of each query (row)
	query.indices <- c(1:nrow(query))
	
	# (for forward/backward lineage queries)
	# store id of valid data nodes when found
	# this is so that in cases where no start.line is searched for,
	# we will know which node id to return
	valid.d.id <- c()
	
	# this is akin to a loop where, for every query (row),
	# a TRUE or FALSE will be returned. TRUE corresponds to valid query
	# this is used later to extract from the table of queries
	valid.indices <- sapply(query.indices, function(i)
	{
		# extract individual components of the query
		query.var <- query$name[i]
		query.valType <- query$valType[i]
		query.line <- query$startLine[i]
		
		# QUERY: filter by node name
		subset <- pos.nodes[pos.nodes$name == query.var, ]
		subset <- .remove.na.rows(subset)
		
		# CASE: no row with queried node name found - return false
		if(nrow(subset) == 0)
			return(FALSE)
		
		# QUERY: filter by valType, if queried valType is not NA
		if(!is.na(query.valType))
		{
			# get the regex form for the valType query
			query.valType <- paste("*", query.valType, "*", sep="")
			
			# extract the cells where the queried valType can be found
			subset <- subset[grep(query.valType, subset$valType), ]
			subset <- .remove.na.rows(subset)
			
			# CASE: no nodes with queried valType found - return false
			if(nrow(subset) == 0)
				return(FALSE)
		}
		
		# (for lineage queries)
		# QUERY: start line queried is NA, 
		# find the id of the node to be used
		query.line.int <- .to.int(query.line)
		
		if(is.null(query.line.int))
			return(FALSE)
		
		query.line <- query.line.int
		
		if(is.na(query.line))
		{
			# extract data node id
			d.id <- subset$`d.id`
			
			# find the id of the node to be used
			# forward lineage - get first node
			# backwards lineage - get last node
			if(nrow(subset) == 1)
				valid.d.id <<- append(valid.d.id, d.id)
			else if(forward)
				valid.d.id <<- append(valid.d.id, d.id[1])
			else
				valid.d.id <<- append(valid.d.id, d.id[length(d.id)])
			
			# node is found - return true
			return(TRUE)
		}
		
		# QUERY: search for queried start line
		subset <- subset[subset$startLine == query.line, ]
		subset <- .remove.na.rows(subset)
		
		# CASE: start line not found
		if(nrow(subset) == 0)
			return(FALSE)
		
		# node found: record data node id
		valid.d.id <<- append(valid.d.id, subset$`d.id`)
		
		return(TRUE)
	})
	
	# STEP: extract valid queries
	valid.queries <- query[valid.indices, ]
	valid.queries <- .remove.na.rows(valid.queries)
	
	# CASE: no valid queries
	if(nrow(valid.queries) == 0) {
		cat("No valid queries.\n\n")
		return(invisible(NULL))
	}
	
	# STEP: bind valid data node id column to valid queries
	valid.queries <- cbind("d.id" = valid.d.id,
						   valid.queries,
						   stringsAsFactors = FALSE)
	return(valid.queries)
}

#' Get each instance of the queried data node/variable name, bound into a data frame.
#'
#' @param query A query. Must be valid.
#'              columns: name, valType, startLine, scriptNum
#'
#' @return A data frame of all instances of the queried data node.
#'         columns: value, container, dimension, type, scriptNum, startLine, code
#'
#' @noRd
.get.output.var <- function(query)
{
	pos.data <- .debug.env$data.nodes
	pos.proc <- .debug.env$proc.nodes
	
	# STEP: from query, extract applicable columns
	# name, valType
	query <- query[ , c("name", "valType")]
	
	# STEP: from all data nodes, 
	# get nodes with queried name
	# extract columns: id, value, valType
	data.nodes <- pos.data[pos.data$name == query$name, 
							 c("id", "value", "valType")]
	
	# STEP: extract nodes with queried valType, if not NA
	if(!is.na(query$valType)) {
		query.valType <- paste("*", query$valType, "*", sep="")
		data.nodes <- data.nodes[grep(query.valType, data.nodes$valType), ]
	}
	
	# STEP: for each data node, get columns for val type
	# and from corresponding procedure node
	rows <- lapply(c(1:nrow(data.nodes)), function(i)
	{
		# STEP: get row from data nodes
		# columns: id, value
		data.fields <- data.nodes[i, c("id", "value")]
		
		# STEP: get val type columns from provParseR
		# columns: container, dimension, type
		valType.fields <- provParseR::get.val.type(.debug.env$prov, data.fields$id)
		valType.fields <- valType.fields[ , c("container", "dimension", "type")]
		
		# STEP: get corresponding procedure node id (could have mulitple)
		p.id <- .get.p.id(data.fields$id)
		
		# STEP: get fields from proc nodes
		# columns: scriptNum, startLine, code
		# cbind with data.fields and valType.fields
		row <- lapply(p.id, function(id)
		{
			proc.fields <- pos.proc[pos.proc$id == id, c("scriptNum", "startLine", "name")]
			colnames(proc.fields) <- c("scriptNum", "startLine", "code")
			
			# cbind with data.fields and valType.fields
			# remove id (first) column
			fields <- cbind(data.fields, valType.fields, proc.fields, stringsAsFactors = FALSE)
			fields <- fields[ ,-1]
			
			return(fields)
		})
		
		# if there are multiple rows, combine into data frame
		if(length(row) == 1)
			row <- row[[1]]
		else
			row <- .form.df(row)
		
		return(row)
	})
	
	# STEP: bind rows into data frame, return
	return(.form.df(rows))
}
