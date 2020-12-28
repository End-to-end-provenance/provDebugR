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

#' Displaying variable values
#' 
#' debug.line displays the values of variables that are either used or set
#' on a particular line.
#' 
#' These functions are part of the provDebugR package.  To use them, you must
#' first initialise the debugger using
#' one its initialisation functions: \code{\link{prov.debug}}, 
#' \code{\link{prov.debug.file}}, or \code{\link{prov.debug.run}}.
#' 
#' For each line number queried, debug.line returns a data frame of the data 
#' that the procedure in that line inputs and outputs.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item name: The name of the data.
#'		\item value: The value of the data.
#'		\item container: The type of the container of the data, such as vector or data frame.
#'		\item dimension: The size of the container.
#'		\item type: The data type(s) contained within the container.
#' }
#'
#' @param ... The value being queried.  For debug.line and debug.state, these are line
#'            numbers.  For debug.variable, these are variable names.  For
#'            debug.view, these are variable names and/or file names. 
#' @param script.num The script numbers to be queried.
#'                   The main script is script 1.  If any scripts are included
#'                   using the `source` function, they will be assigned a unique
#'                   script number.
#'                   If script.num == "all", all possible script numbers will be queried.
#' @param all If TRUE, it is as if the ... parameter listed all possible values.
#'            For debug.line, it will display input and output information for
#'            all lines.  For debug.variable, it will display values for all
#'            variables.
#'
#' @return debug.line returns a list of data frames showing the inputs and outputs for the procedure
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
#' @seealso \code{\link{debug.type.changes}}: Returns a data frame for each variable in
#'              the execution containing the instances where the data type changed.
#' @seealso \code{\link{debug.warning}}: Returns the backwards lineage of the queried
#'              warning(s), if any.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R")
#' debug.line(5)
#' debug.line(all = TRUE)
#' debug.line(5, 10, script.num = 2)
#' debug.line(3, script.num = "all")
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
	# columns: p.id, startLine, scriptNum, scriptName, code
	pos.nodes <- .get.pos.line(.debug.env$proc.nodes)
	
	if(is.null(pos.nodes)) {
		cat("There are no lines.\n")
		return(invisible(NULL))
	}
	
	# STEP: get user's query
	# columns: startLine, scriptNum
	query <- .get.query.line(..., script.num = script.num, all = all)
	
	# STEP: get valid queries
	# columns: p.id, startLine, scriptNum, scriptName, code
	valid.queries <- .get.valid.query.line(pos.nodes, query)
	# invalid.queries <- 
	
	# CASE: no valid queries
	if(is.null(valid.queries)) {
		cat("There are no valid queries.\n\n")
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
		p.id <- valid.queries$p.id[i] 
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
		
		# TODO clean this up!!!!
		
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
	
	names(output) <- c(1:nrow(valid.queries))
	
	# print results
	.print.line(output, valid.queries)
	
	return(invisible(output))
}

#' Get all possible procedure nodes. 
#' For each possible procedure node, find its corresponding script name.
#' columns: p.id, startLine, scriptNum, scriptName, code
#'
#' @param proc.nodes A table of all procedure nodes.
#'
#' @return The table of all procedure nodes, with their script names recorded.
#'         columns: p.id, startLine, scriptNum, scriptName, code
#'
#' @noRd
.get.pos.line <- function(proc.nodes)
{
	# CASE: no procedure nodes
	if(nrow(proc.nodes) == 0)
		return(NULL)
	
	# get Operation nodes
	pos.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
	pos.nodes <- .remove.na.rows(pos.nodes)
	
	# CASE: no operation nodes
	if(nrow(pos.nodes) == 0)
		return(NULL)
	
	# get p.id, startLine, scriptNum, scriptName, code
	pos.nodes <- pos.nodes[ , c("id", "startLine", "scriptNum", "scriptName", "name")]
	names(pos.nodes) <- c("p.id", "startLine", "scriptNum", "scriptName", "code")
	
	# rename rows, return
	rownames(pos.nodes) <- c(1:nrow(pos.nodes))
	return(pos.nodes)
}

#' Get the user's queries, bound into a data frame.
#' columns: startLine, scriptNum
#'
#' @param ... The user's line number queries
#' @param script.num The script number queries. Can be "all".
#' @param all If TRUE, this ignores other parameters and returns the table of
#'            all possible line and script number combinations.
#'
#' @return A data frame of the user's queries.
#'         columns: startLine, scriptNum
#'
#' @noRd
.get.query.line <- function(..., script.num = 1, all = FALSE)
{
	# Case: all == TRUE
	# get all possible startline and script number combinations from proc nodes table
	if(all)
	{
		proc.nodes <- .debug.env$proc.nodes
		proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
		
		queries <- proc.nodes[ , c("startLine", "scriptNum")]
		return(unique(queries))
	}
	
	# all == FALSE
	# flatten user's queries
	query.lines <- .flatten.args(...)
	
	# case: no queried lines
	if(is.null(query.lines))
		return(NULL)
	
	# Case: script.num == "all"
	if(tolower(script.num[1]) == "all")
		script.num <- c(1:nrow(provParseR::get.scripts(.debug.env$prov)))
	
	# get all combinations of start lines and script numbers
	startLine <- rep(query.lines, length(script.num))
	scriptNum <- rep(script.num, each = length(query.lines))
	
	# combine into data frame, return
	queries <- data.frame(startLine, scriptNum, stringsAsFactors = FALSE)
	return(unique(queries))
}

#' Returns a data frame of valid queries. .
#' columns: p.id, startLine, scriptNum, scriptName, code
#'
#' @param pos.nodes Table of all possible options.
#' @param query A list of the user's queries.
#'
#' @return A data frame of valid line number queries
#'         columns: p.id, startLine, scriptNum, scriptName, code
#'
#' @noRd
.get.valid.query.line <- function(pos.nodes, query)
{
	# CASE: no queries
	if(is.null(query))
		return(NULL)
	
	# Check validity of each query
	# For every query (row), an index identifying the corresponding
	# row in pos.nodes will be returned if found (valid query).
	# NULL will be returned if the query is not valid.
	valid.indices <- sapply(c(1:nrow(query)), function(i)
	{
		# extract startLine and scriptNum
		query.line <- .to.int(query$startLine[i])
		query.script <- .to.int(query$scriptNum[i])
		
		# CASE: line or script number is not an int
		if(is.null(query.line) || is.null(query.script))
			return(NULL)
		
		# QUERY: filter by line and script num
		indices <- c(1:nrow(pos.nodes))[pos.nodes$startLine == query.line &
										pos.nodes$scriptNum == query.script]
		
		# CASE: index not found
		if(length(indices) == 0)
			return(NULL)
		
		# return indices found (could be multiple)
		return(indices)
	})
	
	# unlist valid.indices into a single vector
	valid.indices <- unique(unlist(valid.indices))
	
	# CASE: no valid indices
	if(is.null(valid.indices))
		return(NULL)
	
	# extract valid rows from pos.nodes table
	return(.remove.na.rows(pos.nodes[valid.indices, ]))
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

#' Prints all inputs and outputs for the line queried.
#'
#' @param output list of all inputs and outputs for each line.
#' @param valid.queries list of all queries that corresponded to lines of code.
#'         
#' @noRd
.print.line <- function(output, valid.queries) {
  # print script numbers, if multiple scripts
  num.scripts <- .print.script.nums()
  
  # loop through and print all valid queries before any details
  cat('Results for line(s): ')
  lapply(c(1:nrow(valid.queries)), function(i) {
    if (i == nrow(valid.queries)) {
      # on last loop, no comma
      cat(paste(valid.queries$startLine[i], "\n"))
    } 
    else {
      cat(paste(valid.queries$startLine[i], ", ", sep=""))
    }
  })
  
  
  # print details for each query
  lapply(c(1:nrow(valid.queries)), function(i) {
    # line information
    # if only one script, print just line number
    if (num.scripts == 1) {
      cat(paste("\n", valid.queries$startLine[i], ": ", sep=""))
    }
    else {
      cat(paste("\n", valid.queries$scriptNum[i], ", ",
                valid.queries$startLine[i], ": ", sep=""))
    }

    # split code based on \n
    tempCode <- strsplit(valid.queries$code[i], "\n")

    # print line of code up to first \n, shortening if over 50 chars
    if (nchar(tempCode[[1]][1]) > 75)
      cat(paste(substring(tempCode[[1]][1], 1, 75), "...\n"))
    else
      cat(paste(tempCode[[1]][1], "\n", sep = ""))
    
    
    # inputs
    cat(paste("\t", "Inputs:", "\n"))
    if ((length(output[[i]]$input) == 1) && is.na(output[[i]]$input)) {
      cat("\t\tNone\n")
    }
    else {
      lapply(c(1:nrow(output[[i]]$input)), function(j) {
        # print each output line by line
        cat(paste("\t\t", j, ". ", output[[i]]$input$name[j], "   ", 
                  output[[i]]$input$value[j], "\n", sep=""))
      })
    }
    
    # outputs
    cat(paste("\t", "Outputs:", "\n"))
    if ((length(output[[i]]$output) == 1) &&is.na(output[[i]]$output)) {
      cat("\t\tNone\n")
    }
    else {
      lapply(c(1:nrow(output[[i]]$output)), function(j) {
        # print each output line by line
        cat(paste("\t\t", j, ". ", output[[i]]$output$name[j], "   ", 
                  output[[i]]$output$value[j], "\n", sep=""))
      })
    }
  })
  
  
}
