# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2020, 2021.

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

#' debug.variable
#' 
#' debug.variable shows all values that a particular variable has during 
#' execution of a script.
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
#'  	\item scriptName: The name of the script the variable or file is associated with.
#'		\item startLine: The line number the variable is associated with.
#'		\item code: The code this variable is associated with.
#' }
#'
#' @param ... The variable names to be queried.
#' @param val.type If not "all", this filters the results to contain
#'                 only instances where the valType (container or type) has the
#'                 queried type. Only one type may be queried per function call.
#' @param script.num The script number of the queried variables. Defaults to "all".
#' @param all If TRUE, results for all variables of the specified script will be
#'            returned.
#' @param showType If TRUE, variable container, dimension, and type are displayed.
#' 
#'
#' @return debug.variable returns a list of data frames showing all instances of each variable queried.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R")
#' debug.variable(x)
#' debug.variable(all = TRUE)
#' debug.variable("a", b, "x", val.type = "logical")
#' debug.variable("a", "b", x, script.num = 3)
#' }
#'
#' @export
#' @rdname debug.line
debug.variable <- function(..., val.type = "all", script.num = "all", 
                           all = FALSE, showType = FALSE)
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
		cat("No valid queries.\n\n")
		.print.pos.options(pos.vars[ , c("name", "startLine", "scriptNum", "scriptName")])
		return(invisible(NULL))
	}
	
	# STEP: extract name and valType columns
	valid.queries <- valid.queries[ , c("name", "valType")]
	valid.queries <- unique(valid.queries)
	
	# STEP: for each valid query, form table for user output
	output <- lapply(c(1:nrow(valid.queries)), function(i) {
		return(.get.output.var(valid.queries[i, ]))
	})
	
	names(output) <- valid.queries$name
	
	.print.variable(output, showType)
	
	return(invisible(output))
}

#' For each possible data node find its corresponding procedure node.
#' Function shared with debug.lineage
#' columns: d.id, p.id, name, valType, startLine, scriptNum, scriptName
#'
#' @param data.nodes A table of all possible data nodes
#'
#' @return The table of all possible data nodes, with the necessary fields found.
#'         columns: d.id, p.id, name, value, valType, startLine, scriptNum, scriptName
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
		
		# get id, startLine, scriptNum, scriptName from proc nodes table
		# cbind with row from data nodes table
		row <- lapply(p.id, function(id) {
			p.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes$id == id,
											  c("id", "startLine", "scriptNum", "scriptName")]
			scriptName <- .debug.env$scripts[p.fields$scriptNum]
			
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
	colnames(rows) <- c("d.id", "name", "value", "valType", "p.id", "startLine", "scriptNum", "scriptName")
	rows <- rows[ , c("d.id", "p.id", "name", "value", "valType", "startLine", "scriptNum", "scriptName")]
	
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
.get.query.var <- function(query.vars, val.type = "all", start.line = "all", script.num = "all")
{
	# CASE: no queried variables
	if(is.null(query.vars))
		return(NULL)
	
	# script.num == "all"
	if(tolower(script.num[1]) == "all")
		script.num <- c(1:nrow(provParseR::get.scripts(.debug.env$prov)))
	
	# get all queries for each queried node & combine
	queries <- lapply(query.vars, function(var)
	{	
		q.lines <- start.line
		
		# start.line = "all"
		# get all start lines for the node queried. leave NA if none found
		if(!is.na(q.lines[1]) && tolower(q.lines[1]) == "all") 
		{
			# get data node ids
			d.id <- .debug.env$data.nodes$id[.debug.env$data.nodes$name == var]
			
			if(length(d.id) == 0) {
				q.lines <- NA
			}
			else {
				# get corresponding proc node ids & start lines
				q.lines <- lapply(d.id, function(id) {
					p.id <- .get.p.id(id)
					
					lines <- sapply(p.id, function(id) {
						return(.debug.env$proc.nodes$startLine[.debug.env$proc.nodes$id == id])
					})
					
					return(lines)
				})
				
				q.lines <- unique(unlist(q.lines))
			}
		}
		
		# match start lines to script numbers
		query.lines <- rep(q.lines, length(script.num))
		query.scripts <- rep(script.num, each = length(q.lines))
		
		# Match valType query to script numbers and start lines.
		length.scripts <- length(query.scripts)
		length.types <- length(val.type)
	
		query.lines <- rep(query.lines, each = length.types)
		query.scripts <- rep(query.scripts, each = length.types)
		query.types <- rep(val.type, length.scripts)
		
		# replicate var query to match length of other columns
		vars <- rep(var, length(query.lines))
		
		# combine each column into a table
		query.table <- data.frame(vars, query.types, query.lines, query.scripts, stringsAsFactors = FALSE)
		names(query.table) <- c("name", "valType", "startLine", "scriptNum")
		
		return(query.table)
	})
	
	queries <- .form.df(queries)
	
	# return unique rows (queries)
	return(unique(queries))
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
#'         columns: d.id, name, valType, startLine, scriptNum, scriptName
#'
#' @noRd
.get.valid.query.var <- function(pos.nodes, query, forward = FALSE)
{
	# CASE: no queries
	if(is.null(query))
		return(NULL)
	
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
		query.line <- .to.int(query$startLine[i])
		query.script <- .to.int(query$scriptNum[i])
		
		# CASE: line or script number is not an int
		if(is.null(query.line) || is.null(query.script))
			return(FALSE)
		
		# QUERY: filter by node name and script num
		subset <- pos.nodes[pos.nodes$name == query.var &
							pos.nodes$scriptNum == query.script, ]
		subset <- .remove.na.rows(subset)
		
		# CASE: no row with queried node name found - return false
		if(nrow(subset) == 0)
			return(FALSE)
		
		# QUERY: filter by valType, if queried valType is not "all"
		if(tolower(query.valType) != "all")
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
		# QUERY: start line queried is "all" or NA, 
		# find the id of the node to be used
		query.line.int <- .to.int(query.line)
		
		if(is.null(query.line.int))
			return(FALSE)
		
		query.line <- query.line.int
		
		if(is.na(query.line.int))
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
	if(nrow(valid.queries) == 0)
		return(NULL)
	
	# STEP: bind valid data node id column to valid queries
	valid.queries <- cbind("d.id" = valid.d.id,
						   valid.queries,
						   stringsAsFactors = FALSE)
	return(valid.queries)
}

#' Get each instance of the queried data node/variable name, bound into a data frame.
#'
#' @param query A query. Must be valid.
#'              columns: name, valType
#'
#' @return A data frame of all instances of the queried data node.
#'         columns: value, container, dimension, type, scriptNum, scriptName, startLine, code
#'
#' @noRd
.get.output.var <- function(query)
{	
	pos.data <- .debug.env$data.nodes
	pos.proc <- .debug.env$proc.nodes
	
	# STEP: from all data nodes, 
	# get nodes with queried name
	# extract columns: id, value, valType
	data.nodes <- pos.data[pos.data$name == query$name, 
							 c("id", "value", "valType")]
	
	# STEP: extract nodes with queried valType, if not "all"
	if(tolower(query$valType) != "all") {
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
		# columns: scriptNum, scriptName, startLine, code
		# cbind with data.fields and valType.fields
		row <- lapply(p.id, function(id)
		{
			# get and rename proc.fields
			proc.fields <- pos.proc[pos.proc$id == id, c("scriptNum", "scriptName",
														 "startLine", "name")]
			colnames(proc.fields) <- c("scriptNum","scriptName","startLine","code")
			
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


#' Prints the lineage of each variable queried.
#'
#' @param output list of variable lineage for each valid variable queried.
#' @param showType if TRUE, container, dimension, and type information for each
#'                 variable is shown.
#'         
#' @noRd
.print.variable <- function(output, showType) {
  # print script numbers, if multiple scripts
  num.scripts <- .print.script.nums()
  
  # print details for each query
  lapply(c(1:length(output)), function(i) {
    
    # print variable name
    cat(paste("Var:", names(output[i]), "\n"))
    
    # print lineage
    lapply(c(1:nrow(output[[i]])), function(j) {
      # if only one script, print just line number
      if (num.scripts == 1) {
        cat(paste("\t", output[[i]]$startLine[j], ": ", sep=""))
      }
      else {
        cat(paste("\t", output[[i]]$scriptNum[j], ", ",
                  output[[i]]$startLine[j], ": ", sep=""))
      }
      
      # split code based on \n
      tempCode <- strsplit(output[[i]]$code[j], "\n")
      
      # print line of code, shortening if over 50 chars
      if (nchar(tempCode[[1]][1]) > 50)
        cat(paste("\t", output[[i]]$value[j], "\t", 
                  substring(tempCode[[1]][1], 1, 47), "...\n"))
      else
        cat(paste("\t", output[[i]]$value[j], "\t", tempCode[[1]][1], "\n"))
      
      # print valType info, if desired
      if (showType == TRUE) {
        print(output[[i]][j, c(2:4)], right = FALSE)
      }
    })
  })
  if (showType == FALSE)
    cat("\nRerun with showType = TRUE to see more detailed variable information.\n")
}
