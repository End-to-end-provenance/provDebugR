# === LINE =================================================================== #

#' @export
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

#' returned columns: id, startLine, scriptNum, code
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

# === VARIABLE =============================================================== #

#' @export
debug.variable <- function(..., val.type = NA, script.num = 1, all = FALSE)
{
	# CASE: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: get all possible variables
	# data nodes must have type = "Data" or "Snapshot" to be considered a variable
	data.nodes <- .debug.env$data.nodes
	data.nodes <- data.nodes[data.nodes$type == "Data" | data.nodes$type == "Snapshot", ]
	
	# columns: d.id, p.id, name, valType, startLine, scriptNum
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

# function shared with debug.lineage
# columns: d.id, p.id, name, valType, startLine, scriptNum
#'
#' @noRd
.get.pos.var <- function(data.nodes)
{
	# CASE: no data nodes/variables
	if(nrow(data.nodes) == 0)
		return(NULL)
	
	# from data nodes, keep columns: id, name, valType
	# rename id column to d.id
	data.nodes <- data.nodes[ , c("id", "name", "valType")]
	colnames(data.nodes) <- c("d.id", "name", "valType")
	
	# for each data node, get the corresponding procedure node
	# some may have multiple proc nodes associated with this (files, url, fromEnv nodes etc.)
	rows <- lapply(c(1:nrow(data.nodes)), function(i)
	{
		# get id and row from data nodes table
		d.fields <- data.nodes[i, ]
		d.id <- d.fields$`d.id`
		
		# try to get the procedure node that assigned/produced the data node
		p.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == d.id]
		
		# alternatively, get the procedure node that first used the data (e.g. url)
		if(length(p.id) == 0)
			p.id <- .debug.env$data.proc$activity[.debug.env$data.proc$entity == d.id]
		
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
	colnames(rows) <- c("d.id", "name", "valType", "p.id", "startLine", "scriptNum")
	rows <- rows[ , c("d.id", "p.id", "name", "valType", "startLine", "scriptNum")]
	
	return(rows)
}

#' function shared with debug.lineage
#' columns: name, valType, startLine, scriptNum
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

#' pos.nodes columns: d.id, p.id, name, valType, startLine, scriptNum
#' query columns: name, valType, startLine, scriptNum
#'
#' returned columns: d.id, name, valType, startLine, scriptNum
#'
#' function shared with debug.lineage
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

#' returned columns: value, container, dimension, type, scriptNum, startLine, code
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
		
		# STEP: get corresponding procedure node id
		p.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == data.fields$id]
		
		if(length(p.id) == 0)
			p.id <- .debug.env$data.proc$activity[.debug.env$data.proc$entity == data.fields$id]
		
		# STEP: get fields from proc nodes
		# columns: scriptNum, startLine, code
		proc.fields <- pos.proc[pos.proc$id == p.id, c("scriptNum", "startLine", "name")]
		colnames(proc.fields) <- c("scriptNum", "startLine", "code")
		
		# STEP: cbind columns
		# remove id (first) column
		fields <- cbind(data.fields, valType.fields, proc.fields, stringsAsFactors = FALSE)
		fields <- fields[ ,-1]
		
		return(fields)
	})
	
	# STEP: bind rows into data frame, return
	return(.form.df(rows))
}

# === LINEAGE ================================================================ #

#' @export
debug.lineage <- function(..., start.line = NA, script.num = 1, all = FALSE, forward = FALSE)
{
	# CASE: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: get all possible nodes
	# columns: d.id, p.id, name, valType, startLine, scriptNum
	pos.nodes <- .get.pos.var(.debug.env$data.nodes)
	
	# CASE: no data nodes
	if(is.null(pos.nodes)) {
		cat("There are no data nodes.\n")
		return(invisible(NULL))
	}
	
	# STEP: get user's query
	# columns: name, valType (NA), startLine, scriptNum
	if(all)
		query.nodes <- unique(pos.nodes$name)
	else
		query.nodes <- .flatten.args(...)
	
	query <- .get.query.var(query.nodes, val.type = NA, 
							start.line = start.line, script.num = script.num)
	
	# STEP: get valid queries
	valid.queries <- .get.valid.query.var(pos.nodes, query, forward = forward)
	
	# CASE: no valid queries
	# columns: d.id, name, valType, startLine, scriptNum
	if(is.null(valid.queries)) {
		cat("No valid queries.\n\n")
		.print.pos.options(pos.nodes[ , c("name", "startLine", "scriptNum")])
		return(invisible(NULL))
	}
	
	# STEP: for each valid query, get lineage and form table for user output
	# as some nodes may not have a lineage, keep a vector to keep track 
	# of which indices of the valid queries table do
	indices <- c()
	
	lineages <- lapply(c(1:nrow(valid.queries)), function(i)
	{
		# get id and name
		d.id <- valid.queries$`d.id`[i]
		d.name <- valid.queries$name[i]
		
		# get lineage
		lineage <- .get.lineage(d.id, forward = forward)
		
		# case: no lineage
		if(is.null(lineage)) {
			cat(paste("No lineage for ", d.name, ".\n", sep=""))
			return(invisible(NULL))
		}
		
		# keep track of index and form output
		indices <<- append(indices, i)
		return(.get.output.lineage(lineage))
	})
	
	# Case: no lineages to display
	if(length(indices) == 0)
		return(invisible(NULL))
	
	# extract lineages that are not null
	if(length(indices) < nrow(valid.queries)) {
		lineages <- lineages[indices]
		valid.queries <- valid.queries[indices, ]
	}
	
	names(lineages) <- valid.queries$name
	return(lineages)
}

#' @noRd
.get.lineage <- function(node.id, forward = FALSE)
{	
	# get lineage, extract proc nodes
	lineage <- provGraphR::get.lineage(.debug.env$graph, node.id, forward = forward)
	lineage <- lineage[grep('^p[[:digit:]]+', lineage)]
	
	# if getting the forward lineage, get the proc node which first assigned the variable
	if(forward)
	{
		edge <- .debug.env$proc.data[.debug.env$proc.data$entity ==  node.id, ]
		
		if(nrow(edge) > 0)
			lineage <- append(lineage, edge$activity[[1]], 0)
	}
	
	# case: no lineage
	if(length(lineage) == 0)
		return(NULL)
	
	# order by increasing proc node number
	node.nums <- as.integer(sub("^[[:alpha:]]", "", lineage))
	lineage <- lineage[order(node.nums)]
	
	return(lineage)
}

#' @noRd
.get.output.lineage <- function(id.list)
{
	proc.nodes <- .debug.env$proc.nodes
	
	# get output for user
	rows <- lapply(id.list, function(id)
	{
		fields <- proc.nodes[proc.nodes$id == id, c("scriptNum", "startLine", "name")]
		names(fields) <- c("scriptNum", "startLine", "code")
		return(fields)
	})
	
	# form data frame
	df <- .form.df(rows)
	
	# remove any rows where scriptNum is NA 
	# (this occurs for plots if dev.off is not called)
	df <- df[!is.na(df$scriptNum), ]
	return(df)
}

# === TYPE CHANGES =========================================================== #

#' @export
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
		
		# get proc node which either set or used the data node
		proc.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == data.id]
		
		if(length(proc.id) == 0)
			proc.id <- .debug.env$data.proc$activity[.debug.env$data.proc$entity == data.id]
		
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

# === STATE ================================================================== #

#' assumes that the line had been executed
#' script num ignored if no line 
#' @export
debug.state <- function(..., script.num = 1)
{
	# CASE: No provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# STEP: get all possible queriable procedure nodes (Operation nodes)
	# keep columns: id, scriptNum, startLine, name (code)
	pos.proc.all <- .debug.env$proc.nodes[.debug.env$proc.nodes$type == "Operation", ]
	pos.proc.all <- pos.proc.all[ , c("id", "scriptNum", "startLine", "name")]
	names(pos.proc.all) <- c("id", "scriptNum", "startLine", "code")
	
	# STEP: remove non-variables from:
	# proc.nodes, data.nodes, proc.data, data.proc
	# As the user may query lines that do not have code or variables attached,
	# this is to ease the search of the closest line prior to the query
	# where the state changed.
	pos.tables <- .get.state.tables()
	
	# CASE: no data nodes/variables
	if(is.null(pos.tables)) {
		cat("There is no state.\n\n")
		return(invisible(NULL))
	}
	
	# STEP: assign tables from pos.tables to their own variables for easy access.
	pos.proc <- pos.tables$proc.nodes
	pos.data <- pos.tables$data.nodes
	pos.proc.data <- pos.tables$proc.data
	pos.data.proc <- pos.tables$data.proc
	
	# STEP: check validity of script.num
	# bind query into table form, extract code column, if any.
	query <- .get.valid.query.state(pos.proc.all, ..., script.num = script.num)
	
	# if query is null, that means there are no valid queries or the query itself is empty.
	# in this case, we want to show the state at the end of execution.
	# we want to keep a variable to track that the end of execution is automatically shown.
	end.of.execution <- FALSE
	
	if(is.null(query)) {
		end.of.execution = TRUE
		cat("State at the end of execution:\n")
		query <- pos.proc.all[nrow(pos.proc.all), c("startLine", "scriptNum")]
	}
	
	# STEP: Get state for each query
	# Keep a growing table of queries with state. This is for user output purposes.
	# Also keep a vector to keep track of queries with no state
	queries <- data.frame(id = character(),
						  scriptNum = integer(),
						  startLine = integer(),
						  code = character(),
						  notes = character(),
						  stringsAsFactors = FALSE)
	no.state <- c()
	
	states <- lapply(c(1:nrow(query)), function(i)
	{
		# Get closest procedure above queried line where state changed.
		proc <- .get.closest.proc(pos.proc, query[i, ])
		p.id <- proc$id
		
		queries <<- rbind(queries, proc, stringsAsFactors = FALSE)
		
		# get the dnum of last data node attached
		d.id <- .get.last.var(p.id, pos.data, pos.proc.data, pos.data.proc)
		
		# get list of data nodes that form the state
		d.list <- .get.state(d.id, pos.data)
		
		# CASE: no state
		if(is.null(d.list)) {
			no.state <<- append(no.state, i)
			
			cat("There is no state for line ", proc$startLine, "in script ", 
				proc$scriptNum, ".", sep='')
			return(invisible(NULL))
		}
		
		# get output
		return(.get.output.state(d.list, p.id, pos.proc.data, pos.data.proc))
	})
	
	# Remove, if any, for those with no state
	if(length(no.state) > 0) {
		queries <- queries[-no.state, ]
		states <- states[-no.state]
	}
	
	# CASE: no state to display at all
	if(length(states) == 0)
		return(invisible(NULL))
	
	# user output: print table of queries with state if
	# not directly showing the end of execution (when there are no valid queries)
	if(! end.of.execution) 
	{
		row.names(queries) <- c(1:nrow(queries))
		
		cat("Results for:\n")
		print(queries[ , -1])
		cat('\n')
	}
	
	# label output with indices of queries, return
	names(states) <- row.names(queries)
	return(states)
}

#' returned: list of tables: proc.nodes, data.nodes, proc.data, data.proc
#'
#' @noRd
.get.state.tables <- function()
{
	# get list of non-variables
	data.nodes <- .debug.env$data.nodes
	non.vars <- data.nodes$id[!(data.nodes$type == "Data" | data.nodes$type == "Snapshot")]
	
	# case: no variables
	if(nrow(data.nodes) == 0 || length(non.vars) == nrow(data.nodes))
		return(NULL)
	
	# get table of variables (data nodes)
	# keep columns: id, name, value, fromEnv
	data.nodes <- data.nodes[data.nodes$type == "Data" | data.nodes$type == "Snapshot", ]
	data.nodes <- data.nodes[c("id", "name", "value", "fromEnv"), ]
	
	# get all queriable operation nodes
	proc.nodes <- .debug.env$proc.nodes
	proc.nodes <- proc.nodes[proc.nodes$type == "Operation", ]
	proc.nodes <- proc.nodes[ , c("id", "scriptNum", "startLine", "name")]
	names(proc.nodes) <- c("id", "scriptNum", "startLine", "code")
	
	# get edges
	proc.data <- .debug.env$proc.data
	data.proc <- .debug.env$data.proc
	
	# case: no non-variables
	if(length(non.vars) == 0)
		return(list(proc.nodes, data.nodes, proc.data, data.proc))
	
	# for each procedure (operation) node, remove edges to and from non-variables.
	# remove the procedure node if all references, if any, are to non-variables
	# these vectors are to keep track of which indices from which table to remove.
	remove.proc <- c()
	remove.proc.data <- c()
	remove.data.proc <- c()
	
	lapply(c(1:nrow(proc.nodes)), function(i)
	{
		# get proc node id
		p.id <- proc.nodes$id[i]
		
		# check for output edges to non-variables
		pd <- proc.data[proc.data$activity == p.id, ]
		
		# keep track of indices to be removed, if any
		remove.pd.indices <- integer()   # for proc-data edges
		remove.dp.indices <- integer()   # for data-proc edges
		
		if(nrow(pd) > 0)
		{
			# for each edge, check if the connected data node is a non-variable
			remove.pd <- sapply(1:nrow(pd), function(j) {
				return(pd$entity[j] %in% non.vars)
			})
			
			# get the indices of edges to be removed, if any
			remove.pd.indices <- as.integer(rownames(pd)[remove.pd])
			
			if(length(remove.pd.indices) > 0)
				remove.proc.data <<- append(remove.proc.data, remove.pd.indices)
		}
		
		# check for input edges from non-variables
		dp <- data.proc[data.proc$activity == p.id, ]
		
		if(nrow(dp) > 0)
		{
			# for each edge, check if the connected data node is a non-variable
			remove.dp <- sapply(1:nrow(dp), function(j) {
				return(dp$entity[j] %in% non.vars)
			})
			
			# get the indices of edges to be removed, if any
			remove.dp.indices <- as.integer(rownames(dp)[remove.dp])
			
			if(length(remove.dp.indices) > 0)
				remove.data.proc <<- append(remove.data.proc, remove.dp.indices)
		}
		
		# if procedure node has no output or input edges, 
		# or all output and input edges are to non-variables,
		# record index of procedure node as it is to be removed.
		if((nrow(pd) == 0 && nrow(dp) == 0) || 
		   (length(remove.pd.indices) == nrow(pd) && length(remove.dp.indices) == nrow(dp))) {
			remove.proc <<- append(remove.proc, i)
		}
	})
	
	# extract out nodes and edges to be removed, if any
	if(length(remove.proc) > 0)
		proc.nodes <- proc.nodes[-remove.proc, ]
	if(length(remove.proc.data) > 0)
		proc.data <- proc.data[-remove.proc.data, ]
	if(length(remove.data.proc) > 0)
		data.proc <- data.proc[-remove.data.proc, ]
	
	# bind into list and return
	return(list(proc.nodes, data.nodes, proc.data, data.proc))
}

#' returned columns: startLine, scriptNum
#'
#' @noRd
.get.valid.query.state <- function(pos.nodes, ..., script.num = 1)
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
	pos.proc <- proc.nodes[proc.nodes$scriptNum == script.num, ]
	
	if(nrow(pos.proc) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n"))
		return(invisible(NULL))
	}
	
	# For each query, ensure that it is an integer
	query <- lapply(query, function(line) 
	{
		line.int <- .to.int(line)
		
		if(is.null(line) || is.na(line))
			return(NULL)
		
		return(line)
	})
	
	query <- as.vector(unique(.remove.null(query)))
	
	if(is.null(query)) {
		cat("No valid queries.\n\n", call. = FALSE)
		return(invisible(NULL))
	}
	
	# Bind valid queries to script num
	queries <- data.frame(startLine = query,
						  scriptNum = rep(script.num, length(query)),
						  stringsAsFactors = FALSE)
	return(queries)
}

#' get closest procedure node id with line number <= queried line number
#' returns p0 for pre-existing data nodes (where fromEnv == TRUE)
#' @noRd
.get.closest.proc <- function(proc.nodes, query)
{
	line <- query$startLine
	script.num <- query$scriptNum
	
	# get proc nodes for queried script
	script.proc <- proc.nodes[proc.nodes$scriptNum == script.num, ]
	
	# Try to get the procedure node referenced by queried line
	proc <- script.proc[script.proc$startLine == line, ]
	
	# Case: line number can be found
	# Since there could be multiple proc nodes on one line,
	# return the proc node with the highest proc node id.
	# columns: id, scriptNum, startLine, code, nodes
	if(nrow(proc) > 0) {
		proc <- proc[nrow(proc), c("id", "scriptNum", "startLine", "code")]
		proc <- cbind(proc, notes = "", stringsAsFactors = FALSE)
		return(proc)
	}
	
	# Case: line number can not be found
	# Need to find where the queried line number fits 
	# into the list of possible line numbers
	pos.lines <- script.proc$startLine
	
	# Case: line number < line number of the first proc node in script
	# Find where the first procedure node of script fall in the table of
	# all possible procedure nodes. Then return the one above it, 
	# or "p0" if reached the top of the table
	if(line < pos.lines[1])
	{
		index <- c(1:nrow(proc.nodes))[proc.nodes$id == script.proc$id[1]]
		index <- index - 1
		
		# at beginning of execution
		if(index == 0) {
			id <- "p0"
			notes <- "At the beginning of execution."
		}
		else {
			id <- proc.nodes$id[index]
			notes <- paste("At the beginning of script ", script.num, ".", sep='')
		}
	}
	# Case: line number > line number of last proc node in script
	else if(line > pos.lines[length(pos.lines)])
	{
		id <- script.proc$id[nrow(script.proc)]
		
		# note that the user is given state at end of execution if script.num is 1
		# otherwise, note that the state at the end of the script is given instead
		if(script.num == 1)
			notes <- "At the end of execution."
		else
			notes <- paste("At the end of script ", script.num, ".", sep='')
	}
	# Case: line number is in between possible lines
	else
	{
		# get the line index where the queried line is after in sequence
		index <- .find.num.loc(pos.lines, line)
		
		id <- script.proc$id[index]
		notes <- ""
	}
	
	# return
	return(data.frame(id = id, 
					  scriptNum = script.num, 
					  startLine = line,
					  code = "",
					  notes = notes,
					  stringsAsFactors = FALSE))
}

#' @noRd
.get.last.var <- function(p.id, data.nodes, proc.data, data.proc)
{
	# Case: beginning of execution
	if(p.id == "p0")
		return("d0")
	
	# Try to get last output node associated with given p.id
	# d.id for output nodes > d.id for input nodes
	d.id <- proc.data$entity[proc.data$activity == p.id]
	
	if(length(d.id) > 0)
		return(d.id[length(d.id)])
	
	# Otherwise, get last input node associated with given p.id
	# At least a data node is guarenteed to be found.
	d.id <- data.proc$entity[data.proc$activity == p.id]
	return(d.id[length(d.id)])
}

#' get all data nodes up to specified num
#' for each unique variable name, get the last occurrance of it
#' this forms the state
#'
#' @noRd
.get.state <- function(d.id, data.nodes)
{
	# First, get the id and name of data nodes where fromEnv is TRUE
	vars <- data.nodes[data.nodes$fromEnv, c("id", "name")]
	
	# Then, get the data nodes that are generated during the execution of the script.
	# "d0" is for the case where just the nodes where fromEnv is TRUE should be returned.
	# This list is appended to the list of fromEnv nodes before obtaining
	# the last occurence of each unique variable
	# fromEnv nodes will always have unique variable names
	if(d.id != "d0")
	{
		# get data nodes up to specified data node, then append to list of
		# fromEnv nodes, if any.
		max.index <- c(1:nrow(data.nodes))[data.nodes$id == d.id]
		vars <- rbind(vars, data.nodes[1:max.index, c("id", "name")], stringsAsFactors = FALSE)
		
		# get unique variable names.
		# for each unique variable name, get the last data node id,
		# if there are repeated occurences of it.
		unique.names <- unique(vars$name)
		
		if(length(unique.names) < nrow(vars))
		{
			return(sapply(unique.names, function(name)
			{
				id.list <- vars$id[vars$name == name]
				return(id.list[length(id.list)])
			}))
		}
	}
	
	# If there are no data nodes, there is no state
	if(nrow(vars) == 0)
		return(NULL)
	
	return(vars$id)
}

#' @noRd
.get.output.state <- function(id.list, p.id, proc.data, data.proc)
{
	# output consists of: var name, value, valType, scriptNum, startLine
	output <- lapply(id.list, function(d.id)
	{
		d.node <- .debug.env$data.nodes[.debug.env$data.nodes$id == d.id, ]
		
		# get fields from data node table
		# columns: name, value
		d.fields <- d.node[ , c("name", "value")]
		
		# get valType (remove id column)
		val.type <- provParseR::get.val.type(.debug.env$prov, d.id)
		val.type <- val.type[ , c("container", "dimension", "type")]
		
		# get start line and script number
		# for fromEnv variables, start line and script number are NA
		if(d.node$fromEnv) {
			p.fields <- data.frame(scriptNum = NA, startLine = NA,
									  stringsAsFactors = FALSE)
		}
		else
		{
			# for non-fromEnv variables, find the procedure nodes associated with it.
			# as there could be multiple, we need to use the given procedure node id
			# to get the closest one that is <= the given procedure node id.
			# e.g. for variables read into script multiple times
			proc.id <- .find.p.id(d.id, proc.data, data.proc)
			
			if(length(proc.id) > 1)
			{
				id.nums <- as.integer(unname(mapply(sub, "p", '', proc.id, SIMPLIFY = TRUE)))
				num <- as.integer(sub("p", '', p.id))
				
				index <- .find.num.loc(id.nums, num)
				proc.id <- proc.id[index]
			}
			
			# get procedure node fields
			p.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes == proc.id, ]
			p.fields <- proc.fields[ , c("scriptNum", "startLine")]
		}
		
		# combine fields into a row
		return(cbind(d.fields, val.type, p.fields, stringsAsFactors = FALSE))
	})
	
	# combine rows into a data frame
	return(.form.df(output))
}

# === ERROR ================================================================== #

#' @export
debug.error <- function(stack.overflow = FALSE)
{
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# get errors
	error.node <- .debug.env$data.nodes[.debug.env$data.nodes$name == "error.msg", ]
	
	# case: no error
	if(nrow(error.node) == 0) {
		cat("There are no errors in this script.")
		return(invisible(NULL))
	}
	
	# extract the first error
	error.node <- error.node[1, ]
	message <- error.node$value
	
	# the error
	cat(paste("Your Error: ", message, "\n", sep = ""))
	
	# get lineage
	lineage <- .get.lineage(error.node$id, forward = FALSE)
	
	# query Stack Exchange API
	if(stack.overflow)
	{
		tryCatch({
			.search.stackoverflow(message)
		},
		error = function(e){
			warning(e$message)
			warning("Connection to Stack Overflow did not succeed.")
		})
	}
	
	# return
	return(.get.output.lineage(lineage))
}

#' @noRd
.search.stackoverflow <- function(search.query, order = "desc", sort = "votes", tagged = "r") 
{
	search.query <- .process.error(search.query)

	# The url is the name of the api site
	url <- "http://api.stackexchange.com"
	# The path shows the version of the API and all the options the
	# user is choosing
	path <- paste("/2.2/search?order=", order,
				  "&sort=", sort,
				  "&tagged=", tagged, "
				  &intitle=", search.query,
				  "&site=stackoverflow",
				  sep ="")

	# Query the site for the information
	raw.result <- httr::GET(url = url, path = URLencode(path))

	# A 200 status code is a success, an unsuccesful code would be something
	# like 400, 404, etc
	if(raw.result$status_code != 200) {
		stop("Connection to Stack Overflow Did Not Succeed")
	}

	# parse the content
	result <- jsonlite::fromJSON(rawToChar(raw.result$content))

	# USER INPUT
	# Grab the titles and links to the questions
	pos.urls <- head(result$items)[, c("title", "link")]

	# This serves as a "menu" of sorts since it will print the row number
	# of each title
	print(pos.urls[, "title"])

	# They can either choose none or an index that will be matched to a row
	cat("\nChoose a numeric value that matches your error the best or q to quit: \n")
	chosen.result <- readline()

	if(!chosen.result == "q") 
	{
		chosen.result <- as.integer(chosen.result)

		# The input needs to be an integer so it can be used to
		# index into the rows of the data frame
		if(is.na(chosen.result)){
			stop("Invalid Input")
		} else if (chosen.result > 6 || chosen.result < 1) {
			stop ('Choose an option between 1 - 6')
		}

		# Open up the requested link in the default web browser
		browseURL(pos.urls[chosen.result ,]$link)
		cat("\nCode that led to error message:\n")
	}
}

#' @noRd
.process.error <- function(error.message)
{
	split <- strsplit(error.message, ":")[[1]]

	# Error messages from the prov.json will
	#typically have an uneeded prefix followed
	# by a colon ":"
	if(length(split) > 1) {
		error.message <- split[-1]
	}

	# This complicated mess of regex i=actually checks for 4 things (all inclusive):
	# Matches to characters surronded by quotes "dog"
	# Matches to characters surronded by escaped quotes \"dog\"
	# Matches to characters surronded by single quotes 'dog'
	# Matches to characters surronded by escaped quotes \'dog\'
	exp <- "\\\"[^\"\r]*\\\"|\"[^\"\r]*\"|\'[^\"\r]*\'|\\\'[^\"\r]*\\\'"
	error.message <- gsub(exp, "", error.message, perl = T)

	# remove whitespace from beginning and end
	exp <- "^ *| *\\\n*$"
	error.message <- gsub(exp, "", error.message)

	return(error.message)
}

# === WARNING ================================================================ #

#' @export
debug.warning <- function(..., all = FALSE)
{
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# get all warning nodes
	warning.nodes <- .debug.env$data.nodes
	warning.nodes <- warning.nodes[warning.nodes$name == "warning.msg", 
								   c("id", "value")]
	
	if(nrow(warning.nodes) == 0) {
		cat("There are no warnings in this script.")
		return(invisible(NULL))
	}
	
	num.warnings <- 1:nrow(warning.nodes)
	row.names(warning.nodes) <- num.warnings
	
	valid.queries <- .get.valid.query.warn(warning.nodes, ..., all = all)
	
	if(is.null(valid.queries))
		return(invisible(NULL))
	
	output <- lapply(valid.queries$id, function(id) {
		return(.get.output.lineage(.get.lineage(id)))
	})
	
	names(output) <- row.names(valid.queries)
	return(output)
}

#' @noRd
.get.valid.query.warn <- function(warning.nodes, ..., all = FALSE)
{
	if(all)
		return(warning.nodes)
	
	query <- unique(.flatten.args(...))
	
	if(is.null(query)) {
		.print.pos.warnings(warning.nodes)
		return(NULL)
	}
	
	pos.values <- row.names(warning.nodes)
	
	valid.cells <- sapply(1:length(query), function(i) {
		return(query[i] %in% pos.values)
	})
	
	# print invalid queries
	invalid <- query[!valid.cells]
	
	if(length(invalid) > 0) {
		cat(paste(invalid, " is not a possible query.", sep="", collapse = "\n"))
		cat("\n\n")
	}
	
	valid.queries <- query[valid.cells]
	
	if(length(valid.queries) == 0) {
		.print.pos.warnings(warning.nodes)
		return(NULL)
	}
	
	# as debug.warning requires users to query by warning node number
	# this is equivallent to the row numbers of the table of warning nodes
	# therefore, the valid queries can be directly used to extract from the warning node table
	return(warning.nodes[valid.queries, ])
}

#' @noRd
.print.pos.warnings <- function(warning.nodes)
{
	.print.pos.options(warning.nodes)
	cat("\nPass the corresponding numeric value to the function for info on that warning.\n")
}

# === UTILITY ================================================================ #

#' Collapses the given parameters into a single list.
#'
#' @param ... The parameteres to be collapsed.
#' @return The given parameters, collapsed into a single list.
#'
#' @noRd
.flatten.args <- function(...)
{
	return(unlist(list(...)))
}

#' Combine a list of data frames into a single data frame.
#'
#' @param list The list of data frames to be combined.
#'			   All data frames in this list must have the same columns.
#'			   Guarenteed to have at least one element.
#' @return The combined data frame.
#'
#' @noRd
.form.df <- function(list)
{	
	# get column names
	col.names <- names(list[[1]])
	
	# form data frame
	col.length <- 1:length(col.names)
	
	cols <- lapply(col.length, function(i) 
	{
		return(unlist(mapply(`[[`, list, i)))
	})
	
	names(cols) <- col.names
	df <- data.frame(cols, stringsAsFactors = FALSE)
	
	rownames(df) <- 1:nrow(df)
	return(df)
}

#' Converts a query to an integer.
#' NA and negative integers are accepted.
#' This is used in checking the validity of queried line or script numbers.
#'
#' @param arg The argument to be converted to an integer.
#' @return The argument as an integer, NA, or NULL if the argument is not an integer.
#'
#' @noRd
.to.int <- function(arg)
{
	# Case: If it's already an integer, return it
	if(is.integer(arg))
		return(arg)
	
	# Case: NA is a potentially valid query.
	if(is.na(arg) || arg == "NA")
		return(as.integer(NA))
	
	# Case: Catch logicals. This is because they will be coerced into 1 or 0
	if(is.logical(arg))
		return(NULL)
	
	# Try to coerce into an integer
	# this is so that integers as strings are accepted
	arg.int <- suppressWarnings(as.integer(arg))
	
	# Case: can't be coerced into an integer, return NULL
	if(is.na(arg.int))
		return(NULL)
	
	# Case: make sure decimals are not accepted
	# this is because coercing into an integer truncates decimals
	if(arg != arg.int)
		return(NULL)
	
	# return
	return(arg.int)
}

#' Finds the location of a number within an ordered list of unique numbers.
#'
#' @param num The number. Guarenteed to fall within the list.
#' @return The index of the given list where the number falls right after.
#'
#' @noRd
.find.num.loc <- function(nums.list, num)
{
	# tries to find the number within the list
	index <- c(1:length(nums.list))[nums.list == num]
	
	if(length(index) > 0)
		return(index)
	
	# if exact match can not be found, find the closest index where
	# number in list < num
	# uses binary search	
	low.index <- 1
	high.index <- length(nums.list)
	
	while(high.index - low.index > 1)
	{
		mid.index <- as.integer((low.index + high.index)/2)
		
		if(num < nums.list[mid.index])
			high.index <- mid.index
		else
			low.index <- mid.index
	}
	
	return(low.index)
}

#' Finds the associated proc node id from the given data node id.
#'
#' @param d.id The data node id.
#' @param proc.data The list of procedure-to-data edges.
#' @param data.proc The list of data-to-procedure edges.
#'
#' @return The associated procedure node id, or a list of id if there are multiple.
#'
#' @noRd
.find.p.id <- function(d.id, proc.data, data.proc)
{
	# Search output edges first, where the data node is produced.
	# There should only ever be 1, if any
	p.id <- proc.data$activity[proc.data$entity == d.id]
	
	# if no output edges found, then search input edges
	# it is possible there are multiple
	if(length(p.id) == 0)
		p.id <- data.proc$activity[data.proc$entity == d.id]
	
	return(p.id)
}

#' From the given data frame, remove rows that are all NA, if any.
#' This is needed because sometimes, rows of NA values will be inserted
#' into the data frame.
#'
#' @param df The data frame. May have no rows.
#' @return The data frame with NA rows, if any, removed. Could have no rows.
#' 
#' @noRd
.remove.na.rows <- function(df)
{
	if(nrow(df) == 0)
		return(df)
	
	valid.rows <- sapply(c(1:nrow(df)), function(i)
	{
		row <- as.list(df[i, ])
		return(!all(is.na(row)))
	})
	
	return(df[valid.rows, ])
}

#' Remove elements which are NULL in a given list. 
#' 
#' @param list The list. Has at least 1 element.
#' @return The list with NULL elements are removed. Could be empty.
#'
#' @noRd
.remove.null <- function(list)
{
	nulls <- sapply(list, is.null)
	return(list[!nulls])
}

#' Prints the table or list of possible options to standard output.
#' @param pos.args The table or list of possible options.
#'
#' @noRd
.print.pos.options <- function(pos.args)
{
	cat("Possible options:\n")
	
	if(!is.data.frame(pos.args)) {
		pos.args <- as.data.frame(pos.args)
		colnames(pos.args) <- NULL
	}
	
	print(pos.args)
}
