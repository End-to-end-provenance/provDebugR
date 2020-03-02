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

# === STATE ================================================================== #

#' assumes that the line had been executed
#' script num ignored if no line 
#' @export
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
#' @param ... The user's line queries
#' @param script.num The script number to be queried.
#' 
#' @return The table of valid queries.
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
		
		return(proc.nodes$id[index])
	}
	
	# Case: line falls somewhere in the list or after it.
	return(script.proc$id[script.index])
}

#' From the given procedure node id, loop up the table of procedure nodes until
#' an output variable is found. Returns the data node id of the last variable,
#' or NULL if none are found. Also returns NULL if the procedure node id given is NULL.
#'
#' @param The procedure node id where the search for an variable begins.
#' @return The data node id of the variable found, or NULL if none are found.
#'
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

#' get all data nodes up to specified num
#' for each unique variable name, get the last occurrance of it
#' this forms the state
#'
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
			warning(e$message, call. = FALSE)
			warning("Connection to Stack Overflow did not succeed.", call. = FALSE)
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
	chosen.result <- tolower(trimws(readline()))

	while(chosen.result != "q") 
	{
		chosen.result <- suppressWarnings(as.integer(chosen.result))

		# The input needs to be an integer so it can be used to
		# index into the rows of the data frame
		if(is.na(chosen.result) || (chosen.result > 6 || chosen.result < 1)){
			cat("Invalid Input. Please choose an option between 1 - 6 or q to quit.\n")
			chosen.result <- tolower(trimws(readline()))
			next
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

#' Finds the location of a number within an ordered list of numbers.
#'
#' @param num The number. Guarenteed to fall within the list.
#' @return The index of the given list where the number falls right after.
#'
#' @noRd
.find.num.loc <- function(nums.list, num)
{
	# tries to find the number within the list
	index <- c(1:length(nums.list))[nums.list == num]
	
	# Case: if found, this returns the last index if there are multiple indices.
	if(length(index) > 0)
		return(index[length(index)])
	
	# Case: Out of bounds
	# if number falls before the numbers in the list, return 0
	# if number falls after the numbers in the list, return the last index of the list.
	if(num < nums.list[1])
		return(0)
	
	last.index <- length(nums.list)
	
	if(num > nums.list[last.index])
		return(last.index)
	
	# Case: in between numbers in the list
	# find the closest index where number in list < num
	# uses binary search	
	low.index <- 1
	high.index <- last.index
	
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
#' @return The associated procedure node id, or a list of id if there are multiple.
#'
#' @noRd
.get.p.id <- function(d.id)
{
	# Search output edges first, where the data node is produced.
	# There should only ever be 1, if any
	p.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == d.id]
	
	# if no output edges found, then search input edges
	# it is possible there are multiple
	if(length(p.id) == 0)
		p.id <- .debug.env$data.proc$activity[.debug.env$data.proc$entity == d.id]
	
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
