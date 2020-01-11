# === LINE =================================================================== #

#' @export
debug.line <- function(..., script.num = 1, all = FALSE)
{
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# get valid lines from user input
	valid.lines <- .get.valid.line(..., script.num = script.num, all = all)
	
	# case: no valid lines
	if(is.null(valid.lines))
		return(invisible(NULL))
	
	# get output for each valid line
	# since valid.lines is a table, we perform lapply on row index instead
	# this makes it easy to query the table by row
	indices <- c(1:nrow(valid.lines))
	
	output <- lapply(indices, function(i)
	{
		# a list which stores tables for input data nodes and output data nodes
		data <- list()
		
		# get the activity node id (proc node id)
		activity.id <- valid.lines$id[i]
		
		# INPUT data nodes
		# get all input data node id associated with activity id
		# then form user output table
		input.id <- .debug.env$data.proc$entity[.debug.env$data.proc$activity == activity.id]
		
		if(length(input.id) == 0)
			data$input <- NA
		else
			data$input <- .get.output.line(input.id)
		
		# OUTPUT data nodes
		# same as for input data nodes, but for output data nodes instead
		output.id <- .debug.env$proc.data$entity[.debug.env$proc.data$activity == activity.id]
		
		if(length(output.id) == 0)
			data$output <- NA
		else
			data$output <- .get.output.line(output.id)
		
		# NULL CASE: no input or output nodes
		if(length(input.id) == 0 && length(output.id) == 0) {
			cat(paste("There are no input or output data nodes associated with line", valid.lines$startLine[i], 
					  "of script", valid.lines$scriptNum[i], ".\n"))
			return(NULL)
		}
		
		# USER OUTPUT - before returning completed tables, print line and script number
		cat(paste("Line", valid.lines$startLine[i], " of script", valid.lines$scriptNum[i], "\n"))
		return(data)
	})
	
	# name all tables with line number
	names(output) <- valid.lines$startLine
	return(output)
}

#' @noRd
.get.valid.line <- function(..., script.num = 1, all = FALSE)
{
	# get all possible nodes for query
	all.pos.nodes <- .debug.env$proc.nodes[.debug.env$proc.nodes$type == "Operation", ]
	all.pos.nodes <- all.pos.nodes[ , c("id", "startLine", "scriptNum")]
	
	# QUERY: ALL
	if(all)
		return(all.pos.nodes)
	
	# get user's query into vector form, 
	query <- unique(.flatten.args(...))
	
	# CASE: no queries
	if(is.null(query)) {
		print.pos.options(all.pos.nodes)
		return(invisible(NULL))
	}
	
	# append script.num to it to form data frame
	query <- data.frame(query, rep(script.num, length(query)), stringsAsFactors = FALSE)
	names(query) <- c("startLine", "scriptNum")
	
	# QUERY: subset by script number
	pos.nodes <- all.pos.nodes[all.pos.nodes$scriptNum == script.num, ]
	
	if(nrow(pos.nodes) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n"))
		return(invisible(NULL))
	}
	
	# QUERY: get valid line numbers
	query.indices <- c(1:nrow(query))
	
	# this list keeps track of node id for valid line numbers
	valid.id <- c()
	
	# for every line number, search for a corresponding proc node id.
	# if none, the line number queried is invalid - return FALSE
	# if found, the line number queried is valid - return TRUE, keep track of proc node id
	# this creates a vector of logicals which can be used to subset the table of queries
	valid.cells <- sapply(query.indices, function(i) 
	{
		node.id <- pos.nodes$id[pos.nodes$startLine == query$startLine[i]]
		
		if(length(node.id) == 0)
			return(FALSE)
		
		valid.id <<- append(valid.id, node.id)
		return(TRUE)
	})
	
	# extract valid queries from all queries, cbind id column to it
	valid.queries <- query[valid.cells, ]
	valid.queries <- cbind("id"=valid.id, valid.queries)
	
	# CASE: no valid queries
	if(nrow(valid.queries) == 0) {
		return(invisible(NULL))
	}
	
	# RETURN VALID QUERIES
	return(valid.queries)
}

#' @noRd
.get.output.line <- function(id.list)
{
	rows <- lapply(id.list, function(id)
	{
		data.node <- .debug.env$data.nodes[.debug.env$data.nodes$id == id, c("name", "value")]
		val.type <- provParseR::get.val.type(.debug.env$prov, id)[c("container", "dimension", "type")]
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
	valid.queries <- .get.valid.var(pos.vars, query)
	
	# CASE: no valid query
	if(is.null(valid.queries)) {
		cat("No valid queries.\n\n")
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
	# CASE: no variables
	if(nrow(data.nodes) == 0)
		return(NULL)
	
	# from data nodes, keep columns: id, name, valType
	# rename id column to d.id
	data.nodes <- data.nodes[ , c("id", "name", "valType")]
	colnames(data.nodes) <- c("d.id", "name", "valType")
	
	# for each data node, get the corresponding procedure node
	proc.nodes <- lapply(data.nodes$`d.id`, function(d.id)
	{
		# try to get the procedure node that assigned/produced the data node
		p.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == d.id]
		
		# alternatively, get the procedure node that first used the data (e.g. url)
		if(length(p.id) == 0)
			p.id <- .debug.env$data.proc$activity[.debug.env$data.proc$entity == d.id]
		
		# get startLine and scriptNum from proc nodes table
		p.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes$id == p.id,
										  c("id", "startLine", "scriptNum")]
		
		# rename id field to p.id, return
		colnames(p.fields) <- c("p.id", "startLine", "scriptNum")
		return(p.fields)
	})
	
	# bind into a single data frame
	proc.nodes <- .form.df(proc.nodes)
	
	# cbind data nodes and their corresponding proc nodes into a single data frame
	pos.nodes <- cbind(data.nodes, proc.nodes, stringsAsFactors = FALSE)
	
	# rearrange columns, rename rows
	pos.nodes <- pos.nodes[ , c("d.id", "p.id", "name", "valType", "startLine", "scriptNum")]
	row.names(pos.nodes) <- c(1:nrow(pos.nodes))
	
	return(pos.nodes)
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
		warning("Please query only 1 script number.")
		return(NULL)
	}
	
	# CASE: more than 1 valType queried
	if(length(val.type) > 1) {
		warning("Please query only 1 valType.")
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
				an equal number of objects and start lines.")
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
#' returned columns: d.id, p.id, name, valType, startLine, scriptNum
#'
#' function shared with debug.lineage
#'
#' @noRd
.get.valid.var <- function(pos.nodes, query, forward = FALSE)
{
	# CASE: no queries
	if(is.null(query))
		return(invisible(NULL))
	
	# QUERY: subset by script number
	# Since there can only be 1 script number queried per call,
	# check for its validity first.
	script.num <- query$scriptNum[1]
	pos.nodes <- pos.nodes[pos.nodes$scriptNum == script.num, ]
	pos.nodes <- na.omit(pos.nodes)
	
	# CASE: invalid script num
	if(nrow(pos.nodes) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n"))
		return(invisible(NULL))
	}
	
	# STEP: check validity of each query (row)
	query.indices <- c(1:nrow(query))
	
	# (for forward/backward lineage queries)
	# store id of valid data nodes and its corresponding proc node when found
	# this is so that in cases where no start.line is searched for,
	# we will know which node id to return
	valid.d.id <- c()
	valid.p.id <- c()
	
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
		subset <- na.omit(subset)
		
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
			subset <- na.omit(subset)
			
			# CASE: no nodes with queried valType found - return false
			if(nrow(subset) == 0)
				return(FALSE)
		}
		
		# (for lineage queries)
		# QUERY: start line queried is NA, 
		# find the id of the node to be used
		if(is.na(query.line))
		{
			# extract data node id and corresponding proc node id columns
			d.id <- subset$`d.id`
			p.id <- subset$`p.id`
			
			# find the id of the node to be used
			# forward lineage - get first node
			# backwards lineage - get last node
			if(nrow(subset) == 1) {
				valid.d.id <<- append(valid.d.id, d.id)
				valid.p.id <<- append(valid.p.id, p.id)
			}
			else if(forward) {
				valid.d.id <<- append(valid.d.id, d.id[1])
				valid.p.id <<- append(valid.p.id, p.id[1])
			}
			else {
				valid.d.id <<- append(valid.d.id, d.id[length(d.id)])
				valid.p.id <<- append(valid.p.id, p.id[length(p.id)])
			}
			
			# node is found - return true
			return(TRUE)
		}
		
		# QUERY: search for queried start line
		subset <- subset[subset$startLine == query.line, ]
		subset <- na.omit(subset)
		
		# CASE: start line not found
		if(nrow(subset) == 0)
			return(FALSE)
		
		# node found: record data node id and corresponding proc node id
		valid.d.id <<- append(valid.d.id, subset$`d.id`)
		valid.p.id <<- append(valid.p.id, subset$`p.id`)
		
		return(TRUE)
	})
	
	# STEP: extract valid queries
	valid.queries <- na.omit(query[valid.indices, ])
	
	# CASE: no valid queries
	if(nrow(valid.queries) == 0)
		return(invisible(NULL))
	
	# STEP: bind valid data node id and proc node id columns to valid queries
	valid.queries <- cbind("d.id" = valid.d.id,
						   "p.id" = valid.p.id,
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
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# get table of possible variables
	# this includes valType, start line and script number
	pos.vars <- .get.pos.var(.debug.env$data.nodes)
	
	# get valid queries - all data nodes are possible nodes
	valid.queries <- .get.valid.var(pos.vars, ..., val.type = NA, start.line = start.line, 
									script.num = script.num, all = all, forward = forward)
	
	# case: no valid queries
	if(is.null(valid.queries))
		return(invisible(NULL))
	
	# get lineage
	valid.vars.id <- valid.queries$id
	
	lineages <- lapply(valid.vars.id, function(id)
	{
		lineage <- .get.lineage(id, forward = forward)
		return(.get.output.lineage(lineage))
	})
	
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
	
	return(.form.df(rows))
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

#' @export 
debug.state <- function(..., script.num = 1)
{
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# Get all possible procedure nodes that can be queried. (Operation nodes)
	pos.proc.all <- .debug.env$proc.nodes[.debug.env$proc.nodes$type == "Operation", ]
	
	# Remove non-variables from table of data nodes
	# Remove proc nodes that do not have output edges to variables from proc node table
	# Remove output edges to non-variables from proc-data edge table
	pos.data <- .debug.env$data.nodes[.debug.env$data.nodes$type == "Data" | 
									  .debug.env$data.nodes$type == "Snapshot", ]
	pos.proc <- .remove.non.vars.proc(pos.proc.all)
	pos.edges <- .remove.non.vars.proc.data()
	
	# get user's query
	query <- .flatten.args(...)
	
	# no queries - show state at end of execution
	if(is.null(query))
	{
		cat("State at the end of execution:\n")
		last.proc <- pos.proc.all[nrow(pos.proc.all), ]
		
		query <- last.proc$startLine
		script.num <- last.proc$scriptNum
	}
	else   # check for valid script number query
	{
		if(length(script.num) > 1) {
			warning("Please query only 1 script number.")
			return(NULL)
		}
	}
	
	# subset by queried script number
	pos.proc <- pos.proc[pos.proc$scriptNum == script.num, ]
	
	if(nrow(pos.proc) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n"))
		return(invisible(NULL))
	}
	
	# for each query, obtain state of all variables at that time
	# keep a vector for keeping track of indices where the query is invalid
	# or there is no state.
	remove.indices <- c()
	
	output <- lapply(c(1:length(query)), function(i)
	{
		line <- query[i]
		
		# find closest proc node with line number <= queried line
		closest.proc <- .get.closest.proc(pos.proc, line)
		
		if(is.null(closest.proc)) {
			remove.indices <<- append(remove.indices, i)
			return(NULL)
		}
		
		# get closest data node where its proc node has line number <= queried line
		closest.data <- .get.closest.data(closest.proc, pos.proc, pos.edges)
		
		# get state
		state <- .get.state(closest.data, pos.edges, pos.data)
		
		if(is.null(state)) {
			cat(paste("There are no variables in the environment at line", line, ".\n"))
			remove.indices <<- append(remove.indices, i)
			return(NULL)
		}
		
		return(state)
	})
	
	# remove elements, if any, for invalid queries & no state
	if(length(remove.indices) > 0) {
		query <- query[-remove.indices]
		output <- output[-remove.indices]
	}
	
	# no state to display
	if(length(output) == 0) {
		return(invisible(NULL))
	}
	
	# return
	names(output) <- query
	return(output)
}

#' @noRd
.remove.non.vars.proc <- function(pos.proc)
{	
	# get list of non-variables
	data.nodes <- .debug.env$data.nodes
	non.vars <- data.nodes[!(data.nodes$type == "Data" | data.nodes$type == "Snapshot"), "id"]
	
	# nothing to remove
	if(length(non.vars) == 0)
		return(pos.proc)
	
	# for each non-variable, get corresponding proc node
	# find index of proc node in proc node table
	# add to list to be removed later
	remove.proc <- c()
	
	lapply(non.vars, function(data.id) 
	{
		proc.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == data.id]
		
		# no output edge
		if(length(proc.id) == 0)
			return(NULL)
		
		index <- c(1:nrow(pos.proc))[pos.proc$id == proc.id]
		remove.proc <<- append(remove.proc, index)
	})
	
	# if there is nothing to remove
	if(length(remove.proc) == 0)
		return(pos.proc)
	
	# otherwise, remove from proc node table
	return(pos.proc[-remove.proc, ])
}

# Remove output edges to non-variables from proc-data edge table
#' @noRd
.remove.non.vars.proc.data <- function()
{
	data.nodes <- .debug.env$data.nodes
	proc.data <- .debug.env$proc.data
	
	# get list of non-variables
	non.vars <- data.nodes[!(data.nodes$type == "Data" | data.nodes$type == "Snapshot"), "id"]
	
	# nothing to remove
	if(length(non.vars) == 0)
		return(proc.data)
	
	# for each non-variable, find index of output edge in proc-data edge table
	# if found, add to list to be removed from table later.
	remove.indices <- c()
	
	lapply(non.vars, function(node)
	{
		indices <- row.names(proc.data)[proc.data$entity == node]
		
		if(length(indices) > 0)
			remove.indices <<- append(remove.indices, as.integer(indices))
	})
	
	# extract nodes to be removed
	return(proc.data[-remove.indices, ])
}

# get closest procedure node id with line number <= queried line number
# returns p0 for pre-existing data nodes (where fromEnv == TRUE)
#' @noRd
.get.closest.proc <- function(pos.nodes, line)
{
	# try to parse the given line number as an integer.
	# it is possible that line number queries are coerced into strings.
	line.int <- suppressWarnings(as.integer(line))
	
	# not a number - invalid
	if(is.na(line.int) || line.int < 1) {
		cat(paste(line, "is not a possible line number.\n"))
		return(NULL)
	}
	
	# Try to find the proc node which corresponds to the given line number
	node <- pos.nodes[pos.nodes$startLine == line.int, ]
	
	# Case: line number can be found
	# Since there could be multiple proc nodes on one line,
	# return the node id of the proc node with the highest proc node id.
	if(nrow(node) > 0)
		return(node$id[nrow(node)])
	
	# Get list of all line numbers.
	# Need to find where queried line number fits into this list.
	line.nums <- pos.nodes$startLine
	
	# Case: queried line number < line number of first proc node
	if(line < line.nums[1])
		return("p0")
	
	# Case: queried line number > line number of last proc node
	if(line.int > line.nums[length(line.nums)])
		return(pos.nodes$id[length(line.nums)])
	
	# In the list of line numbers, find where the queried line inserts.
	# The function returns the index where the queried line number would be after.
	return(pos.nodes$id[.find.line.loc(line.nums, line.int)])
}

# location is guarenteed to be between possible line numbers
#' @noRd
.find.line.loc <- function(line.nums, line)
{
	# uses binary search	
	low.index <- 1
	high.index <- length(line.nums)
	
	while(high.index - low.index > 1)
	{
		mid.index <- as.integer((low.index + high.index)/2)
		
		if(line < line.nums[mid.index])
			high.index <- mid.index
		else
			low.index <- mid.index
	}
	
	return(low.index)
}

#' @noRd
.get.closest.data <- function(proc.node, pos.proc, pos.edges)
{
	# case: proc.node == "p0"
	# this indicates that data nodes that were present before script execution
	# should be returned (where fromEnv == TRUE)
	if(proc.node == "p0")
		return("d0")
	
	# find the index of the given proc node in table of operation nodes
	index <- c(1:nrow(pos.proc))[pos.proc$id == proc.node]
	
	# from the given proc node, loop up the table of proc nodes until
	# one with output (proc-to-data) edges are found, or there are no
	# more proc nodes to loop through.
	while(index > 0)
	{
		# get output edges, if any, from specified proc node
		entities <- pos.edges$entity[pos.edges$activity == proc.node]
	
		# if there are output data nodes, 
		# return data node with largest data node number
		if(length(entities) > 0)
			return(entities[length(entities)])
		
		# otherwise, decrement index
		index <- index - 1
	}
	
	# no output edges found, return "d0" 
	# (special case to get nodes where fromEnv == TRUE)
	return("d0")
}

#' @noRd
.get.state <- function(data.node, pos.edges, pos.data)
{
	# get id of data nodes where fromEnv is TRUE
	var.id <- pos.data$id[pos.data$fromEnv]
	
	# get data nodes that are generated during execution of script
	# "d0" is for the case where just the nodes from before script execution
	# should be returned
	if(data.node != "d0")
	{
		max.index <- c(1:nrow(pos.edges))[pos.edges$entity == data.node]
		var.id <- append(var.id, pos.edges$entity[1:max.index])
		
		# get var names for each data id
		var.names <- sapply(var.id, function(id) {
			return(pos.data$name[pos.data$id == id])
		})
		
		# get unique var names
		# for each unique var name, get the last data node id.
		unique.vars <- unique(var.names)
		
		if(length(unique.vars) < length(var.names))
		{
			# this is a table of possible nodes and their variable names
			pos.vars <- data.frame(var.id, var.names, stringsAsFactors = FALSE)
			colnames(pos.vars) <- c("id", "name")
		
			var.id <- sapply(unique.vars, function(name) {
				id.list <- pos.vars$id[pos.vars$name == name]
				return(id.list[length(id.list)])
			})
		}
	}
	
	# edge: no data nodes
	if(length(var.id) == 0)
		return(NULL)
	
	# form output
	return(.get.output.state(var.id))
}

#' @noRd
.get.output.state <- function(id.list)
{	
	# output consists of: var name, value, valType, scriptNum, startLine
	output <- lapply(id.list, function(id)
	{
		data.node <- .debug.env$data.nodes[.debug.env$data.nodes$id == id, ]
		
		# fields from data node
		data.fields <- data.node[ , c("name", "value")]
		
		# valType (remove id column)
		valType <- provParseR::get.val.type(.debug.env$prov, id)
		valType <- valType[ , c("container", "dimension", "type")]
		
		# startLine and scriptNum
		if(data.node$fromEnv) {
			proc.fields <- data.frame("scriptNum" = NA, "startLine" = NA, 
									  stringsAsFactors = FALSE)
		}
		else
		{
			proc.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == id]
			proc.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes$id == proc.id, 
												 c("scriptNum", "startLine")]
		}
		
		# bind fields into a row
		return(cbind(data.fields, valType, proc.fields, stringsAsFactors = FALSE))
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
		cat("There were no errors in this script!")
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
	warning.nodes <- .debug.env$data.nodes[.debug.env$data.nodes$name == "warning.msg", 
										   c("id", "value")]
	
	if(nrow(warning.nodes) == 0) {
		cat("There were no warnings in this script!")
		return(invisible(NULL))
	}
	
	num.warnings <- 1:nrow(warning.nodes)
	row.names(warning.nodes) <- num.warnings
	
	valid.queries <- .get.valid.warn(warning.nodes, ..., all = all)
	
	if(is.null(valid.queries))
		return(invisible(NULL))
	
	output <- lapply(valid.queries$id, function(id) {
		return(.get.output.lineage(.get.lineage(id)))
	})
	
	names(output) <- row.names(valid.queries)
	return(output)
}

#' @noRd
.get.valid.warn <- function(warning.nodes, ..., all = FALSE)
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

#' @noRd
.flatten.args <- function(...)
{
	return(unlist(list(...)))
}

#' @noRd
.form.df <- function(list)
{
	# get column names
	col.names <- names(list[[1]])
	
	# form data frame
	col.length <- 1:length(col.names)
	
	cols <- lapply(col.length, function(i) 
	{
		return(mapply(`[[`, list, i))
	})
	
	names(cols) <- col.names
	
	df <- data.frame(cols, stringsAsFactors = FALSE)
	
	rownames(df) <- 1:nrow(df)
	return(df)
}

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
