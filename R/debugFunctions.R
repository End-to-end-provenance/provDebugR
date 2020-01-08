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
	query <- unique(unlist(list(...)))
	
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
	
	# USER OUTPUT: print invalid queries, if any
	.print.invalid.queries(query[!valid.cells, ])
	
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
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	data.nodes <- .debug.env$data.nodes
	
	# get table of possible variables
	# this includes valType, start line and script number
	# only nodes with type = "Data" or "Snapshot" are permitted here.
	pos.vars <- .get.pos.vars(data.nodes[data.nodes$type == "Data" | data.nodes$type == "Snapshot", ])
	
	# get valid queries, keeping name, valType, scriptNum columns
	valid.queries <- .get.valid.var(pos.vars, ..., val.type = val.type, script.num = script.num, all = all)
	valid.queries[ , c("name", "valType", "scriptNum")]
	
	# case: no valid queries
	if(is.null(valid.queries))
		return(invisible(NULL))
	
	# get user output
	variables <- lapply(c(1:nrow(valid.queries)), function(i) {
		return(.get.output.var(pos.vars, valid.queries[i, ]))
	})
	
	names(variables) <- valid.queries$name
	return(variables)
}

#' @noRd
.get.pos.vars <- function(pos.nodes)
{
	# list possible variables, taking into account start line and script number
	id.list <- pos.nodes$id
	
	rows <- lapply(id.list, function(data.id)
	{
		# get line of code that assigned/produced the data
		proc.id <- .debug.env$proc.data$activity[.debug.env$proc.data$entity == data.id]
		
		# alternatively, get the line that first used the data (e.g. URL nodes)
		if(length(proc.id) == 0)
			proc.id <- .debug.env$data.proc$activity[.debug.env$data.proc$entity == data.id]
		
		# get startLine, script number, code from the proc nodes
		proc.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes$id == proc.id, 
											 c("startLine", "scriptNum", "name")]
		names(proc.fields) <- c("startLine", "scriptNum", "code")
		return(proc.fields)
	})
	
	# form the data frame of startLine and scriptNum
	pos.vars <- .form.df(rows)
	
	# cbind id, name, value, valType of variable to df
	data.fields <- pos.nodes[ , c("id", "name", "value", "valType")]
	pos.vars <- cbind(data.fields, pos.vars, stringsAsFactors = FALSE)
	
	# return unique rows
	return(unique(pos.vars))
}

#' @noRd
.get.valid.var <- function(pos.nodes, ..., val.type = NA, start.line = NA, script.num = 1, all = FALSE, forward = FALSE)
{
	# save table of all possible nodes - this is required later
	all.pos.nodes <- pos.nodes
	
	# QUERY: ALL
	if(all)
		return(all.pos.nodes)
	
	# get user's query
	query <- .get.query.var(..., val.type = val.type, start.line = start.line)
	
	# CASE: no queries
	if(is.null(query)) {
		.print.pos.options(all.pos.nodes)
		return(NULL)
	}
	
	# QUERY: subset by script number
	pos.nodes <- all.pos.nodes[all.pos.nodes$scriptNum == script.num, ]
	
	if(nrow(pos.nodes) == 0) {
		cat(paste("Script number", script.num, "is not a possible input.\n"))
		return(NULL)
	}
	
	# QUERY: check for valid variable and start line combinations
	query.indices <- c(1:nrow(query))
	
	# store id of valid data nodes when found
	# this is so that in cases where no start.line is searched for,
	# we will know which node id to return (for forward/backward lineage queries)
	valid.id <- c()
	
	# this is akin to a loop where, for every row (var/line combination),'
	# a TRUE or FALSE will be returned. TRUE corresponds to valid inputs.
	# this is used later to extract the table of valid inputs.'
	valid.cells <- sapply(query.indices, function(i)
	{
		# the query
		query.var <- query$name[i]
		query.valType <- query$valType[i]
		query.line <- query$startLine[i]
		
		# get indices of variables found, if any
		pos.indices <- as.integer(row.names(pos.nodes))[pos.nodes$name == query.var]
		
		# CASE: no row with var found - return false
		if(length(pos.indices) == 0)
			return(FALSE)
		
		# filter by valType 
		if(!is.na(query.valType))
		{
			# get the regex form for the valType query
			query.valType <- paste("*", query.valType, "*", sep="")
			
			# get the valType column for the data nodes where name == query
			# extract the cells where the queried valType can be found
			valTypes <- pos.nodes$valType[pos.indices]
			pos.indices <- pos.indices[grep(query.valType, valTypes)]
			
			# CASE: no cells with queried valType found - return false
			if(length(pos.indices) == 0)
				return(FALSE)
		}
		
		# Start Line is NA - node depends on forward/backwards lineage wanted
		if(is.na(query.line))
		{
			node.id <- pos.nodes$id[pos.indices]
			
			# find the id of the node to be used.
			if(length(pos.indices) == 1)
				valid.id <<- append(valid.id, node.id)
			else if(forward)	# forward lineage - get first node
				valid.id <<- append(valid.id, node.id[1])
			else	# backwards lineage - get last node
				valid.id <<- append(valid.id, node.id[length(node.id)])
			
			return(TRUE)
		}
		
		# else: get all possible startLine values for the argument
		subset <- pos.nodes[pos.indices, ]
		
		# find index of startLine, if any
		node.id <- subset$id[subset$startLine == query.line]
		
		# CASE: no valid line found - return false
		if(length(node.id) == 0)
			return(FALSE)
		
		# record value of id
		valid.id <<- append(valid.id, node.id)
		return(TRUE)
	})
	
	# extract valid inputs
	valid.queries <- query[valid.cells, ]
	
	# USER OUTPUT: print invalid queries, if any
	.print.invalid.queries(query[!valid.cells, ])
	
	# CASE: no valid queries
	if(nrow(valid.queries) == 0) {
		return(NULL)
	}
	
	# cbind id column, return
	valid.queries <- cbind("id" = valid.id, valid.queries, stringsAsFactors=FALSE)
	return(valid.queries)
}

#' @noRd
.get.query.var <- function(..., val.type = NA, start.line = NA, script.num = 1)
{
	if(length(script.num) > 1) {
		warning("Please query only 1 script number.")
		return(NULL)
	}
	
	if(length(val.type) > 1) {
		warning("Please query only 1 valType.")
		return(NULL)
	}
	
	query.vars <- unlist(list(...))
	
	if(is.null(query.vars))
		return(NULL)
	
	query.types <- c()
	query.lines <- c()
	query.scripts <- c()
	
	if(length(start.line) == 1) 
	{
		query.types <- rep(val.type, length(query.vars))
		query.lines <- rep(start.line, length(query.vars))
		query.scripts <- rep(script.num, length(query.vars))
	}
	else if(length(query.vars) == 1)
	{
		query.lines <- start.line
		query.vars <- rep(query.vars, length(query.lines))
		query.types <- rep(val.type, length(query.lines))
		query.scripts <- rep(script.num, length(query.lines))
	}
	else if(length(query.vars) == length(start.line))
	{
		query.types <- rep(val.type, length(query.vars))
		query.lines <- start.line
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
	
	query.table <- data.frame(query.vars, query.types, query.lines, query.scripts, stringsAsFactors = FALSE)
	names(query.table) <- c("name", "valType", "startLine", "scriptNum")
	return(query.table)
}

#' @noRd
.get.output.var <- function(pos.vars, query)
{
	# extract all data nodes from pos.vars with
	# name == var, scriptNum == script.num
	nodes <- pos.vars[pos.vars$scriptNum == query$scriptNum, ]
	nodes <- nodes[nodes$name == query$name, ]
	
	# extract columns with queried valType, if it is not na
	if(!is.na(query$valType)) {
		query.valType <- paste("*", query$valType, "*", sep="")
		nodes <- nodes[grep(query.valType, nodes$valType), ]
	}
	
	# extract columns: id, value, startLine, scriptNum, code
	nodes <- nodes[ , c("id", "value", "startLine", "scriptNum", "code")]
	
	# get valType columns from provParseR
	valTypes <- provParseR::get.val.type(.debug.env$prov, nodes$id)
	
	# merge tables by id
	nodes <- merge(nodes, valTypes, by.x = "id")
	
	# order table by increasing node id
	# this involves stripping the id values of their 'd', then ordering them
	id.num <- as.integer(sub("^[[:alpha:]]", "", nodes$id))
	nodes <- nodes[order(id.num), ]
	
	# order columns, remove id column
	nodes <- nodes[ , c("value", 
						"container", "dimension", "type", 
						"scriptNum", "startLine", "code")]
	
	# re-number row numbers, return
	row.names(nodes) <- c(1:nrow(nodes))
	return(nodes)
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
	pos.vars <- .get.pos.vars(.debug.env$data.nodes)
	
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
			cat("No valid variables.\n")
			cat("Possible Options:\n")
			cat(paste(vars.names, collapse='\n'))
			cat('\n')
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
	query <- unlist(list(...))
	
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
	
	# extract the first error
	error.node <- .debug.env$data.nodes[.debug.env$data.nodes$name == "error.msg", ]
	error.node <- error.node[1, ]
	message <- error.node$value
	
	# no error
	if(length(message) == 0) {
		cat("There were no errors in this script!")
		return(invisible(NULL))
	}
	
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
	
	query <- unique(unlist(list(...)))
	
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
	cat("Possible results: \n")
	results.df <- as.data.frame(warning.nodes$value)
	colnames(results.df) <- NULL
	print(results.df)
	cat("\nPass the corresponding numeric value to the function for info on that warning.\n")
}

# === UTILITY ================================================================ #

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

# print invalid queries, if any.
#' @noRd
.print.invalid.queries <- function(invalid)
{	
	if(length(invalid) == 0)
		return(NULL)
	
	if(nrow(invalid) == 0)
		return(NULL)
	
	indices <- c(1:nrow(invalid))
	
	sapply(indices, function(i) 
	{
		output <- ''
		
		# line query
		if(is.null(invalid$name)) {
			output <- paste("Line ", invalid$startLine[i], ", ", sep='')
		}
		else {	# lineage query
			# variable/object name
			output <- paste(invalid$name[i], ", ", sep='')
			
			# valType
			if(!is.null(invalid$valType) && !is.na(invalid$valType[i]))
				output <- paste(output, "with type ", invalid$valType[i], ", ", sep='')
			
			# line
			if(!is.null(invalid$startLine) && !is.na(invalid$startLine[i]))
				output <- paste(output, "on line ", invalid$startLine[i], ", ", sep='')
		}
		
		# scriptNum
		if(!is.null(invalid$scriptNum))
			output <- paste(output, "in script ", invalid$scriptNum[i], ", ", sep='')
		
		output <- paste(output, "is not a valid query.\n", sep='')
		cat(output)
	})
}

#' @noRd
.print.pos.options <- function(pos.args)
{
	cat("Options:\n")
	
	indices <- c(1:nrow(pos.args))
	
	# name column is null -> line number query
	if(is.null(pos.args$name)) {
		sapply(indices, function(i) {
			cat(paste("line", pos.args$startLine[i], 
					  "in script", pos.args$scriptNum[i], "\n"))
		})
	}
	else {	# lineage query
		sapply(indices, function(i) {
			cat(paste(pos.args$name[i], 
					  "on line", pos.args$startLine[i], 
					  "in script", pos.args$scriptNum[i], "\n"))
		})
	}
}
