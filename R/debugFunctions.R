# === LINE =================================================================== #

debug.line <- function(..., script.num = 1, all = FALSE)
{
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

debug.variable <- function(..., val.type = NA, script.num = 1, all = FALSE)
{
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

.get.query.var <- function(..., val.type = NA, start.line = NA, script.num = 1)
{
	if(length(script.num) > 1) {
		warning("Please query only 1 script number.")
		return(NULL)
	}
	
	# TMP
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
		query.vars <- rep(query.vars, length(query.lines))
		query.types - rep(val.type, length(query.lines))
		query.lines <- start.line
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

debug.lineage <- function(..., start.line = NA, script.num = 1, all = FALSE, forward = FALSE)
{
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

# === STATE ================================================================== #

# === ERROR ================================================================== #

# === WARNING ================================================================ #

# === UTILITY ================================================================ #

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
			output <- paste("Line", invalid$startLine[i], ", ", sep='')
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
