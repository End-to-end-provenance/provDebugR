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

# === LINEAGE ================================================================ #

#' The Lineage of a Variable/Data Node.
#' 
#' For each data node queried, debug.lineage returns a data frame representing
#' its forwards (how the data is used), or backwards (how the data was generated)
#' lineage.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item scriptNum: The script number the data node is associated with.
#'		\item startLine: The line number the data node is associated with.
#'		\item code: The line of code which used/produced the data node.
#' }
#'
#' debug.lineage belongs to provDebugR, a debugger which utilises provenance collected
#' post-execution to facilitate understanding of the execution and aid in debugging.
#'
#' This function may be used only after the debugger has been initialised using
#' one its initialisation functions (listed below).
#'
#' @param ... The names of data nodes to be queried.
#' @param start.line The line number of the queried data nodes. Optional.
#' @param script.num The script number of the queried data nodes. 
#'                   Defaults to script number 1 (main script).
#' @param all If TRUE, this function returns the linages of all data node names.
#' @param forward If TRUE, this function returns the forwards lineage 
#'                (how the data is used) instead of the backwards lineage
#'                (how the data was generated).
#'
#' @return A list of data frames showing the forwards or backwards lineage of all
#'         queried data nodes.
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
#' @seealso \code{\link{debug.state}}: Returns the state at the line(s) queried,
#'              after the line had been executed. The state is the list of all 
#'              variables and their values in the environment at the queried line.
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
#' debug.lineage("x")
#' debug.lineage("x", start.line = 5, script.num = 2)
#' debug.lineage("a", "b", forward = TRUE)
#' debug.lineage(all = TRUE)
#' }
#'
#' @export
#' @rdname debug.lineage
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

#' Get the lineage of the specified data node.
#' Function shared with debug.error and debug.warning .
#'
#' @param node.id The data node id.
#' @param forward If TRUE, gets the forward lineage instead of the default backwards lineage.
#'
#' @return The lineage (vector of procedure nodes, sorted by increasing procedure node id).
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

#' Get user output for lineages.
#' Function shared with debug.error and debug.warning .
#' columns: scriptNum, startLine, code
#'
#' @param id.list A vector of procedure node id (the lineage).
#'
#' @return The lineage, formatted for user output.
#'         columns: scriptNum, startLine, code
#'
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
