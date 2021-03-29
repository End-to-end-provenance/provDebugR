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

# === TYPE CHANGES =========================================================== #

#' Tracking Type Changes
#' 
#' Returns a data frame for each variable in the execution containing the 
#' instances where the data type changed.
#' Each data frame contains the following columns:
#' \itemize{
#'		\item value: The value of the variable.
#'		\item container: The type of the container of the variable.
#'		\item dimension: The size of the container.
#'		\item type: The data type(s) contained within the container.
#'		\item code: The line of code associated with the variable.
#'		\item scriptNum: The script number associated with the variable.
#'		\item scriptName: The name of the script associated with the variable.
#'		\item startLine: The line number associated with the variable.
#' }
#'
#' debug.type.changes belongs to provDebugR, a debugger which utilises provenance 
#' collected post-execution to facilitate understanding of the execution and aid 
#' in debugging.
#'
#' This function may be used only after the debugger has been initialised using
#' one its initialisation functions (listed below).
#'
#' @param ... Optional. Variable name(s) to be queried. If variables are given (not NULL),
#'            the results will be filtered to show only those with the given variable name.
#'
#' @return A list of data frames for each variable with at least 1 data type change.
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
#' debug.type.changes()
#' debug.type.changes(x)
#' debug.type.changes("a", "b")
#' }
#'
#' @export
#' @rdname debug.type.changes
debug.type.changes <- function(...)
{
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# Get all data nodes with type "Data" or "Snapshot"
	data.nodes <- .extract.vars(.debug.env$data.nodes)
	
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
		type.changes <- 1 # start with just the first node
		
		val.type.changes <- lapply(c(2:nrow(nodes)), function(i) {
		  # if(nodes$valType[i] != nodes$valType[i-1])
		  # 	type.changes <<- append(type.changes, c(i-1, i))
		  
		  # check if the type changed in any way
		  if (nodes$valType[i] != nodes$valType[i-1]) {
		    type.changes <<- append(type.changes, i)
		    
		    val.type.changes <- .get.val.type.changes(nodes, i)
		    
		    return(val.type.changes)
		  }
		})
		
		# make one-dimensional list
		val.type.changes <- unlist(val.type.changes)
		changes <- "NA" # first value is NA - no changes to the type
		changes <- append(changes, val.type.changes)
		
		# remove duplicates
		type.changes <- unique(type.changes)
		
		if(length(type.changes) <= 1) {
			remove.indices <<- append(remove.indices, i)
			return(NULL)
		}
		
		# extract specified nodes with type changes
		nodes <- nodes[type.changes, ]
		
		return(.get.fields.type.changes(nodes, changes))
	})
	
	# remove vars without type changes
	if(length(remove.indices) > 0) {
		vars.names <- vars.names[-remove.indices]
		vars <- vars[-remove.indices]
	}
	
	names(vars) <- vars.names
	
	# if the user has specified variable(s) to be queried, get the valid ones
	# for this function, this process is much simpler than get.valid.var
	# first, collect user queries and remove repeated ones
	var <- unique(.flatten.args(...))
	
	if(!is.null(var))
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
	
	.print.type.changes(vars)
	
	return(invisible(vars))
}

#' Generates fields that will be used in user output.
#' columns: value, container, dimension, type, code, scriptNum, scriptName, startLine
#'
#' @param data.nodes The data nodes to be displayed to the user.
#' @param changes Specifies what part of the type changed.
#'
#' @return The data frame of type changes to be returned to the user.
#'         columns: value, container, dimension, type, code, scriptNum, scriptName, startLine
#' @noRd
.get.fields.type.changes <- function(data.nodes, changes)
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
		
		# extract code, scriptNum, scriptName, startLine from proc nodes
		proc.fields <- .debug.env$proc.nodes[.debug.env$proc.nodes$id == proc.id, 
											 c("name", "scriptNum", "scriptName", "startLine")]
		
		# combine fields
		fields <- cbind(data.value, val.type, proc.fields, changes[i],
		                stringsAsFactors = FALSE)
		names(fields) <- c("value", 
						   "container", "dimension", "type", 
						   "code", "scriptNum", "scriptName", "startLine", "changes")
		return(fields)
	})
	
	return(.form.df(rows))
}

#' Determines the type or types of changes that occurred for a data node at a 
#' given point in the script execution.
#'
#' @param nodes The list of data nodes for a particular variable.
#' @param i Index determining which node in the list is currently being 
#' examined.
#'    
#' @return A character string containing some combination of c, d, and t for
#' container, dimension, and type-related changes, respectively.
#'         
#' @noRd
.get.val.type.changes <- function(nodes, i) {
  # initialize values to false (no changes occurred)
  val.type.changes <- c()
  
  # split the valType into its components
  val.type.current <- provParseR::get.val.type(.debug.env$prov, node.id = nodes$id[i])
  val.type.prev <- provParseR::get.val.type(.debug.env$prov, node.id = nodes$id[i - 1]) 
  # TODO more efficient way to do this?
  
  # check if the change was from container
  if (.get.val.type.changes.helper(val.type.current, val.type.prev, "container"))
    val.type.changes <- paste(val.type.changes, "c", sep = "")
  
  # check if change was from dimension
  if (.get.val.type.changes.helper(val.type.current, val.type.prev, "dimension"))
    val.type.changes <- paste(val.type.changes, "d", sep = "")
  
  # check if change was from type
  if (.get.val.type.changes.helper(val.type.current, val.type.prev, "type"))
    val.type.changes <- paste(val.type.changes, "t", sep = "")
  
  # cat("the changes are in: ")
  # print(val.type.changes)
  
  return(val.type.changes)
}

#' A helper function for .get.val.type.changes that checks if a given type
#' change occurred.
#'
#' @param val.type.current The valType of the node currently being examined. 
#' @param val.type.prev The valType of the previous node, compared to the 
#' current one to determine if any changes occurred.
#' @param component The part of the valType being compared between the two 
#' nodes. Can be container, dimension, or type.
#'    
#' @return TRUE if the component changed and FALSE otherwise.
#'         
#' @noRd
.get.val.type.changes.helper <- function(val.type.current, val.type.prev, component) {
  changed <- FALSE
  
  # print(paste(val.type.current[[component]], val.type.prev[[component]]))
  
  if (is.na(val.type.current[[component]]) || is.na(val.type.prev[[component]])) {
    # if they are not both NA, then a component change occurred
    if (!(is.na(val.type.current[[component]]) && is.na(val.type.prev[[component]]))) {
      changed <- TRUE
    }
  }
  else if (val.type.current[[component]] != val.type.prev[[component]]) {
    # cat("we got inside\n")
    
    # a change occurred and neither is NA
    changed <- TRUE
  }
  
  return(changed)
}

#' Prints user output.
#'
#' @param type.changes The list of all changes that occurred in the script.
#' @return String that contains all type changes in easy to read format.
#'         
#' @noRd
.print.type.changes <- function(type.changes) {
  # loop through each element, printing relevant information
  lapply(c(1:length(type.changes)), function(i) {
    var <- type.changes[[i]]
    
    cat(paste("The type of variable ", names(type.changes[i]), " has changed. ",
              names(type.changes[i]), " was declared on line ", var$startLine[1],
              " in ", var$scriptName[1], ".\n", sep = ""))
    
    lapply(c(2:nrow(var)), function(j) {
      cat(paste("\t", j-1, ": ", var$scriptName[j], ", line ", 
                var$startLine[j], "\n", sep = ""))
      
      # if there were container changes, print
      if (.are.changes("c", var$changes[j])) {
        cat(paste("\t\tcontainer changed to: ", var$container[j],
                  "\n\t\tfrom:\t\t",  "      ", var$container[j-1], "\n",
                  sep = ""))
      }
      
      # if there were dimension changes, print
      if (.are.changes("d", var$changes[j])) {
        cat(paste("\t\tdimension changed to: ", var$dimension[j],
                  "\n\t\tfrom:\t\t", "      ", var$dimension[j-1], "\n",
                  sep = ""))
      }
      
      # if there were type changes, print
      if (.are.changes("t", var$changes[j])) {
        cat(paste("\t\telement type changed to: ", var$type[j],
                  "\n\t\tfrom:\t\t\t", " ", var$type[j-1], "\n",
                  sep = ""))
      }
      
      if (nchar(var$code[j]) > 50)
        cat(paste("\t\tcode excerpt:", substring(var$code[j], 1, 47), "...\n"))
      else
        cat(paste("\t\tcode excerpt:", var$code[j], "\n"))
    })
  })
}

#' A helper function that tests if a certain type change occurred.
#'
#' @param invalid.names The list of any variables in the script with invalid 
#' names.
#' 
#' @return TRUE if change.type occurred, FALSE if not.
#'         
#' @noRd
.are.changes <- function(change.type, changes.value) {
  are.changes <- FALSE
  
  # check if there was this type of change
  if (!identical(grep(change.type, changes.value), integer(0)))
    are.changes <- TRUE
  
  return(are.changes)
}