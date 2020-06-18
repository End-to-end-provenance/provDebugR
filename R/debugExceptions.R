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

# === ERROR ================================================================== #

#' Tracking the Lineage of Errors and Warnings
#' 
#' debug.error displays the backwards lineage of (the 
#' statements leading up to) an error that occurred when R code was executed.
#'
#' These functions are part of the provDebugR package.  To use them, you must
#' first initialise the debugger using
#' one its initialisation functions: \code{\link{prov.debug}}, 
#' \code{\link{prov.debug.file}}, or \code{\link{prov.debug.run}}.
#' 
#' The lineage is represented with a data frame that shows the R statements
#' whose execution led to the error or warning.  More specifically, 
#' each row of the data frame corresponds to one line of code.
#' The columns of the data frame are:
#' \itemize{
#'		\item scriptNum: The script number the statement is from.
#'  	\item scriptName: The name of the script the statement is from.
#'		\item startLine: The line number for the statement  If the statement spans multiple
#'		  lines, this will be the first line.
#'		\item code: The statement itself.  If the statement is long, this
#'		  will just be the start of the statement.
#' }
#'
#' @param stack.overflow If TRUE, the error message will be searched for on Stack Overflow. 
#'
#' @return debug.error returns a data frame representing the backwards lineage 
#' of the error in the execution, if any.
#'
#' @seealso provDebugR Initialisation Functions: 
#' @seealso \code{\link{prov.debug}}
#' @seealso \code{\link{prov.debug.file}} 
#' @seealso \code{\link{prov.debug.run}}
#'
#' @seealso Other provDebugR Functions (non-initialisation):
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
#' @seealso \code{\link{debug.variable}}: Returns a data frame showing all instances
#'              of the variable(s) queried.
#' @seealso \code{\link{debug.view}}: Opens and displays the contents of each file or variable
#'              or variable queried.
#'
#' @examples
#' \dontrun{
#' prov.debug.run("test.R")
#'
#' debug.error()
#' debug.error(stack.overflow = TRUE)
#'
#' debug.warning(1)
#' debug.warning(2,3)
#' debug.warning(all = TRUE)
#' }
#'
#' @export
#' @rdname debug.exceptions
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
	lineage <- .get.output.lineage(lineage)
	
	# query Stack Exchange API
	if(stack.overflow)
	{
		# display lineage before connecting to StackOverflow
		cat("Code that led to error message:\n\n")
		print(lineage)
		cat("\n")
		
		tryCatch({
			.search.stackoverflow(message)
		},
		error = function(e){
			stop(e$message, call. = FALSE)
		})
	}
	
	# return
	cat("Code that led to error message:\n\n")
	return(lineage)
}

#' Searches the error on Stack Overflow.
#' The user may choose a number between 1 and 6 (inclusive) and the corresponding
#' Stack Overflow page will be opened.
#'
#' @param search.query The error message.
#' @param order A parameter to query StackExchange api. Determines how the results are ordered.
#' @param sort A parameter to query StackExchange api. Determines how the results are sorted.
#' @param tagged A parameter to query StackExchange api.
#'
#' @return N/A
#' @noRd
.search.stackoverflow <- function(search.query, order = "desc", sort = "votes", tagged = "r") 
{
	# process the error into a form that can be used to query StackOverflow
	query <- .process.error(search.query)

	# queries StackOverflow
	result <- .query.stackoverflow(query, order = order, sort = sort, tagged = tagged)
	
	# If there are no results, try again with a query that has quotes retained
	# this is useful for errors like:
	# invalid 'type' (character) of argument
	# object 'b' not found
	if(length(result) == 0) {
		query <- .process.error(search.query, remove.quotes = FALSE)
		result <- .query.stackoverflow(query, order = order, sort = sort, tagged = tagged)
	}
	
	# If, at this point, there are still no results, stop.
	if(length(result) == 0)
		stop("No results from Stack Overflow.")
	
	# USER INPUT
	# Grab the titles and links to the questions
	pos.urls <- result[, c("title", "link")]
	
	# decode html characters in the titles, if any
	pos.urls$title <- unname(sapply(pos.urls$title, textutils::HTMLdecode))

	# This serves as a "menu" of sorts since it will print the row number
	# of each title
	cat("\nResults from StackOverflow:\n")
	print(pos.urls[, "title"])

	# They can either choose none or an index that will be matched to a row
	cat("\nChoose a numeric value that matches your error the best or q to quit: \n")
	chosen.result <- tolower(trimws(readline()))

	while(chosen.result != "q") 
	{
		chosen.result <- suppressWarnings(as.integer(chosen.result))

		# The input needs to be an integer so it can be used to
		# index into the rows of the data frame
		if(is.na(chosen.result) || (chosen.result > 6 || chosen.result < 1)) {
			cat("Invalid Input. Please choose an option between 1 - 6 or q to quit.\n")
		}
		# Open up the requested link in the default web browser
		else {
			utils::browseURL(pos.urls[chosen.result ,]$link)
		}
		
		chosen.result <- tolower(trimws(readline()))
	}
}

#' Queries StackExchange api.
#' A helper function to .search.stackoverflow .
#'
#' @param search.query The error message, in a form that can be used to query StackExchange api.
#' @param order A parameter to query StackExchange api. Determines how the results are ordered.
#' @param sort A parameter to query StackExchange api. Determines how the results are sorted.
#' @param tagged A parameter to query StackExchange api.
#'
#' @return The results from StackExchange api, if any.
#' @noRd
.query.stackoverflow <- function(search.query, order = "desc", sort = "votes", tagged = "r")
{
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
	raw.result <- httr::GET(url = url, path = utils::URLencode(path))
	
	# A 200 status code is a success, an unsuccesful code would be something
	# like 400, 404, etc
	if(raw.result$status_code != 200) {
		stop("Connection to Stack Overflow did not succeed.", call. = FALSE)
	}

	# parse the content
	result <- jsonlite::fromJSON(rawToChar(raw.result$content))
	return(utils::head(result$items))
}

#' Processes the error message into a form that can be used to query StackExchange api.
#'
#' @param error.message The error message.
#' @param remove.quotes If TRUE, elements surrounded by quotes will be removed.
#'
#' @return The error message, in a form that can be used to query StackExchange api.
#' @noRd
.process.error <- function(error.message, remove.quotes = TRUE)
{
	split <- strsplit(error.message, ":")[[1]]

	# Error messages from the prov.json will
	# typically have an uneeded prefix followed
	# by a colon ":"
	if(length(split) > 1) {
		error.message <- split[-1]
	}

	# This complicated mess of regex i=actually checks for 4 things (all inclusive):
	# Matches to characters surronded by quotes "dog"
	# Matches to characters surronded by escaped quotes \"dog\"
	# Matches to characters surronded by single quotes 'dog'
	# Matches to characters surronded by escaped quotes \'dog\'
	if(remove.quotes)
	{
		exp <- "\\\"[^\"\r]*\\\"|\"[^\"\r]*\"|\'[^\"\r]*\'|\\\'[^\"\r]*\\\'"
		error.message <- gsub(exp, "", error.message, perl = T)
	}
	
	# remove whitespace from beginning and end
	exp <- '^[[:space:]]*|[[:space:]]*$'
	error.message <- gsub(exp, "", error.message)

	return(error.message)
}

# === WARNING ================================================================ #

#' Debugging Errors and Warnings
#'
#' debug.warning displays the backwards lineage of (the 
#' statements leading up to) one or more warnings that occurred when R code was executed.
#'
#' @param ... The warning(s) to be queried.
#' @param all If TRUE, the lineages of all warnings are returned.
#'
#' @return debug.warning returns a list of data frames of lineages for the queried 
#'         warnings.
#'
#' @export
#' @rdname debug.exceptions
debug.warning <- function(..., all = FALSE)
{
	# case: no provenance
	if(!.debug.env$has.graph)
		stop("There is no provenance.")
	
	# get all warning nodes
	warning.nodes <- .debug.env$data.nodes
	warning.nodes <- warning.nodes[warning.nodes$name == "warning.msg", 
								   c("id", "value")]
	
	# case: no warnings
	if(nrow(warning.nodes) == 0) {
		cat("There are no warnings in this script.")
		return(invisible(NULL))
	}
	
	# get valid queries
	num.warnings <- 1:nrow(warning.nodes)
	row.names(warning.nodes) <- num.warnings
	
	valid.queries <- .get.valid.query.warn(warning.nodes, ..., all = all)
	
	# case: no valid queries
	if(is.null(valid.queries))
		return(invisible(NULL))
	
	# return lineage for each valid query
	output <- lapply(valid.queries$id, function(id) {
		return(.get.output.lineage(.get.lineage(id)))
	})
	
	names(output) <- row.names(valid.queries)
	return(output)
}

#' Returns a table of valid warning queries.
#' columns: id, value
#'
#' @param warning.nodes Table of all possible warning nodes.
#' @param ... The user's query.
#' @param all If TRUE, automatically returns the table of all warning nodes.
#'
#' @return A data frame of valid warning queries.
#'         columns: id, value
#' @noRd
.get.valid.query.warn <- function(warning.nodes, ..., all = FALSE)
{
	if(all)
		return(warning.nodes)
	
	# get user's query
	query <- unique(.flatten.args(...))
	
	if(is.null(query)) {
		.print.pos.warnings(warning.nodes$value)
		return(NULL)
	}
	
	# identify cells which contain valid queries
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
	
	# extract valid queries
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

#' Prints the given data frame of all possible warning nodes to standard output.
#'
#' @param warning.nodes The data frame of warning nodes.
#'
#' @return N/A
#' @noRd
.print.pos.warnings <- function(warning.nodes)
{
	.print.pos.options(warning.nodes)
	cat("\nPass the corresponding numeric value to the function for info on that warning.\n")
}
