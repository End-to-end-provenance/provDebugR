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

# === UTILITY ================================================================ #

#' Collapses the given parameters into a single list.
#'
#' @param ... The parameteres to be collapsed.
#'
#' @return The given parameters, collapsed into a single list.
#' @noRd
.flatten.args <- function(...)
{
	# call substitute to obtain parse trees for every argument, bound into a list
	args <- substitute(...())
	
	args <- lapply(args, function(arg)
	{
		# if arg is a symbol (a variable), deparse and return
		if(is.symbol(arg))
			return(deparse(arg))
		
		# if arg is a function call, evaluate and return
		# make sure the evaluate environment is one which called
		# .flatten.args to begin with or it won't find the objects
		# to evaluate
		if(is.call(arg))
			return(eval(arg, envir = parent.frame(n=3)))
		
		# otherwise, return argument as is
		return(arg)
	})
	
	return(unlist(args))
}

#' Extract all possible variables from the given data nodes table.
#' Data nodes must have type = "Data" or "Snapshot" to be considered a variable.
#'
#' @param data.nodes The data nodes table.
#'
#' @return Table of variables extracted from the given data nodes table.
#' @noRd
.extract.vars <- function(data.nodes)
{
	if(nrow(data.nodes) == 0)
		return(data.nodes)
	
	data.nodes <- data.nodes[data.nodes$type == "Data" | data.nodes$type == "Snapshot", ]
	data.nodes <- .remove.na.rows(data.nodes)
	
	return(data.nodes)
}

#' Combine a list of data frames into a single data frame.
#'
#' @param list The list of data frames to be combined.
#'			   All data frames in this list must have the same columns.
#'			   Guarenteed to have at least one element.
#'
#' @return The combined data frame.
#' @noRd
.form.df <- function(list)
{	
	# get column names
	col.names <- names(list[[1]])
	
	# form data frame
	col.length <- 1:length(col.names)
	
	cols <- lapply(col.length, function(i) 
	{
		return(.flatten.args(mapply(`[[`, list, i)))
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
#'
#' @return The argument as an integer, NA, or NULL if the argument is not an integer.
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
#'
#' @return The index of the given list where the number falls right after.
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
#'
#' @return The associated procedure node id, or a list of id if there are multiple.
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
#'
#' @return The data frame with NA rows, if any, removed. Could have no rows.
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
#'
#' @return The list with NULL elements are removed. Could be empty.
#' @noRd
.remove.null <- function(list)
{
	nulls <- sapply(list, is.null)
	return(list[!nulls])
}

#' Prints the table or list of possible options to standard output.
#'
#' @param pos.args The table or list of possible options.
#'
#' @return N/A
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


#' Print the number associated with each script.
#'
#' @return number of scripts present
#' @noRd
.print.script.nums <- function() {
  # get scripts
  scripts <- provParseR::get.scripts(.debug.env$prov)
  num.scripts <- nrow(scripts)
  
  # if there are multiple scripts, print their associated script numbers
  if (num.scripts > 1) {
    cat("Script Numbers:\n")
    lapply(c(1:num.scripts), function(i) {
      cat(paste(i, "\t", scripts$script[i], "\n", sep=""))
    })
    
    cat("\n")
  }
  
  return(num.scripts)
}