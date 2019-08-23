# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2018.

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

#' Lines where variable is a certain type
#'
#' Given a variable and a type, this function will return a list
#' of the lines where the inputted variable is that type.
#'
#' @param var The variable to examine.
#' @param type The type to track.
#' @return A list of all the lines where the inputted variable is
#' the inputted type.
#' @export
#' @examples
#' \dontrun{
#' debug.from.type("x", "numeric")
#' debug.from.type("df1", "character")
#' }

debug.from.type <- function(var, type) {

  
  # EF EDITS
  val.types <- provParseR::get.val.type(.debug.env$prov)
  
  
  

  # Load data from parser
  data.nodes <- provParseR::get.data.nodes(.debug.env$prov)
  proc.nodes <- provParseR::get.proc.nodes(.debug.env$prov)
  proc.data <- provParseR::get.proc.data(.debug.env$prov)
  
  # Find variable entities
  labels <- data.nodes[data.nodes$name == var, "id"]

  # Extract type information
  var.types <- data.nodes[data.nodes$name == var, "valType"]
  var.types <- lapply(var.types, jsonlite::fromJSON)

  is.type.match <- function(var.type) {
    if (type %in% unlist(var.type)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # Remove instances of the variable that are the wrong type
  type.logicals <- unlist(lapply(var.types, is.type.match))
  labels <- labels[type.logicals]

  if (length(labels) == 0) {
    print("There are no instances of this variable as this type")
  } else {
    # Create a list of rows
    label.rows <- lapply(labels, function(label) {
    # Get line number and code from corresponding procedure node
      proc.node <- proc.data[proc.data$entity == label, "activity"]
      line <- proc.nodes[proc.nodes$id == proc.node, "startLine"]
      name <- proc.nodes[proc.nodes$id == proc.node, "name"]

      label.row <- c(line, name)
    })

    # Create data frame
    label.df <- as.data.frame(do.call(rbind, label.rows), stringsAsFactors = F)
    colnames(label.df) <- c("line", "code")

    return(label.df)
  }
}
