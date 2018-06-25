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
#' debug.from.type(..., ...)
#' ...
#' }


debug.from.type <- function(var, type) {
  # if either parameter is left blank, return an error message

  data.nodes <- get.data.nodes()
  proc.nodes <- get.proc.nodes()
  proc.data <- get.proc.data()

  labels <- data.nodes[data.nodes$name == var, "label"]
  var.types <- data.nodes[data.nodes$name == var, "valType"]
  var.types <- lapply(var.types, jsonlite::fromJSON)

  is.type.match <- function(var.type) {
    var.type <- unlist(var.type)
    if (var.type["type"] == type) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  type.logicals <- unlist(lapply(var.types, is.type.match))
  labels <- labels[type.logicals]

  label.rows <- lapply(labels, function(label) {
    # Get line number and code from corresponding procedure node
    proc.node <- proc.data[proc.data$entity == label, "activity"]
    line <- proc.nodes[proc.nodes$label == proc.node, "startLine"]
    name <- proc.nodes[proc.nodes$label == proc.node, "name"]

    label.row <- c(line, name)
  })

  label.df <- as.data.frame(do.call(rbind, label.rows), stringsAsFactors = F)
  colnames(label.df) <- c("line", "code")

  return(label.df)
}
