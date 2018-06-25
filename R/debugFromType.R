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

  if (length(var) <= 0) {
    warning("Please enter variable")
  }

  if (length(type) <= 0) {
    warning("Please enter type")
  }

}
