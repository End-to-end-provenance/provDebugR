#' Search Stack Overflow
#'
#' For more information see: https://api.stackexchange.com/docs/
#'
#' @param search.query What the user is searching for in stack overflow
#' @param order Determines the order parameter
#' @param sort Determines how to sort the results
#' @param tagged Determines which tags to search under
#'
#' @return a list of information converted from JSON
#' @importFrom utils URLencode browseURL head
#' @name debug.search
#' @examples
#' \dontrun{
#' debug.search("object not found")
#' }
#' @noRd
.debug.search <- function(search.query, order = "desc",
                    sort = "votes", tagged = "r") {

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

  # parse the content and return it to the user as a list
  jsonlite::fromJSON(rawToChar(raw.result$content))

}
