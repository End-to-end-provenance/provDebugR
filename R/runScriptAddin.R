run.script.addin <- function(input.path = NA) {
  if(is.na(input.path)) {
    input.path <- rstudioapi::getActiveDocumentContext()$path
  }
  if(input.path == "") {
    cat("No input path specified, make sure your cursor is within your script not the console")
  } else {
    debug.init(input.path)
  }
}

