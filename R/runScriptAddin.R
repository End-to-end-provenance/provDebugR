run.script.addin <- function(input.path = NA) {
  ddgdir <- NULL
  if(is.na(input.path)) {
    if(rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())$text == "")
    {
      input.path <- rstudioapi::getActiveDocumentContext()$path
    } else {
      selection <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())
      line.range <- paste("[", selection$range$start[[1]], "-",
                          selection$range$end[[1]], "]", sep = "")
      file.name <- rstudioapi::getActiveDocumentContext()$path
      ddgdir <- paste(strsplit(file.name, "\\.")[[1]][1],
                      line.range,
                      "_ddg", sep = "")
      file.name <- basename(file.name)
      input.path <- file.path(tempdir(), file.name)
      write(selection$text, file = input.path)
    }
   
  }
  if(input.path == "") {
    cat("No input path specified, make sure your cursor is within your script not the console\n")
  } else {
    debug.init(input.path, dir = ddgdir)
  }
}

