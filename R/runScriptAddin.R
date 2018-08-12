#' Serves as an interface with rstudioapi to facilitate use by RStudio users
#' 
#' This function is bound to a button that will show up undeer the RSudio addins menu.
#' For more information on addins see: https://www.rstudio.com/resources/webinars/understanding-add-ins/
#' Preferably that button will be bound to a keyboard shortcut. This will allow users
#' to hit a keyboard shortcut to run their script and collect prov
#'
#' @param input.path A path to a file to run. If not given, will pull a script from where the cursor is.
#' If the cursor has highlighted text will grab that and only run the highlighted text.
#'
#' @return nothing
#' @export
#'
#' @examples 
#' \dontrun{
#' # This function will be run by navigating to the Addins menu in RStudio
#' # and then clicking the Run w/ Debugging button. It is reccommended this
#' # button is asssigned to a keyboard shortcut. 
#' }
run.script.addin <- function(input.path = NA) {
  # The prov.dir is where the prov folder will be saved to when RDataTracker (RDT) runs.
  # If the value is NULL a default directory is used by RDT, the dir the script resides in. 
  # If the user chose to run on highlighted text, a modified version of the script will be 
  # saved in the tempdir and run. Which, by default, would cause the prov to be saved to
  # the tempdir. Therefore if they choose highlighted text, this variable will change to
  # where the original script is
  prov.dir <- NULL
  if(is.na(input.path)) {
    # If the user did not highlight text then $text == "" and the whole script will be run
    if(rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())$text == "")
    {
      input.path <- rstudioapi::getActiveDocumentContext()$path
    # If the $text was not blank, they highlighted text and RDT will run on that
    } else {
      # Grab the selection as it is used in a few different places
      selection <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())
      # The prov folder will have the range of lines appended to the script name
      line.range <- paste("[", selection$range$start[[1]], "-",
                          selection$range$end[[1]], "]", sep = "")
      # The filename is needed for saving to the tempdir
      file.name <- rstudioapi::getActiveDocumentContext()$path
      # The dir is needed for overriding where RDT will save the prov
      #prov.dir <- paste(strsplit(file.name, "\\.")[[1]][1],
                      #line.range,
                     # "prov_", sep = "")
      # This provides just the name, minus the path so we can save the same filename
      # to the tempdir. This allows script names in prov to be the same even though
      # the file is technically different.
      file.name <- basename(file.name)
      input.path <- file.path(tempdir(), file.name)
      if(selection$range$start[[1]] > 1) {
        new.lines <- rep("\n", selection$range$start[[1]] - 1)
        # Write the hgihlighted text to that file, but to keep line numbers correct add newlines
        write(paste(paste(new.lines, collapse = ""), selection$text, sep = ""), file = input.path)
      } else {
        # Write the hgihlighted text to that file
        write(selection$text, file = input.path)
      }

    }
   
  }
  # If the cursor was not in a script the path will be ""
  if(input.path == "") {
    cat("No input path specified, make sure your cursor is within your script not the console\n")
  } else {
    debug.init(input.path, dir = prov.dir)
  }
}

