
debugGadget <- function() {
  
  ui <- ...
  
  server <- function(input, output) {
    ...
  }
  
  runGadget(ui, server, viewer = dialogViewer("provDebugR"))
}