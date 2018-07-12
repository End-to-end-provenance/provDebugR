library(shiny)
library(miniUI)
library(provDebugR)

debugGadget <- function() {
  
  ui <- ...
  
  server <- function(input, output) {
    ...
  }
  
  runGadget(ui, server, viewer = dialogViewer("provDebugR"))
}