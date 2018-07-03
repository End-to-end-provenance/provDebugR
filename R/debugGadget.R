library(shiny)
library(miniUI)

debugGadget <- function(inputValue) {

  ui <- miniPage(
    gadgetTitleBar("provDebugR"),
    miniContentPanel(
      fillCol(
        textInput(inputId = "lines",
                  label = "Enter the lines you would like to examine",
                  value = "",
                  placeholder = "E.g., 4, 11:18"),
        flex = 1, width = "100%", height = "100%"),
      fillCol(
        ..., # data table
        flex = 1, width = "100%", height = "100%")
      # Define layout, inputs, outputs
      # output - data table
      # renderDataTable(expr, env = debug.env)
      # expr - call to function (one of the options in the scroll down menu)
    )
  )

  # input format depends on the function
  # maybe the gadget shouldn't depend on the functions as separate
  # the user won't make calls to the function directly
  # they'll be able to select some parameter (lines, variable, type) and examine it from that perspective

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.


  }

  runGadget(ui, server, viewer = dialogViewer("provDebugR"))
}
