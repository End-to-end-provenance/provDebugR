debugGadget <- function() {

  ui <- miniPage(
    gadgetTitleBar("provDebugR"),
    miniContentPanel(
      fillRow(
        flex = c(1, 2),
        fillCol(
          fileInput(inputId = "file",
                    label = "Choose R script or prov-JSON file",
                    accept = c(".R", 
                               ".Rmd", 
                               ".json"),
                    multiple = FALSE,
                    buttonLabel = "Browse...",
                    placeholder = "No file selected"),
          radioButtons(inputId = "state",
                       label = "State or reference",
                       choices = list("State" = 1,
                                      "Reference" = 2),
                       selected = 1),
          textInput(inputId = "lines",
                    label = "Lines",
                    value = "",
                    placeholder = "Enter lines to examine")
          ),
        fillCol(
          verbatimTextOutput(outputId = "value")
        )
      )
    )
  )
  
  #session?
  server <- function(input, output) {
    
    #debug.init(input$file)
    
    output$value <- renderPrint({
      input$file
      #env = .debug.env()
      #debug.from.line(input$lines)
    })
  }
  
  # paneViewer()?
  runGadget(ui, server, viewer = dialogViewer("provDebugR"))
}
