
debugGadget <- function() {
  
  ui <- miniPage(
    gadgetTitleBar("provDebugR"),
    miniContentPanel(padding = 15,
      fileInput(inputId = "file",
                label = "Choose R script or prov-JSON file",
                accept = c(
                  ".R",
                  ".Rmd",
                  ".json"),
                multiple = FALSE,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
                ),
      fillRow(textInput(inputId = "lines",
                        label = "Lines", #NULL?
                        value = "",
                        placeholder = "Enter the lines you want to examine")
              ),
      #hr(),
      fillRow(verbatimTextOutput("value")),
      tableOutput("return")
      )
  )
  
  #session?
  server <- function(input, output) {
    
    #debug.init(input$file)
    
    output$lines <- renderPrint({
      debug.from.line(input$lines)
    })
    
    output$value <- renderPrint({
      input$file#,
      #env = .debug.env()
    })
    
    output$return <- renderTable({
      #debug.from.line()
    })
  }
  
  # dialogViewer("provDebugR")?
  runGadget(ui, server, viewer = paneViewer())
}
