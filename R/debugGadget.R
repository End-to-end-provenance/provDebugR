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
                    label = "Enter lines to examine, separated by a comma or colon (for range)",
                    value = "",
                    placeholder = "E.g., 1, 3:5, 10")
          ),
        fillCol(
          verbatimTextOutput(outputId = "value")
          #uiOutput("document", container = rCodeContainer)
        )
      )
    )
  )
  
  #session?
  server <- function(input, output) {
    
    reactiveInit <- reactive({
      file <- input$file$datapath
      debug.init(file)
    
      # interesting - automatically tries to run on line 0
      # environment issue?
    })
    
    reactiveDebug <- reactive({
      args <- as.numeric(unlist(strsplit(input$lines, ",")))
      state <- input$state
      debug.from.line(args, state, script = 0)
    })

    output$value <- renderPrint({
      # put message for when file hasn't been inputed yet
      # state
      reactiveInit()
      if (input$lines != "") {
        reactiveDebug()
      } else {
        print("No lines entered")
      }
    })
    
    observeEvent(input$done, {
      #to return last lines queried:
      #returnValue <- reactiveDebug()
      #stopApp(returnValue)
      stopApp()
    })
    
    observeEvent(input$cancel, {
      stopApp()
    })
  }

  # paneViewer()
  runGadget(ui, server, viewer = dialogViewer("provDebugR", width = 1000, height = 800))
}
