debugGadget <- function() {

  ui <- miniPage(
    gadgetTitleBar("provDebugR"),
    miniContentPanel(
      fillRow(
        flex = c(1, 3),
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
                       choices = list("State" = TRUE,
                                      "Reference" = FALSE),
                       selected = TRUE),
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
      args <- unname(as.numeric(unlist(strsplit(input$lines, ","))))
      state <- as.logical(input$state)
      
      debug.from.line(args, state = state, script = 0)
    })

    output$value <- renderPrint({
      # put message for when file hasn't been inputed yet
      # state
      reactiveInit()
      if (input$lines != "") {
        reactiveDebug()
      } else {
        #debug.from.line()
        print("No lines entered")
      }
    })
    
    observeEvent(input$done, {
      #to return last lines queried:
      returnValue <- reactiveDebug()
      stopApp(returnValue)
      #stopApp()
    })
    
    observeEvent(input$cancel, {
      stopApp()
    })
  }

  # paneViewer()
  runGadget(ui, server, viewer = dialogViewer("provDebugR", width = 1000, height = 800))
}
