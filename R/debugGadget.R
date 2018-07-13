debug.gadget <- function() {

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
                       choices = list("State" = TRUE,
                                      "Reference" = FALSE),
                       selected = TRUE),
          textInput(inputId = "lines",
                    label = "Enter lines to examine, separated by a comma",
                    value = "",
                    placeholder = "E.g., 1, 3:5, 10")
          ),
        fillCol(
          #vervatimTextOutput(outputId = "code"),
          tags$h4("Code"),
          uiOutput("code", container = rCodeContainer),
          tags$h4("Output"),
          verbatimTextOutput(outputId = "value")
        )
      )
    )
  )
  
  #session?
  server <- function(input, output) {
    
    reactiveInit <- reactive({
      file <- input$file$datapath
      debug.init(file)
    })
    
    # reactiveDebug <- observeEvent(input$lines, {
    reactiveDebug <- reactive({
      args <- unname(as.numeric(unlist(strsplit(input$lines, ","))))
      state <- as.logical(input$state)
      debug.from.line(args, state = state, script = 0)
    })

    output$code <- renderUI({
      input$file$name
      #name <- get.proc.nodes()[get.proc.nodes()$startLine == 2, "name"]
      #name <- name[!is.na(name)]
    })
    
    output$value <- renderPrint({
      # put message for when file hasn't been inputed yet
      reactiveInit()
      if (input$lines != "") {
        reactiveDebug()
      } else {
        print("No lines entered")
        #debug.from.line()
      }
    })
    
    observeEvent(input$done, {
      #to return last lines queried: (might be an error if there's no line entered yet)
      returnValue <- reactiveDebug()
      stopApp(returnValue)
    })
    
    observeEvent(input$cancel, {
      stopApp()
    })
  }

  # paneViewer()
  runGadget(ui, server, viewer = dialogViewer("provDebugR", width = 1000, height = 800))
}

rCodeContainer <- function(...) {
  code <- HTML(as.character(tags$code(class = "language-r", ...)))
  div(pre(code))
}


#ideas:
# find scripts in scripts folder in ddg folder
# make each line a button, click a line and it would tell you what's going on
# action button, next/previous line -- returns info about what variables are referenced on each line
# two panels, one with debug.from.line and the other with a step-through debugger
#   not sure which is more useful
# observeEvent or reactiveEvent which triggers debug.from.line()
