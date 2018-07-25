debug.gadget <- function() {
  
  ui <- miniPage(
    gadgetTitleBar("Debug with Provenance"),
    miniTabstripPanel(
      miniTabPanel("Initialize",
                   miniContentPanel(
                     h4("Initialize and check error lineage"),
                     helpText("Select a file on which to initialize the debugger."),
                     hr(),
                     fileInput(inputId = "file",
                               label = "Choose R script or prov-JSON file",
                               accept = c(".R", 
                                          ".Rmd", 
                                          ".json")),
                     h5("Debug Init"),
                     verbatimTextOutput(outputId = "init",
                                        placeholder = TRUE),
                     h5("Error Trace"), 
                     verbatimTextOutput(outputId = "trace",
                                        placeholder = TRUE)),
                   icon = icon("power-off")),
      miniTabPanel("From Line",
                   miniContentPanel(
                     h4("Examine the variables in a script on a line-by-line basis"),
                     helpText(em("Reference"), " refers to the variables referenced on a line.",
                              em("State"), " refers to the state of variables in a script up to
                              that line in execution."),
                     hr(),
                     .stableColumnLayout(
                       textInput(inputId = "lines",
                                 label = "Enter lines to examine, separated by a comma",
                                 value = "",
                                 placeholder = "E.g., 1, 3:5, 10"),
                       radioButtons(inputId = "state",
                                    label = "State or reference",
                                    choices = list("State" = TRUE, 
                                                   "Reference" = FALSE),
                                    selected = FALSE)),
                     hr(),
                     verbatimTextOutput(outputId = "lineValue")),
                   icon = icon("align-justify")),
      miniTabPanel("Lineage",
                   miniContentPanel(
                     h4("Examine the lineage, either forward or backward, of variables"),
                     helpText("Lineage is the connection between variables. It can be used
                              to identify the steps and values used to create particular 
                              data values. ", em("Forward"), " lineage reveals the effects 
                              that a variable has on the rest of the script. ", em("Backward"), 
                              " lineage reveals what went into creating a variable."),
                     hr(),
                     .stableColumnLayout(
                       textInput(inputId = "variables",
                                 label = "Enter variables to examine, separated by a comma",
                                 value = "",
                                 placeholder = "E.g., ..."),
                       radioButtons(inputId = "forward",
                                    label = "Forward or backward",
                                    choices = list("Forward" = TRUE,
                                                   "Backward" = FALSE),
                                    selected = FALSE)),
                     hr(),
                     verbatimTextOutput(outputId = "posVariables"),
                     verbatimTextOutput(outputId = "lineageValue")),
                   icon = icon("arrows-v")),
      miniTabPanel("Variable Type",
                   miniContentPanel(
                     h4("Examine changes in the types of variables"),
                     helpText("If true, ", em("just logical"), " returns whether the variable type
                              has changed. If false, it returns the variable's type(s) throughout
                              execution."),
                     hr(),
                     .stableColumnLayout(
                       textInput(inputId = "vars",
                                 label = "Enter variables to examine, separated by a comma",
                                 value = "",
                                 placeholder = "Ex: x, df"),
                       radioButtons(inputId = "just.logical",
                                    label = "Just logical",
                                    choices = c("True" = TRUE,
                                                "False" = FALSE),
                                    selected = FALSE)
                       ),
                     hr(),
                     verbatimTextOutput(outputId = "posVars"),
                     verbatimTextOutput(outputId = "typeValue")),
                   icon = icon("code"))
      )
    )

  server <- function(input, output) {

    ####################################################################
    ## Debug Init and Debug Error Trace
    ####################################################################
    # Initialize debugger
    reactiveInit <- reactive({
      if (is.null(input$file$datapath)) {
        file <- NA
      } else {
        file <- input$file$datapath
      }
      debug.init(file)
    })
    
    output$init <- renderPrint({
      reactiveInit()
    })
    
    # Trace error lineage, if there is an error
    reactiveError <- reactive({
      debug.error.trace(stack.overflow = FALSE)
    })
    
    output$trace <- renderPrint({
      reactiveError()
    })

    ####################################################################
    ## Debug From Line
    ####################################################################
    reactiveLine <- reactive({
      args <- unname(as.numeric(unlist(strsplit(input$lines, ","))))
      state <- as.logical(input$state)
      debug.from.line(args, state = state, script = 0)
    })

    output$lineValue <- renderPrint({
      if (input$lines != "") {
        reactiveLine()
      } else {
        print("No lines entered")
      }
    })
    
    ####################################################################
    ## Debug Lineage
    ####################################################################
    reactiveLineage <- reactive({
      args <- unlist(strsplit(gsub(" ", "", input$variables), ","))
      forward <- as.logical(input$forward)
      debug.lineage(args, forward = forward)
    })
    
    # Display variable options
    output$posVariables <- renderPrint({
      debug.lineage()
    })
    
    output$lineageValue <- renderPrint({
      if (input$variables != "") {
        reactiveLineage()
      } else {
        print("No variables entered")
      }
    })
    
    ####################################################################
    ## Debug Variable Type
    ####################################################################
    reactiveType <- reactive({
      args <- unlist(strsplit(gsub(" ", "", input$vars), ","))
      just.logical <- as.logical(input$just.logical)
      debug.variable.type(args, just.logical = just.logical)
    })
    
    # Display variable options
    output$posVars <- renderPrint({
      debug.variable.type()
    })
    
    output$typeValue <- renderPrint({
      if (input$vars != "") {
        reactiveType()
      } else {
        print("No variables entered")
      }
    })

    ####################################################################
    ## Cancel/Done Handling
    ####################################################################
    observeEvent(input$done, {
      #to return last lines queried: (might be an error if there's no line entered yet)
      returnValue <- reactiveLine()
      stopApp(returnValue)
    })

    observeEvent(input$cancel, {
      stopApp()
    })
  }

  runGadget(ui, server, viewer = dialogViewer("provDebugR", width = 1000, height = 800))
}

# from rstudio/addinexamples repo
.stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  fluidRow(
    lapply(dots, function(el) {
      div(class = class, el)
    })
  )
}
