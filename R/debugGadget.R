debug.gadget <- function() {
  
  ui <- miniPage(
    gadgetTitleBar("Debug with Provenance"),
    miniTabstripPanel(
      miniTabPanel("From Line",
                   miniContentPanel(
                     h4("Examine the variables in a script on a line-by-line basis"),
                     hr(),
                     stableColumnLayout(
                       fileInput(inputId = "file",
                                 label = "Choose R script or prov-JSON file",
                                 accept = c(".R", 
                                            ".Rmd", 
                                            ".json")),
                       radioButtons(inputId = "state",
                                    label = "State or reference",
                                    choices = list("State" = TRUE, 
                                                   "Reference" = FALSE),
                                    selected = TRUE),
                       textInput(inputId = "lines",
                                 label = "Enter lines to examine, separated by a comma",
                                 value = "",
                                 placeholder = "E.g., 1, 3:5, 10")),
                     hr(),
                     verbatimTextOutput(outputId = "lineValue")),
                   icon = icon("align-justify")),
      miniTabPanel("Lineage",
                   miniContentPanel(
                     h4("Examine the lineage, either forward or backward, of variables"),
                     hr(),
                     stableColumnLayout(
                       radioButtons(inputId = "forward",
                                    label = "Forward or backward",
                                    choices = list("Forward" = TRUE,
                                                   "Backward" = FALSE),
                                    selected = FALSE),
                       textInput(inputId = "variables",
                                 label = "Enter variables to examine, separated by a comma",
                                 value = "",
                                 placeholder = "E.g., ...")),
                     hr(),
                     verbatimTextOutput(outputId = "posVariables"),
                     verbatimTextOutput(outputId = "lineageValue")),
                   icon = icon("arrows-v")),
      miniTabPanel("Variable Type",
                   miniContentPanel(
                     h4("Examine the types of variables"),
                     hr(),
                     stableColumnLayout(
                       radioButtons(inputId = "just.logical",
                                    label = "Just logical",
                                    choices = c("On" = TRUE,
                                                "Off" = FALSE),
                                    selected = FALSE),
                       textInput(inputId = "vars",
                                 label = "Enter variables to examine, separated by a comma",
                                 value = "",
                                 placeholder = "E.g., ...")),
                     hr(),
                     verbatimTextOutput(outputId = "posVars"),
                     verbatimTextOutput(outputId = "typeValue")),
                   icon = icon("code")),
      miniTabPanel("Error Trace",
                   miniContentPanel(
                     h4("Examine the lineage of an error and query stack overflow"),
                     hr(),
                     stableColumnLayout(
                       # actionButton(inputId = "error",
                       #              label = "Trace error"),
                       radioButtons(inputId = "stack.overflow",
                                    label = "Stack overflow",
                                    choices = c("On" = TRUE,
                                                "Off" = FALSE),
                                    selected = FALSE)),
                     hr(),
                     verbatimTextOutput(outputId = "trace")),
                   icon = icon("exclamation-circle"))
      )
    )

  #session?
  server <- function(input, output) {

    # Initialize debugger
    reactiveInit <- reactive({
      # put message for when file hasn't been inputed yet
      file <- input$file$datapath
      debug.init(file)
    })

    ####################################################################
    ## Debug From Line
    ####################################################################
    # reactiveLine <- observeEvent(input$lines, {
    reactiveLine <- reactive({
      args <- unname(as.numeric(unlist(strsplit(input$lines, ","))))
      state <- as.logical(input$state)
      debug.from.line(args, state = state, script = 0)
    })

    output$lineValue <- renderPrint({
      reactiveInit()
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
      args <- unname(as.character(unlist(strsplit(input$variables, ","))))
      forward <- as.logical(input$forward)
      debug.lineage(args, forward = forward)
    })
    
    output$posVariables <- renderPrint({
      debug.lineage()
    })
    
    output$lineageValue <- renderPrint({
      if (input$variables != "") {
        reactiveLineage()
      } else {
        print("No variables entered")
      }
      #input$variables
      #args
    })
    
    ####################################################################
    ## Debug Variable Type
    ####################################################################
    reactiveType <- reactive({
      args <- input$vars
      just.logical <- as.logical(input$just.logical)
      debug.variable.type(args, just.logical = just.logical)
    })
    
    output$posVars <- renderPrint({
      debug.variable.type()
    })
    
    output$typeValue <- renderPrint({
      # if (input$vars != "") {
      #   reactiveType()
      # } else {
      #   print("No variables entered")
      # }
      input$vars
    })
    
    ####################################################################
    ## Debug Error Trace
    ####################################################################
    reactiveError <- reactive({
      stack.overflow <- as.logical(input$stack.overflow)
      debug.error.trace(stack.overflow = stack.overflow)
    })
    
    output$trace <- renderPrint({
      #observeEvent(input$error, {
      #  reactiveError()
      #})
      input$error
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

  # paneViewer()
  runGadget(ui, server, viewer = dialogViewer("provDebugR", width = 1000, height = 800))
}

# from rstudio/addinexamples repo
stableColumnLayout <- function(...) {
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

#######################################################################################################

# uiOutput("code", container = rCodeContainer),

# output$code <- renderUI({
#   input$file$name
#   #name <- get.proc.nodes()[get.proc.nodes()$startLine == 2, "name"]
#   #name <- name[!is.na(name)]
# })

# rCodeContainer <- function(...) {
#   code <- HTML(as.character(tags$code(class = "language-r", ...)))
#   div(pre(code))
# }

#ideas:
# find scripts in scripts folder in ddg folder
# make each line a button, click a line and it would tell you what's going on
# action button, next/previous line -- returns info about what variables are referenced on each line
# two panels, one with debug.from.line and the other with a step-through debugger
#   not sure which is more useful
# observeEvent or reactiveEvent which triggers debug.from.line()

# tabs: from line, lineage, variable type, error
