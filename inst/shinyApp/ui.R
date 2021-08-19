ui = shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(),
    #The sidebar is where the selection of the randomization method occurs.
    # Using radio buttons to simplify the code for choosing the setup.
  shinydashboard::dashboardSidebar(

      # Input: If Response Variable == Mean, then select a data file to open ----
      shiny::fileInput(inputId = "data_file", label = "Choose CSV File",multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      shiny::radioButtons(inputId = "responseType", label = h3("Response Type"), inline = TRUE,
                   choices = c("Quantitative", "Categorical"), select = "Quantitative"),
      shiny::selectInput(inputId = "response_var", label = "Response Variable", choices = "NONE"),
      shiny::conditionalPanel(condition = "input.response_var != 'NONE' & input.responseType == 'Categorical'",
                              shiny::radioButtons(inputId = "respRefGroup", label = h3("Reference Group"), inline = TRUE,
                                    choices = c("One", "Two"), select = "Two")
      ),
      shiny::selectInput(inputId = "explain_var", label = "Explanatory Variable", choices = "NONE"),
      shiny::conditionalPanel(condition = "input.explain_var != 'NONE'",
                              shiny::radioButtons(inputId = "explRefGroup", label = h3("Reference Group"), inline = TRUE,
                                    choices = c("One", "Two"), select = "Two")
      ),
      # Horizontal line ----
      #      tags$hr(),

      # Type of Simulation
      shiny::radioButtons(inputId = "simType", label = h3("Type of Simulation"),
                   choices = list("Bootstrap Conf. Interval" = "CI", "Randomization Hyp. Test" = "TEST"))
    ),
    # This code allows the tab selections to appear in the output.
  shinydashboard::dashboardBody(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(1, offset = 0,
                      shiny::numericInput(inputId = "numberSamples", label = "Number of Samples",
                              min = 1, max = 5000, value=1)
          ),
        shiny::column(1, offset = 0,
                      dipsaus::actionButtonStyled("addMore", "+ SAMPLES", class="btn btn-success")
          ),
        shiny::column(1, offset = 0,
                      dipsaus::actionButtonStyled("resetDist", "RESET", class="btn btn-danger")
          ),
        shiny::column(2, offset = 0,
                      shiny::radioButtons(inputId = "normalOutline", label = "Normal Outline",
                              choices = c("ON" = "TRUE", "OFF" = "FALSE"),
                              selected = "FALSE", inline = TRUE)
          ),
        shiny::conditionalPanel(condition = "input.simType == 'CI'",
                                shiny::column(1, offset = 0,
                                              shiny::numericInput(inputId = "confLevel", label = "Confidence Level (%)",
                                               min=1, max = 99, value=95)
                           )
          ),
        shiny::conditionalPanel(condition = "input.simType == 'TEST'",
                                shiny::column(1, offset = 0,
                                  #                                  numericInput(inputId = "nullMean", label = "\u03bc =", value=0),
                                  shiny::numericInput(inputId = "nullMean", label = "\u03bc \u003d", value=0),
                           )
          ),
        shiny::column(4, offset = 0,
                      shiny::radioButtons(inputId = "distTail", label = "Confidence Interval Tail: Percentile Method",
                              choices = list("NONE", "Left", "Two-Tail", "Right"),
                              selected = "NONE", inline = TRUE)
          ),
        shiny::conditionalPanel(condition = "input.simType == 'TEST' & input.explain_var != 'NONE'",
                                shiny::column(2, offset = 0,
                                              shiny::radioButtons(inputId = "randomMethod", label = "Randomization Method",
                                               choices = list("Reassign", "Resample"),
                                               selected = "Reassign", inline = TRUE)
                           )
          ),
        ),

        tags$br(),

      shiny::fluidRow(
        shiny::column(8, offset = 0, shiny::plotOutput(outputId = "sampleMeanPlot",
                                           hover = hoverOpts(id = "meanPlotHover",
                                                             delayType = "debounce", delay = 25),
                                           width = "100%", height = 600)
          ),
        shiny::column(4, shiny::plotOutput(outputId = "observedPlot", width = 'auto', height = 300)
          ),
        shiny::column(4, shiny::plotOutput(outputId = "resamplePlot", width = 'auto', height = 300)
          )
        ),

        tags$br()

      )
    )
  )

