# Script for UI in the part difference between distributions #
# Autor : G. Mulier                                          #
# Created the 03/05/2025, modified the 03/07/2025            #

DifferenceDist <- function(id) {
  
  ns <- NS(id)
  
  sidebarLayout(
    # Left zone for parameters
    sidebarPanel(
      h4("Choose a distribution"),
      selectInput(ns("ChoixDistDiff"), NULL, names(ListeDifferences)),
      h4("Parameters of the distribution"),
      uiOutput(ns("ParamsDistDiff")),
      h4("Graphical parameters"),
      numericInput(ns("MinDensDiff"), label = "Minimum of graphical window", value = -5),
      numericInput(ns("MaxDensDiff"), label = "Maximum of graphical window", value = 5),
      h4("Desired probabilities"),
      p(tags$i(HTML("You can enter multiple probability queries separated by a space. For each query, use > or < followed by the threshold to get, for example, Pr(X<sub>1</sub> - X<sub>2</sub> < threshold)."))),
      textInput(ns("InputThreshDiff"), label = NULL),
      h4("Time to get the results"),
      actionButton(ns("PlotButDiff"), "Compute results")
    ),
    # Center = plot, code and results
    mainPanel(
      tabsetPanel(
        p(tags$i("Of note, the difference is between 2 distributions of the same type: for example if you select normal, you will get the difference between 2 normal distributions.",
                 "Moreover, we are considering both distributions independant here.")),
        id = ns("TypePlot"),
        tabPanel("base R",
                 h4("Plot of the distribution"),
                 p(tags$i("If multiple thresholds are supplied, the plot is drawn for the first one.")),
                 plotOutput(ns("GrapheDensiteBaseRDiff")),
                 h4("Code to reproduce the plot"),
                 verbatimTextOutput(ns("CodeDensiteBaseRDiff"))),
        tabPanel("ggplot2",
                 h4("Plot of the distribution"),
                 p(tags$i("If multiple thresholds are supplied, the plot is drawn for the first one.")),
                 plotOutput(ns("GrapheDensiteGgplotDiff")),
                 h4("Code to reproduce the plot"),
                 verbatimTextOutput(ns("CodeDensiteGgplotDiff")))
      ),
      br(), br(),
      h4("Results on the asked probabilities"),
      tableOutput(ns("TableResDiff")),
      verbatimTextOutput(ns("CodeResDiff"))
    )
  )
  
}
