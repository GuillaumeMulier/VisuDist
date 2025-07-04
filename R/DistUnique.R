# Script for UI in the part 1 distribution               #
# Autor : G. Mulier                                      #
# Created the 07/05/2025, modified the 02/07/2025        #

UniqueDist <- function(id) {
  
  ns <- NS(id)
  
  sidebarLayout(
    # Left zone for parameters
    sidebarPanel(
      h4("Choose a distribution"),
      selectInput(ns("ChoixDist1"), NULL, names(ListeDistribution)),
      h4("Parameters of the distribution"),
      uiOutput(ns("ParamsDist")),
      h4("Graphical parameters"),
      numericInput(ns("MinDens1"), label = "Minimum of graphical window", value = -5),
      numericInput(ns("MaxDens1"), label = "Maximum of graphical window", value = 5),
      h4("Desired probabilities"),
      p(tags$i("You can enter multiple probability queries separated by a space. For each query, use > or < followed by the threshold to get, for example, Pr(X < threshold).")),
      textInput(ns("InputThresh1"), label = NULL),
      h4("Time to get the results"),
      actionButton(ns("PlotBut1"), "Compute results")
    ),
    # Center = plot, code and results
    mainPanel(
      tabsetPanel(
        id = ns("TypePlot"),
        tabPanel("base R",
                 h4("Plot of the distribution"),
                 p(tags$i("If multiple thresholds are supplied, the plot is drawn for the first one.")),
                 plotOutput(ns("GrapheDensiteBaseR")),
                 h4("Code to reproduce the plot"),
                 verbatimTextOutput(ns("CodeDensiteBaseR"))),
        tabPanel("ggplot2",
                 h4("Plot of the distribution"),
                 p(tags$i("If multiple thresholds are supplied, the plot is drawn for the first one.")),
                 plotOutput(ns("GrapheDensiteGgplot")),
                 h4("Code to reproduce the plot"),
                 verbatimTextOutput(ns("CodeDensiteGgplot")))
      ),
      br(), br(),
      h4("Results on the asked probabilities"),
      tableOutput(ns("TableRes1")),
      verbatimTextOutput(ns("CodeRes1"))
    )
  )
  
}
