# Main script to run the application                     #
# Autor : G. Mulier                                      #
# Created the 07/05/2025, modifid the 03/07/2025         #

library(shiny)
library(bslib)
library(ggplot2)
library(phase1b)

ui <- page_navbar( 
  header = tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
      color: red;
      font-weight: bold;
    }
  "))
  ),
  nav_panel("Get started", 
            fluidPage(
              p("This application aims at computing probabilities given a known distribution (for example posterior probabilities in bayesian analysis.",
                "It is separated in 2 parts:"),
              tags$ul(
                tags$li("One distribution"),
                tags$li("Difference between 2 distributions")
              ),
              br(),
              p("For both parts, the usage is the same.",
                "You choose your distribution, then you select values for the parameters (for the difference between 2 distributions, the parameters are for X1-X2).",
                "You can adjust the graphical window for the plot and the desired probabilities that you want (Pr(X>s) for example.",
                "Click on Compute Results to display the plot and the asked probabilities.",
                "You can retrieve these results using the code displayed below the plot and table."),
              br(), br(),
              tags$i("Autor: G. Mulier")
            )
            ), 
  nav_panel("One distribution", UniqueDist("DistUnique")), 
  nav_panel("Difference between 2 distributions", DifferenceDist("DistDiff")), 
  title = "Get probabilities from distribution", 
  id = "page", 
) 

server <- function(input, output) {

  DistUniqueServer("DistUnique")
  DistDiffServer("DistDiff")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
