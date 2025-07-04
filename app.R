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
                "It is separated in 2 parts:"
                )
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
