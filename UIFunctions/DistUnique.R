# Script pour les UI de visualisation d'une distribution #
# Auteur : G. Mulier                                     #
# Créé le 07/05/2025, modifié le 01/07/2025              #

# UI 
UniqueDist <- function(id) {
  
  ns <- NS(id)
  
  sidebarLayout(
    # Zone de gauche de sélection des paramètres des différentes lois
    sidebarPanel(
      selectInput(ns("ChoixDist1"),
                  "Choose a distribution",
                  names(ListeDistribution)),
      h4("Parameters of the distribution"),
      uiOutput(ns("ParamsDist")),
      h4("Paramètres graphiques"),
      numericInput(ns("MinDens1"), label = "Minimum of graphical window", value = -5),
      numericInput(ns("MaxDens1"), label = "Maximum of graphical window", value = 5),
      h4("Time to get the results"),
      actionButton(ns("PlotBut1"), "Compute results")
    ),
    # Zone centrale où il y aura le graphique, les résultats et le code
    mainPanel(
      h4("Densité en x = 1 :"),
      verbatimTextOutput(ns("densite")),
      plotOutput(ns("GrapheDensite"))
    )
  )
  
}
