# Script pour les UI de visualisation d'une distribution #
# Auteur : G. Mulier                                     #
# Créé le 07/05/2025, modifié le 07/05/2025              #

# UI 
UniqueDist <- function(id) {
  
  ns <- NS(id)
  
  sidebarLayout(
    # Zone de gauche de sélection des paramètres des différentes lois
    sidebarPanel(
      selectInput(ns("ChoixDist1"),
                  "Choisir une distribution",
                  names(ListeDistribution))
    ),
    # Zone centrale où il y aura le graphique, les résultats et le code
    mainPanel(
      
    )
  )
  
}
