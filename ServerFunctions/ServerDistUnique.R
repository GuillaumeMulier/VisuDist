# Script pour le server de visualisation d'une distribution #
# Auteur : G. Mulier                                        #
# Créé le 07/05/2025, modifié le 07/05/2025                 #

DistUniqueServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$ChoixDist1, {
        print(ListeDistribution[[input$ChoixDist1]])
      })
    }
  )
}


