# Script pour le server de visualisation d'une distribution #
# Auteur : G. Mulier                                        #
# Créé le 07/05/2025, modifié le 01/07/2025                 #

DistUniqueServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Reactive value of the chosen distribution
      ChosenDist <- reactive({input$ChoixDist1})
      ItemDist <- reactive({ListeDistribution[[ChosenDist()]]})
      
      # Display the parameters of the chosen distribution
      output$ParamsDist <- renderUI({
        Parametres <- ItemDist()[["params"]]
        ListeInputsDist <- lapply(names(Parametres),
                                  \(param) {
                                    numericInput(
                                      inputId = session$ns(paste0(ChosenDist(), "_", param)),
                                      label = HTML(Parametres[[param]][["label"]]),
                                      value = Parametres[[param]][["value"]],
                                      min = Parametres[[param]][["min"]],
                                      max = Parametres[[param]][["max"]]
                                    )
                                  })
        return(tagList(ListeInputsDist))
      })
      
      # Plot the density
      output$GrapheDensite <- renderPlot({
        FctDens <- ItemDist()[["dens"]]
        Parametres <- ItemDist()[["params"]]
        for (param in names(Parametres)) { # Else error because inputs not loaded that quicly
          req(input[[paste0(ChosenDist(), "_", param)]])
        }
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDist(), "_", param)]]), names(Parametres))
        TabVals <- data.frame(x = seq(input$MinDens1, input$MaxDens1, length.out = 1000))
        TabVals$Density <- do.call(FctDens, c(list("x" = TabVals$x), ValeursParametres))
        ggplot(TabVals, aes(x, Density)) +
          geom_line()
      })
      
      # Test : densité x = 1
      output$densite <- renderPrint({
        FctDens <- ItemDist()[["dens"]]
        Parametres <- ItemDist()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDist(), "_", param)]]), names(Parametres))
        return(do.call(FctDens, c("x" = 1, ValeursParametres)))
      })
      
      # observeEvent(input$ChoixDist1, {
      #   print(ListeDistribution[[input$ChoixDist1]])
      # })
    }
  )
}


