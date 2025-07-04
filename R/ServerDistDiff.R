# Script pour le server de visualisation de la différence de 2 distributions #
# Auteur : G. Mulier                                                         #
# Créé le 04/07/2025, modifié le 04/07/2025                                  #

DistDiffServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Reactive value of the chosen distribution and validity of inputs
      ChosenDistDiff <- reactive({input$ChoixDistDiff})
      ItemDistDiff <- reactive({ListeDifferences[[ChosenDistDiff()]]})
      ValidityInputsDiff <- eventReactive(input$PlotButDiff, {CheckThresh(input$InputThreshDiff)})
      
      # Display the parameters of the chosen distribution
      output$ParamsDistDiff <- renderUI({
        Parametres <- ItemDistDiff()[["params"]]
        ListeInputsDist <- lapply(names(Parametres),
                                  \(param) {
                                    numericInput(
                                      inputId = session$ns(paste0(ChosenDistDiff(), "_", param)),
                                      label = HTML(Parametres[[param]][["label"]]),
                                      value = Parametres[[param]][["value"]],
                                      min = Parametres[[param]][["min"]],
                                      max = Parametres[[param]][["max"]]
                                    )
                                  })
        return(tagList(ListeInputsDist))
      })
      
      # Dynamic generation of data
      DonneesDistDiff <- eventReactive(input$PlotButDiff, {
        FctDens <- ItemDistDiff()[["dens"]]
        Parametres <- ItemDistDiff()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDistDiff(), "_", param)]]), names(Parametres))
        TabVals <- data.frame("x" = seq(input$MinDensDiff, input$MaxDensDiff, length.out = 1000), check.names = FALSE)
        TabVals$Density <- do.call(FctDens, c(list("x" = TabVals$x), ValeursParametres))
        if (ValidityInputsDiff()[[1]]) {
          Operator <- ValidityInputsDiff()[[2]][1, 1]
          Threshold <- ValidityInputsDiff()[[2]][1, 2]
          if (Operator == "<") {
            TabValsSurf <- data.frame(x = seq(input$MinDensDiff, Threshold, length.out = 1000))
          } else {
            TabValsSurf <- data.frame(x = seq(input$MaxDensDiff, Threshold, length.out = 1000))
          }
          TabValsSurf$Density <- do.call(FctDens, c(list("x" = TabValsSurf$x), ValeursParametres))
          if (Operator == "<") {
            TabValsSurf <- rbind(TabValsSurf, data.frame("x" = c(Threshold, input$MinDensDiff), Density = 0))
          } else {
            TabValsSurf <- rbind(TabValsSurf, data.frame("x" = c(Threshold, input$MaxDensDiff), Density = 0))
          }
        } else {
          TabValsSurf <- NULL
        }
        return(list(TabVals, TabValsSurf))
      })
      DonneesTxtDiff <- eventReactive(input$PlotButDiff, {
        DonneesDens <- ProcessFunctions(ItemDistDiff()[["name_dens"]])
        Prefixe <- DonneesDens[[1]]
        Declaration <- DonneesDens[[2]]
        FctDens <- DonneesDens[[3]]
        Parametres <- ItemDistDiff()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDistDiff(), "_", param)]]), names(Parametres))
        TxtDonnees <- paste(paste0("Donnees <- data.frame(\"x\" = seq(", input$MinDensDiff, ", ", input$MaxDensDiff, ", length.out = 1000))"),
                            paste0("Donnees$Density <- ", FctDens, "(Donnees$x, ", paste(names(ValeursParametres), " = ", ValeursParametres, collapse = ", ", sep = ""), ")"),
                            sep = "\n")
        if (!is.null(Declaration)) TxtDonnees <- paste0(Declaration, "\n", TxtDonnees)
        if (!is.null(Prefixe)) TxtDonnees <- paste0(Prefixe, "\n", TxtDonnees)
        if (ValidityInputsDiff()[[1]]) {
          Operator <- ValidityInputsDiff()[[2]][1, 1]
          Threshold <- ValidityInputsDiff()[[2]][1, 2]
          if (Operator == "<") {
            TxtDonnees <- paste0(TxtDonnees, "\n",
                                 paste0("DonneesSurf <- data.frame(\"x\" = c(seq(", input$MinDensDiff, ", ", Threshold, ", length.out = 1000)", ", ", Threshold, ", ", input$MinDensDiff, "))"))
          } else {
            TxtDonnees <- paste0(TxtDonnees, "\n",
                                 paste0("DonneesSurf <- data.frame(\"x\" = c(seq(", input$MaxDensDiff, ", ", Threshold, ", length.out = 1000)", ", ", Threshold, ", ", input$MaxDensDiff, "))"))
          }
          TxtDonnees <- paste0(TxtDonnees, "\nDonneesSurf$Density <- 0\n", 
                               paste0("DonneesSurf$Density[1:1000] <- ", FctDens, "(DonneesSurf$x[1:1000], ", paste(names(ValeursParametres), " = ", ValeursParametres, collapse = ", ", sep = ""), ")"))
        }
        return(TxtDonnees)
      })
      
      # Plot the density
      output$GrapheDensiteBaseRDiff <- renderPlot({
        plot(DonneesDistDiff()[[1]]$x, DonneesDistDiff()[[1]]$Density, type = "l", xlab = bquote(x[1] - x[2]), ylab = "Density", lwd = 1.2)
        if (!is.null(DonneesDistDiff()[[2]])) {
          polygon(DonneesDistDiff()[[2]]$x, DonneesDistDiff()[[2]]$Density, col = "steelblue", border = NA)
        }
      })
      output$GrapheDensiteGgplotDiff <- renderPlot({
        Graphe <- ggplot(DonneesDistDiff()[[1]], aes(x, Density))
        if (!is.null(DonneesDistDiff()[[2]])) {
          Graphe <- Graphe + 
            geom_polygon(data = DonneesDistDiff()[[2]], fill = "steelblue")
        }
        Graphe +
          geom_line(linewidth = 1.2) +
          labs(x = bquote(x[1] - x[2]))
      })
      
      # Code to get the plot
      output$CodeDensiteBaseRDiff <- renderText({
        StringCode <- paste(DonneesTxtDiff(),
                            "plot(Donnees$x, Donnees$Density, type = \"l\", xlab = bquote(x[1] - x[2]), ylab = \"Density\", lwd = 1.2)",
                            sep = "\n")
        if (ValidityInputsDiff()[[1]]) {
          StringCode <- paste0(StringCode, "\n",
                               "polygon(DonneesSurf$x, DonneesSurf$Density, col = \"steelblue\", border = NA)")
        }
        return(StringCode)
      })
      output$CodeDensiteGgplotDiff <- renderText({
        StringCode <- paste("library(ggplot2)",
                            DonneesTxtDiff(),
                            "ggplot(Donnees, aes(x, Density)) +",
                            "    geom_line(linewidth = 1.2)",
                            sep = "\n")
        if (ValidityInputsDiff()[[1]]) {
          StringCode <- paste0(StringCode, " +\n",
                               "    geom_polygon(data = DonneesSurf, fill = \"steelblue\") +\n     labs(x = bquote(x[1] - x[2]))")
        }
        return(StringCode)
      })
      
      # Result of the probabilities wanted
      output$TableResDiff <- renderTable({
        req(ValidityInputsDiff())
        validate(need(
          ValidityInputsDiff()[[1]],
          HTML("In order to get results, please specify valid threshold(s).")
        ))
        TabOperators <- ValidityInputsDiff()[[2]]
        FctCDF <- ItemDistDiff()[["cdf"]]
        Parametres <- ItemDistDiff()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDistDiff(), "_", param)]]), names(Parametres))
        data.frame(
          "Probability" = vapply(seq_len(nrow(TabOperators)), \(x) paste0("Pr(X1-X2 ", TabOperators$operator[x], " ", TabOperators$threshold[x], ")"), character(1)),
          "Result" = vapply(seq_len(nrow(TabOperators)), \(x) {
            Proba <- do.call(FctCDF, c(list("q" = TabOperators$threshold[x]), ValeursParametres))
            if (TabOperators$operator[x] == ">") Proba <- 1 - Proba
            return(sprintf("%.3f", Proba))
          }, 
          character(1))
        )
      })
      
      # Code to reproduce the probabilities
      output$CodeResDiff <- renderText({
        req(ValidityInputsDiff())
        DonneesCDF <- ProcessFunctions(ItemDistDiff()[["name_cdf"]])
        Prefixe <- DonneesCDF[[1]]
        Declaration <- DonneesCDF[[2]]
        FctCDF <- DonneesCDF[[3]]
        Parametres <- ItemDistDiff()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDistDiff(), "_", param)]]), names(Parametres))
        TxtCDF <- paste(
          "threshold <- ...",
          "# To get the result of Pr(X1-X2 < threshold)",
          paste0(FctCDF, "(threshold, ", paste(names(ValeursParametres), " = ", ValeursParametres, collapse = ", ", sep = ""), ")"),
          "# And to get the result of Pr(X1-X2 > threshold)",
          paste0("1 - ", FctCDF, "(threshold, ", paste(names(ValeursParametres), " = ", ValeursParametres, collapse = ", ", sep = ""), ")"),
          sep = "\n"
        )
        if (!is.null(Declaration)) TxtCDF <- paste0(Declaration, "\n", TxtCDF)
        if (!is.null(Prefixe)) TxtCDF <- paste0(Prefixe, "\n", TxtCDF)
        return(TxtCDF)
      })
      
    }
  )
}


