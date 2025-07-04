# Script pour le server de visualisation d'une distribution #
# Auteur : G. Mulier                                        #
# Créé le 07/05/2025, modifié le 04/07/2025                 #

DistUniqueServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Reactive value of the chosen distribution and validity of inputs
      ChosenDist <- reactive({input$ChoixDist1})
      ItemDist <- reactive({ListeDistribution[[ChosenDist()]]})
      ValidityInputs <- eventReactive(input$PlotBut1, {CheckThresh(input$InputThresh1)})
      
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
      
      # Dynamic generation of data
      Donnees1Dist <- eventReactive(input$PlotBut1, {
        FctDens <- ItemDist()[["dens"]]
        Parametres <- ItemDist()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDist(), "_", param)]]), names(Parametres))
        TabVals <- data.frame(x = seq(input$MinDens1, input$MaxDens1, length.out = 1000))
        TabVals$Density <- do.call(FctDens, c(list("x" = TabVals$x), ValeursParametres))
        if (ValidityInputs()[[1]]) {
          Operator <- ValidityInputs()[[2]][1, 1]
          Threshold <- ValidityInputs()[[2]][1, 2]
          if (Operator == "<") {
            TabValsSurf <- data.frame(x = seq(input$MinDens1, Threshold, length.out = 1000))
          } else {
            TabValsSurf <- data.frame(x = seq(input$MaxDens1, Threshold, length.out = 1000))
          }
          TabValsSurf$Density <- do.call(FctDens, c(list("x" = TabValsSurf$x), ValeursParametres))
          if (Operator == "<") {
            TabValsSurf <- rbind(TabValsSurf, data.frame(x = c(Threshold, input$MinDens1), Density = 0))
          } else {
            TabValsSurf <- rbind(TabValsSurf, data.frame(x = c(Threshold, input$MaxDens1), Density = 0))
          }
        } else {
          TabValsSurf <- NULL
        }
        return(list(TabVals, TabValsSurf))
      })
      Donnees1Txt <- eventReactive(input$PlotBut1, {
        DonneesDens <- ProcessFunctions(ItemDist()[["name_dens"]])
        Prefixe <- DonneesDens[[1]]
        Declaration <- DonneesDens[[2]]
        FctDens <- DonneesDens[[3]]
        Parametres <- ItemDist()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDist(), "_", param)]]), names(Parametres))
        TxtDonnees <- paste(paste0("Donnees <- data.frame(x = seq(", input$MinDens1, ", ", input$MaxDens1, ", length.out = 1000))"),
                            paste0("Donnees$Density <- ", FctDens, "(Donnees$x, ", paste(names(ValeursParametres), " = ", ValeursParametres, collapse = ", ", sep = ""), ")"),
                            sep = "\n")
        if (!is.null(Declaration)) TxtDonnees <- paste0(Declaration, "\n", TxtDonnees)
        if (!is.null(Prefixe)) TxtDonnees <- paste0(Prefixe, "\n", TxtDonnees)
        if (ValidityInputs()[[1]]) {
          Operator <- ValidityInputs()[[2]][1, 1]
          Threshold <- ValidityInputs()[[2]][1, 2]
          if (Operator == "<") {
            TxtDonnees <- paste0(TxtDonnees, "\n",
                                 paste0("DonneesSurf <- data.frame(x = c(seq(", input$MinDens1, ", ", Threshold, ", length.out = 1000)", ", ", Threshold, ", ", input$MinDens1, "))"))
          } else {
            TxtDonnees <- paste0(TxtDonnees, "\n",
                                 paste0("DonneesSurf <- data.frame(x = c(seq(", input$MaxDens1, ", ", Threshold, ", length.out = 1000)", ", ", Threshold, ", ", input$MaxDens1, "))"))
          }
          TxtDonnees <- paste0(TxtDonnees, "\nDonneesSurf$Density <- 0\n", 
                               paste0("DonneesSurf$Density[1:1000] <- ", FctDens, "(DonneesSurf$x[1:1000], ", paste(names(ValeursParametres), " = ", ValeursParametres, collapse = ", ", sep = ""), ")"))
        }
        return(TxtDonnees)
      })
      
      # Plot the density
      output$GrapheDensiteBaseR <- renderPlot({
        plot(Donnees1Dist()[[1]]$x, Donnees1Dist()[[1]]$Density, type = "l", xlab = "x", ylab = "Density", lwd = 1.2)
        if (!is.null(Donnees1Dist()[[2]])) {
          polygon(Donnees1Dist()[[2]]$x, Donnees1Dist()[[2]]$Density, col = "steelblue", border = NA)
        }
      })
      output$GrapheDensiteGgplot <- renderPlot({
        Graphe <- ggplot(Donnees1Dist()[[1]], aes(x, Density))
        if (!is.null(Donnees1Dist()[[2]])) {
          Graphe <- Graphe + 
            geom_polygon(data = Donnees1Dist()[[2]], fill = "steelblue")
        }
        Graphe +
          geom_line(linewidth = 1.2)
      })
      
      # Code to get the plot
      output$CodeDensiteBaseR <- renderText({
        StringCode <- paste(Donnees1Txt(),
              "plot(Donnees$x, Donnees$Density, type = \"l\", xlab = \"x\", ylab = \"Density\", lwd = 1.2)",
              sep = "\n")
        if (ValidityInputs()[[1]]) {
          StringCode <- paste0(StringCode, "\n",
                               "polygon(DonneesSurf$x, DonneesSurf$Density, col = \"steelblue\", border = NA)")
        }
        return(StringCode)
      })
      output$CodeDensiteGgplot <- renderText({
        StringCode <- paste("library(ggplot2)",
              Donnees1Txt(),
              "ggplot(Donnees, aes(x, Density)) +",
              "    geom_line(linewidth = 1.2)",
              sep = "\n")
        if (ValidityInputs()[[1]]) {
          StringCode <- paste0(StringCode, " +\n",
                               "    geom_polygon(data = DonneesSurf, fill = \"steelblue\")")
        }
        return(StringCode)
      })
      
      # Result of the probabilities wanted
      output$TableRes1 <- renderTable({
        req(ValidityInputs())
        validate(need(
          ValidityInputs()[[1]],
          HTML("In order to get results, please specify valid threshold(s).")
        ))
        TabOperators <- ValidityInputs()[[2]]
        FctCDF <- ItemDist()[["cdf"]]
        Parametres <- ItemDist()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDist(), "_", param)]]), names(Parametres))
        data.frame(
          "Probability" = vapply(seq_len(nrow(TabOperators)), \(x) paste0("Pr(X ", TabOperators$operator[x], " ", TabOperators$threshold[x], ")"), character(1)),
          "Result" = vapply(seq_len(nrow(TabOperators)), \(x) {
            Proba <- do.call(FctCDF, c(list("q" = TabOperators$threshold[x]), ValeursParametres))
            if (TabOperators$operator[x] == ">") Proba <- 1 - Proba
            return(sprintf("%.3f", Proba))
          }, 
          character(1))
        )
      })
      
      # Code to reproduce the probabilities
      output$CodeRes1 <- renderText({
        req(ValidityInputs())
        DonneesCDF <- ProcessFunctions(ItemDist()[["name_cdf"]])
        Prefixe <- DonneesCDF[[1]]
        Declaration <- DonneesCDF[[2]]
        FctCDF <- DonneesCDF[[3]]
        Parametres <- ItemDist()[["params"]]
        ValeursParametres <- setNames(lapply(names(Parametres), \(param) input[[paste0(ChosenDist(), "_", param)]]), names(Parametres))
        TxtCDF <- paste(
          "threshold <- ...",
          "# To get the result of Pr(X < threshold)",
          paste0(FctCDF, "(threshold, ", paste(names(ValeursParametres), " = ", ValeursParametres, collapse = ", ", sep = ""), ")"),
          "# And to get the result of Pr(X > threshold)",
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


