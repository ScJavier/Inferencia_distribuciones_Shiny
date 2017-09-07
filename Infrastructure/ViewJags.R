library(shiny)
library(plotly)
library(shinydashboard)
library(ggplot2)

shinyObject <- function (title = "Sin nombre"){
  
  
  shinyObject  <- list()
  shinyObject$ui <- dashboardPage( skin = "black",
    dashboardHeader(title = title),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Bernoulli", tabName = "bernoulliContent", icon = icon("dashboard")),
        menuItem("Binomial", tabName = "binomialContent", icon = icon("dashboard")),
        menuItem("Geométrica", tabName = "geometricaContent", icon = icon("dashboard")),
        menuItem("Binomial Negativa", tabName = "nBinomialContent", icon = icon("dashboard")),
        menuItem("Poisson", tabName = "poissonContent", icon = icon("dashboard")),
        menuItem("Hipergeométrica", tabName = "hiperContent", icon = icon("dashboard")),
        menuItem("Uniforme", tabName = "uniformeContent", icon = icon("dashboard")),
        menuItem("Exponencial", tabName = "exponencialContent", icon = icon("dashboard")),
        menuItem("Normal", tabName = "normalContent", icon = icon("dashboard"), selected = TRUE),
        menuItem("Gamma", tabName = "gammaContent", icon = icon("dashboard")),
        menuItem("Cauchy", tabName = "cauchyContent", icon = icon("dashboard")),
        menuItem("Weibull", tabName = "weibullContent", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "bernoulliContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 1, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("bernoulliProb", "Probabilidad", value = 0.5,
                                 min = 0.05, max = 0.95, step = 0.05)
                  ),
                  box(
                    actionButton("bernoulliLoad", "Cargar")
                  )
                ),
                fluidRow(
                  plotlyOutput("bernoulliGraphic")
                )
        ),
        
        tabItem(tabName = "binomialContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("binomialSize", "Tamaño", value = 5, min = 1, step = 1),
                    actionButton("binomialLoad", "Cargar")
                  ),
                  box(
                    numericInput("binomialProb", "Probabilidad", value = 0.5,
                                 min = 0.05, max = 0.95, step = 0.05)
                  )
                ),
                fluidRow(
                  plotlyOutput("binomialGraphic")
                )
        ),
        
        tabItem(tabName = "geometricaContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 1, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("geometricaProb", "Prob de éxito", value = 0.5,
                                 min = 0.05, max = 0.95, step = 0.05)
                  ),
                  box(
                    actionButton("geometricaLoad", "Cargar")
                  )
                ),
                fluidRow(
                  plotlyOutput("geometricaGraphic")
                )
        ),
        
        tabItem(tabName = "nBinomialContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("nBinomSize", "Tamaño", value = 1, min = 1, step = 1)
                  ),
                  box(
                    numericInput("nBinomProb", "Probabilidad", value = 0.5,
                                 min = 0.05, max = 0.95, step = 0.05),
                    actionButton("nBinomLoad", "Cargar")
                  )
                ),
                fluidRow(
                  plotlyOutput("nBinomGraphic")
                )
        ),
        
        tabItem(tabName = "poissonContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 1, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("poissonLambda", "Intensidad", value = 3,
                                 min = 0.25, step = 0.25)
                  ),
                  box(
                    actionButton("poissonLoad", "Cargar")
                  )
                ),
                fluidRow(
                  plotlyOutput("poissonGraphic")
                )
        ),
        
        tabItem(tabName = "hiperContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 3, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("hiperM", "Número de bolas blancas", value = 5, min = 1, step = 1),
                    numericInput("hiperN", "Número de bolas negras", value = 5, min = 1, step = 1)
                  ),
                  box(
                    numericInput("hiperK", "Número de bolas extraídas", value = 3, min = 1, step = 1),
                    actionButton("hiperLoad", "Cargar")
                  )
                ),
                fluidRow(
                  plotlyOutput("hiperGraphic")
                )
        ),
        
        tabItem(tabName = "uniformeContent", 
                fluidRow(
                  # A static infoBox
                    infoBox("Tipo", "Continua", icon = icon("chain"), color="purple"),
                    # Dynamic infoBoxes
                    infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("uniformeAlpha", "alpha", 1),
                    numericInput("uniformeBeta", "beta", 5),
                    actionButton("uniformeLoad", "Cargar")
                  ),
                  box(
                    numericInput("uniformeMin", "Mínimo", 0),
                    numericInput("uniformeMax", "Máximo", 6)
                  )
                  
                ),
                fluidRow(
                  plotlyOutput("uniformeGraphic")
                )
        ),
        
        tabItem(tabName = "exponencialContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Continua", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 1, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("exponencialRate", "rate", 3),
                    actionButton("exponencialLoad", "Cargar")
                  ),
                  box(
                    numericInput("exponencialMin", "Mínimo", 0),
                    numericInput("exponencialMax", "Máximo", 5)
                  )
                ),
                fluidRow(
                  plotlyOutput("exponencialGraphic")
                )
        ),
        
        tabItem(tabName = "normalContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Continua", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("normalMean", "Media", 0),
                    numericInput("normalSd", "Desviación estandar", 1),
                    actionButton("normalLoad", "Cargar")
                  ),
                  box(
                    numericInput("normalMin", "Mínimo", -5),
                    numericInput("normalMax", "Máximo", 5)
                  )
                ),
                fluidRow(
                  plotlyOutput("normalGraphic")
                )
        ),
        
        tabItem(tabName = "gammaContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Continua", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("gammaShape", "Shape", 1),
                    numericInput("gammaRate", "Rate", 1),
                    actionButton("gammaLoad", "Cargar")
                  ),
                  box(
                    numericInput("gammaMin", "Mínimo", 0),
                    numericInput("gammaMax", "Máximo", 5)
                  )
                ),
                fluidRow(
                  plotlyOutput("gammaGraphic")
                )
        ),
        
        tabItem(tabName = "cauchyContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Continua", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("cauchyLocation", "Ubicación", 0),
                    numericInput("cauchyScale", "Escala", 1),
                    actionButton("cauchyLoad", "Cargar")
                  ),
                  box(
                    numericInput("cauchyMin", "Mínimo", -4),
                    numericInput("cauchyMax", "Máximo", 4)
                  )
                ),
                fluidRow(
                  plotlyOutput("cauchyGraphic")
                )
        ),
        
        tabItem(tabName = "weibullContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Continua", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("weibullShape", "Forma", 1),
                    numericInput("weibullScale", "Escala", 3.1416),
                    actionButton("weibullLoad", "Cargar")
                  ),
                  box(
                    numericInput("weibullMin", "Mínimo", 0),
                    numericInput("weibullMax", "Máximo", 4)
                  )
                ),
                fluidRow(
                  plotlyOutput("weibullGraphic")
                )
        )
      )
    )
  )
  
  shinyObject$server <- function(input, output, session) {
    
    output$bernoulliGraphic <-renderPlotly({
      
      input$bernoulliLoad
      tempBernoulliProb <- isolate(input$bernoulliProb)
      cat("tempBernoulliProb ", tempBernoulliProb, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      df <- data.frame(x = as.factor(0:1), probs = c(1 - tempBernoulliProb, tempBernoulliProb))
      tempPlot <- ggplot(df, aes(x, probs)) +
        geom_col(fill = 'darkcyan', col = 'black') + ylim(0, 1.2*max(df$probs))
      
      gg <- ggplotly(tempPlot)
      
      return (gg)
    })
 
    output$binomialGraphic <-renderPlotly({
      
      input$binomialLoad
      
      tempBinomialSize <- isolate(input$binomialSize)
      tempBinomialProb <- isolate(input$binomialProb)
      
      cat("tempBinomialSize ", tempBinomialSize, " \n")
      cat("tempBinomialProb ", tempBinomialProb, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      df <- data.frame(x = as.factor(0:tempBinomialSize),
                       probs = dbinom(0:tempBinomialSize, tempBinomialSize, tempBinomialProb))
      tempPlot <- ggplot(df, aes(x, probs)) + geom_col(fill = 'darkcyan', col = 'black') +
        ylim(0, 1.2*max(df$probs))
      gg <- ggplotly(tempPlot)
      
      return (gg)
    })
    
    output$geometricaGraphic <-renderPlotly({
      
      input$geometricaLoad
      tempGeometricaProb <- isolate(input$geometricaProb)
      cat("tempGeometricaProb ", tempGeometricaProb, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      n <- qgeom(0.999, tempGeometricaProb) 
      df <- data.frame(x = as.factor(0:n), probs = dgeom(0:n, tempGeometricaProb))
      tempPlot <- ggplot(df, aes(x, probs)) + geom_col(fill = 'darkcyan', col = 'black') +
        ylim(0, 1.2*max(df$probs))

      gg <- ggplotly(tempPlot)
      
      return (gg)
    })    
    
    output$nBinomGraphic <-renderPlotly({
      
      input$nBinomLoad
      
      tempNBinomSize <- isolate(input$nBinomSize)
      tempNBinomProb <- isolate(input$nBinomProb)
      
      cat("tempNBinomSize ", tempNBinomSize, " \n")
      cat("tempNBinomProb ", tempNBinomProb, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      n <- qnbinom(0.999, tempNBinomSize, tempNBinomProb)
      df <- data.frame(x = as.factor(0:n), probs = dnbinom(0:n, tempNBinomSize, tempNBinomProb))
      tempPlot <- ggplot(df, aes(x, probs)) + geom_col(fill = 'darkcyan', col = 'black') +
        ylim(0, 1.2*max(df$probs))
      
      gg <- ggplotly(tempPlot)
      
      return (gg)
    })

    output$poissonGraphic <-renderPlotly({
      
      input$poissonLoad
      tempPoissonLambda <- isolate(input$poissonLambda)

      cat("tempPoissonLambda ", tempPoissonLambda, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      n <- qpois(0.999, tempPoissonLambda)
      df <- data.frame(x = as.factor(0:n), probs = dpois(0:n, tempPoissonLambda))
      tempPlot <- ggplot(df, aes(x, probs)) + geom_col(fill = 'darkcyan', col = 'black') +
        ylim(0, 1.2*max(df$probs))
      
      gg <- ggplotly(tempPlot)
      
      return (gg)
    })
    
    output$hiperGraphic <-renderPlotly({
      
      input$hiperLoad
      
      tempHiperM <- isolate(input$hiperM)
      tempHiperN <- isolate(input$hiperN)
      tempHiperK <- isolate(input$hiperK)
    
      cat("tempHiperM ", tempHiperM, " \n")
      cat("tempHiperN ", tempHiperN, " \n")
      cat("tempHiperK ", tempHiperK, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      updateNumericInput(session, "hiperK", "Número de bolas extraídas",
                         value = tempHiperK,
                         min = 1, max = tempHiperM + tempHiperN, step = 1)
      tempHiperMin <- max(0, tempHiperK - tempHiperN)
      tempHiperMax <- min(tempHiperK, tempHiperM)
      df <- data.frame(x = as.factor(tempHiperMin:tempHiperMax),
                       probs = dhyper(tempHiperMin:tempHiperMax,
                                      m = tempHiperM,
                                      n = tempHiperN,
                                      k = tempHiperK))
      tempPlot <- ggplot(df, aes(x, probs)) + geom_col(fill = 'darkcyan', col = 'black') +
        ylim(0, 1.2*max(df$probs))
      
      gg <- ggplotly(tempPlot)
      
      return (gg)
    })
           
    output$uniformeGraphic <-renderPlotly({
      
      input$uniformeLoad
      
      tempUniformeAlpha <- isolate(input$uniformeAlpha)
      tempUniformeBeta <- isolate(input$uniformeBeta)
      tempUniformeMin <- isolate(input$uniformeMin)
      tempUniformeMax <- isolate(input$uniformeMax)
      
      cat("uniformeAlpha ", tempUniformeAlpha, " \n")
      cat("uniformeBeta ", tempUniformeBeta, " \n")
      cat("tempUniformeMin ", tempUniformeMin, " \n")
      cat("tempUniformeMax ", tempUniformeMax, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      
      tempPlot <- ggplot(data.frame(x = c(tempUniformeMin, tempUniformeMax)), aes(x)) + 
        stat_function(fun = dunif, args = list(min = tempUniformeAlpha, max = tempUniformeBeta))
      
      gg <- ggplotly(tempPlot)
      
      return(gg)
    })
    
    output$exponencialGraphic <-renderPlotly({
      
      input$exponencialLoad
      
      tempExponencialRate <- isolate(input$exponencialRate)
      tempExponencialMin <- isolate(input$exponencialMin)
      tempExponencialMax <- isolate(input$exponencialMax)
      
      cat("tempExponencialRate ", tempExponencialRate, " \n")
      cat("tempExponencialMin ", tempExponencialMin, " \n")
      cat("tempExponencialMax ", tempExponencialMax, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      tempPlot <- ggplot(data.frame(x = c(tempExponencialMin, tempExponencialMax)), aes(x)) + 
        stat_function(fun = dexp, args = list(rate = tempExponencialRate))
      
      gg <- ggplotly(tempPlot)
      
      return ( gg)
    })
    
    output$normalGraphic <-renderPlotly({
      
      input$normalLoad
      
      tempNormalMean <- isolate(input$normalMean)
      tempNormalSd <- isolate(input$normalSd)
      tempNormalMin <- isolate(input$normalMin)
      tempNormalMax <- isolate(input$normalMax)
      
      cat("tempNormalMean ", tempNormalMean, " \n")
      cat("tempNormalSd ", tempNormalSd, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      
      tempPlot <- ggplot(data.frame(x = c(tempNormalMin, tempNormalMax)), aes(x)) + 
        stat_function(fun = dnorm, args = list(mean = tempNormalMean, sd = tempNormalSd))
      
      gg <- ggplotly(tempPlot)
      
      return ( gg)
    })
    
    output$gammaGraphic <-renderPlotly({
      
      input$gammaLoad
      
      tempGammaShape <- isolate(input$gammaShape)
      tempGammaRate <- isolate(input$gammaRate)
      tempGammaMin <- isolate(input$gammaMin)
      tempGammaMax <- isolate(input$gammaMax)
      
      cat("tempGammaShape ", tempGammaShape, " \n")
      cat("tempGammaRate ", tempGammaRate, " \n")
      cat("tempGammaMin ", tempGammaMin, " \n")
      cat("tempGammaMax ", tempGammaMax, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      
      tempPlot <- ggplot(data.frame(x = c(tempGammaMin, tempGammaMax)), aes(x)) + 
        stat_function(fun = dgamma, args = list(shape = tempGammaShape, rate = tempGammaRate))
      
      gg <- ggplotly(tempPlot)
      
      return ( gg)
    })
    
    output$cauchyGraphic <-renderPlotly({
      
      input$cauchyLoad
      
      tempCauchyLocation <- isolate(input$cauchyLocation)
      tempCauchyScale <- isolate(input$cauchyScale)
      tempCauchyMin <- isolate(input$cauchyMin)
      tempCauchyMax <- isolate(input$cauchyMax)
      
      cat("tempCauchyLocation ", tempCauchyLocation, " \n")
      cat("tempCauchyScale ", tempCauchyScale, " \n")
      cat("tempCauchyMin ", tempCauchyMin, " \n")
      cat("tempCauchyMax ", tempCauchyMax, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      
      tempPlot <- ggplot(data.frame(x = c(tempCauchyMin, tempCauchyMax)), aes(x)) + 
        stat_function(fun = dcauchy, args = list(location = tempCauchyLocation, scale = tempCauchyScale))
      
      gg <- ggplotly(tempPlot)
      
      return ( gg)
    })
    
    output$weibullGraphic <-renderPlotly({
      
      input$weibullLoad
      
      tempWeibullShape <- isolate(input$weibullShape)
      tempWeibullScale <- isolate(input$weibullScale)
      tempWeibullMin <- isolate(input$weibullMin)
      tempWeibullMax <- isolate(input$weibullMax)
      
      cat("tempWeibullShape ", tempWeibullShape, " \n")
      cat("tempWeibullScale ", tempWeibullScale, " \n")
      cat("tempWeibullMin ", tempWeibullMin, " \n")
      cat("tempWeibullMax ", tempWeibullMax, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      
      tempPlot <- ggplot(data.frame(x = c(tempWeibullMin, tempWeibullMax)), aes(x)) + 
        stat_function(fun = dweibull, args = list(shape = tempWeibullShape, scale = tempWeibullScale))
      
      gg <- ggplotly(tempPlot)
      
      return ( gg)
    })

  }
  
  return (shinyObject);
}

