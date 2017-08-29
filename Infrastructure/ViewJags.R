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
        menuItem("Uniforme", tabName = "uniformeContent", icon = icon("dashboard")),
        menuItem("Exponencial", tabName = "exponencialContent", icon = icon("dashboard")),
        menuItem("Normal", tabName = "normalContent", icon = icon("dashboard")),
        menuItem("Gamma", tabName = "gammaContent", icon = icon("dashboard"), selected = TRUE),
        menuItem("Bernoulli", tabName = "bernoulliContent", icon = icon("dashboard")),
        menuItem("Cauchy", tabName = "cauchyContent", icon = icon("dashboard")),
        menuItem("Weibull", tabName = "weibullContent", icon = icon("dashboard")),
        menuItem("Binomial", tabName = "binomialContent", icon = icon("dashboard")),
        menuItem("Poisson", tabName = "poissonContent", icon = icon("dashboard")),
        menuItem("Geométrica", tabName = "geometricaContent", icon = icon("dashboard")),
        menuItem("Hipergeométrica", tabName = "hipergeometricaContent", icon = icon("dashboard")),
        menuItem("Binomial Negativa", tabName = "binomailNegativaContent", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      tabItems(
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
                    numericInput("exponencialMin", "Mínimo", -5),
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
                    numericInput("gammaMin", "Mínimo", -5),
                    numericInput("gammaMax", "Máximo", 5)
                  )
                ),
                fluidRow(
                  plotlyOutput("gammaGraphic")
                )
        ),
        tabItem(tabName = "bernoulliContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 1, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("bernoulliProb", "Probabilidad", 1),
                    actionButton("bernoulliLoad", "Cargar")
                  ),
                  box(
                    numericInput("bernoulliMin", "Mínimo", 0),
                    numericInput("bernoulliMax", "Máximo", 0.5)
                  )
                ),
                fluidRow(
                  plotlyOutput("bernoulliGraphic")
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
        tabItem(tabName = "binomialContent", 
                fluidRow(
                  # A static infoBox
                  infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                  # Dynamic infoBoxes
                  infoBox("# Parámetros", 2, icon = icon("book"), color="orange")
                ),
                fluidRow(
                  box(
                    numericInput("binomialSize", "Tamaño", 10),
                    numericInput("binomialProb", "Probabilidad", 0.5),
                    actionButton("binomialLoad", "Cargar")
                  ),
                  box(
                    numericInput("binomialMin", "Mínimo", 0),
                    numericInput("binomialMax", "Máximo", 1)
                  )
                ),
                fluidRow(
                  plotlyOutput("binomialGraphic")
                )
        )
      )
    )
  )
  
  shinyObject$server <- function(input, output, session) {
    
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
    
    output$bernoulliGraphic <-renderPlotly({
      
      input$bernoulliLoad
      
      tempBernoulliProb <- isolate(input$bernoulliProb)
      tempBernoulliMin <- isolate(input$bernoulliMin)
      tempBernoulliMax <- isolate(input$bernoulliMax)
      
      cat("tempBernoulliProb ", tempBernoulliProb, " \n")
      cat("tempBernoulliMin ", tempBernoulliMin, " \n")
      cat("tempBernoulliMax ", tempBernoulliMax, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      
      tempPlot <- ggplot(data.frame(x = c(tempBernoulliMin, tempBernoulliMax)), aes(x)) + 
        stat_function(fun = dbinom, geom="point", args = list(size = 1, prob = tempBernoulliProb))
      
      gg <- ggplotly(tempPlot)
      
      return ( gg)
    })
    
    output$binomialGraphic <-renderPlotly({
      
      input$binomialLoad
      
      tempBinomialSize <- isolate(input$binomialSize)
      tempBinomialProb <- isolate(input$binomialProb)
      tempBinomialMin <- isolate(input$binomialMin)
      tempBinomialMax <- isolate(input$binomialMax)
      
      cat("tempBinomialSize ", tempBinomialSize, " \n")
      cat("tempBinomialProb ", tempBinomialProb, " \n")
      cat("tempBinomialMin ", tempBinomialMin, " \n")
      cat("tempBinomialMax ", tempBinomialMax, " \n")
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      
      tempPlot <- ggplot(data.frame(x = c(tempBinomialMin, tempBinomialMax)), aes(x)) + 
        stat_function(fun = dbinom, args = list(size = tempBinomialSize, prob = tempBinomialProb))
      
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
    
    
  }
  
  return (shinyObject);
}

