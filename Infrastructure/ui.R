library(shiny)
library(shinydashboard)
library(plotly)

shinyUI(dashboardPage( skin = "black",
               dashboardHeader(title = "Probabilidad"),
               dashboardSidebar(
                 sidebarMenu(
                   menuItem("Uniforme", tabName = "uniformeContent", icon = icon("dashboard")),
                   menuItem("Exponencial", tabName = "exponencialContent", icon = icon("dashboard")),
                   menuItem("Normal", tabName = "normalContent", icon = icon("dashboard"), selected = TRUE),
                   menuItem("Gamma", tabName = "gammaContent", icon = icon("dashboard")),
                   menuItem("Bernoulli", tabName = "bernoulliContent", icon = icon("dashboard")),
                   menuItem("Cauchy", tabName = "cauchyContent", icon = icon("dashboard")),
                   menuItem("Weibull", tabName = "weibullContent", icon = icon("dashboard")),
                   menuItem("Binomial", tabName = "binomialContent", icon = icon("dashboard")),
                   menuItem("Poisson", tabName = "poissonContent", icon = icon("dashboard")),
                   menuItem("Geométrica", tabName = "geometricaContent", icon = icon("dashboard")),
                   menuItem("Hipergeométrica", tabName = "hiperContent", icon = icon("dashboard")),
                   menuItem("Binomial Negativa", tabName = "nBinomialContent", icon = icon("dashboard"))
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
                   tabItem(tabName = "bernoulliContent", 
                           fluidRow(
                             # A static infoBox
                             infoBox("Tipo", "Discreta", icon = icon("chain"), color="purple"),
                             # Dynamic infoBoxes
                             infoBox("# Parámetros", 1, icon = icon("book"), color="orange")
                           ),
                           fluidRow(
                             box(
                               numericInput("bernoulliProb", "Probabilidad", 0.5),
                               actionButton("bernoulliLoad", "Cargar")
                             ),
                             box(
                               numericInput("bernoulliMin", "Mínimo", 0),
                               numericInput("bernoulliMax", "Máximo", 2)
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
                               numericInput("binomialMax", "Máximo", 10)
                             )
                           ),
                           fluidRow(
                             plotlyOutput("binomialGraphic")
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
                               numericInput("poissonLambda", "Media y Varianza", 3),
                               actionButton("poissonLoad", "Cargar")
                             ),
                             box(
                               numericInput("poissonMin", "Mínimo", 0),
                               numericInput("poissonMax", "Máximo", 10)
                             )
                           ),
                           fluidRow(
                             plotlyOutput("poissonGraphic")
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
                               numericInput("geometricaProb", "Prob de éxito", 0.3),
                               actionButton("geometricaLoad", "Cargar")
                             ),
                             box(
                               numericInput("geometricaMin", "Mínimo", 0),
                               numericInput("geometricaMax", "Máximo", 10)
                             )
                           ),
                           fluidRow(
                             plotlyOutput("geometricaGraphic")
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
                               numericInput("hiperM", "m", 10),
                               numericInput("hiperN", "n", 7),
                               numericInput("hiperK", "k", 8),
                               actionButton("hiperLoad", "Cargar")
                             ),
                             box(
                               numericInput("hiperMin", "Mínimo", 0),
                               numericInput("hiperMax", "Máximo", 100)
                             )
                           ),
                           fluidRow(
                             plotlyOutput("hiperGraphic")
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
                               numericInput("nBinomSize", "Tamaño", 1),
                               numericInput("nBinomProb", "Probabilidad", 0.5),
                               actionButton("nBinomLoad", "Cargar")
                             ),
                             box(
                               numericInput("nBinomMin", "Mínimo", 0),
                               numericInput("nBinomMax", "Máximo", 100)
                             )
                           ),
                           fluidRow(
                             plotlyOutput("nBinomGraphic")
                           )
                   )
                 )
               )
))