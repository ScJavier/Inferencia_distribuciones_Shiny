library(shiny)
library(shinydashboard)
library(plotly)

shinyUI(dashboardPage( skin = "black",
               dashboardHeader(title = "Probabilidad"),
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
))