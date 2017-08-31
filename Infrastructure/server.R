library(shiny)
library(plotly)
library(ggplot2)


shinyServer(function(input, output, session) {
  
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
      stat_function(fun = dbinom,  args = list(size = 1, prob = tempBernoulliProb))
    
    gg <- ggplotly(tempPlot)
    
    return (gg)
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
  
  output$poissonGraphic <-renderPlotly({
    
    input$poissonLoad
    
    tempPoissonLambda <- isolate(input$poissonLambda)
    tempPoissonMin <- isolate(input$poissonMin)
    tempPoissonMax <- isolate(input$poissonMax)
    
    cat("tempPoissonLambda ", tempPoissonLambda, " \n")
    cat("tempPoissonMin ", tempPoissonMin, " \n")
    cat("tempPoissonMax ", tempPoissonMax, " \n")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    
    tempPlot <- ggplot(data.frame(x = c(tempPoissonMin, tempPoissonMax)), aes(x)) + 
      stat_function(fun = dpois, args = list(lambda = tempPoissonLambda))
    
    gg <- ggplotly(tempPlot)
    
    return ( gg)
  })
  
  output$geometricaGraphic <-renderPlotly({
    
    input$geometricaLoad
    
    tempGeometricaProb <- isolate(input$geometricaProb)
    tempGeometricaMin <- isolate(input$geometricaMin)
    tempGeometricaMax <- isolate(input$geometricaMax)
    
    cat("tempGeometricaProb ", tempGeometricaProb, " \n")
    cat("tempGeometricaMin ", tempGeometricaMin, " \n")
    cat("tempGeometricaMax ", tempGeometricaMax, " \n")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    
    tempPlot <- ggplot(data.frame(x = c(tempGeometricaMin, tempGeometricaMax)), aes(x)) + 
      stat_function(fun = dgeom, args = list(prob = tempGeometricaProb))
    
    gg <- ggplotly(tempPlot)
    
    return ( gg)
  })
  
  output$hiperGraphic <-renderPlotly({
    
    input$hiperLoad
    
    tempHiperM <- isolate(input$hiperM)
    tempHiperN <- isolate(input$hiperN)
    tempHiperK <- isolate(input$hiperK)
    
    tempHiperMin <- isolate(input$hiperMin)
    tempHiperMax <- isolate(input$hiperMax)
    
    
    cat("tempHiperM ", tempHiperM, " \n")
    cat("tempHiperN ", tempHiperN, " \n")
    cat("tempHiperK ", tempHiperK, " \n")
    cat("tempHipeMin ", tempHiperMin, " \n")
    cat("tempHipeMax ", tempHiperMax, " \n")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    
    tempPlot <- ggplot(data.frame(x = c(tempHiperMin, tempHiperMax)), aes(x)) + 
      stat_function(fun = dhyper, args = list(m = tempHiperM, n = tempHiperN, k = tempHiperK ))
    
    gg <- ggplotly(tempPlot)
    
    return ( gg)
  })
  
  output$nBinomGraphic <-renderPlotly({
    
    input$nBinomLoad
    
    tempNBinomSize <- isolate(input$nBinomSize)
    tempNBinomProb <- isolate(input$nBinomProb)
    
    tempNBinomMin <- isolate(input$nBinomMin)
    tempNBinomMax <- isolate(input$nBinomMax)
    
    
    cat("tempNBinomSize ", tempNBinomSize, " \n")
    cat("tempNBinomProb ", tempNBinomProb, " \n")
    cat("tempNBinomMin ", tempNBinomMin, " \n")
    cat("tempNBinomMax ", tempNBinomMax, " \n")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    
    tempPlot <- ggplot(data.frame(x = c(tempNBinomMin, tempNBinomMax)), aes(x)) + 
      stat_function(fun = dnbinom, args = list(size = tempNBinomSize, prob = tempNBinomProb))
    
    gg <- ggplotly(tempPlot)
    
    return ( gg)
  })
  
})