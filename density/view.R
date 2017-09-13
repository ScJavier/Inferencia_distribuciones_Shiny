rm(list=ls())
source(file = "Infrastructure/ViewJags.R")
sh <- shinyObject( "Probabilidad")


#list.files("temp/", pattern = "\\.rds$")

shinyApp(sh$ui, sh$server)
