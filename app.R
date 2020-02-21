library(shiny)
library(shinythemes)
rm(list=ls())
# options(shiny.reactlog=TRUE) 
source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)