library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(plyr)
library(plotly)

datos <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_etudiants.csv",dec = ",")


shinyServer(function(input, output) {
  

output$distPie <- renderPlotly({
  
  
  

  frnationality <- as.data.frame(table(datos$Nationalite == "FRANCAIS(E)"))

  frcount <- as.data.frame(rbind(frnationality[2,2], frnationality[1,2]))
  names(frcount)
  plot_ly(frcount,values=~V1,labels=~c("Nationalité française","Nationalité étrangère"),marker=list(colors=c("green","yellow")),type="pie")
  
})


})






