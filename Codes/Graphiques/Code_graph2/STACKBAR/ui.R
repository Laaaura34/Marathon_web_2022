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

datos <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_recruteurs.csv",sep = ";")


shinyUI(fluidPage(
  titlePanel("Nombre alternants"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "anInput", 
                  label = "Sélectionner une année : ", 
                  choices = c("2016-2017"="2016-2017", "2017-2018"="2017-2018", "2018-2019"="2018-2019", "2019-2020"="2019-2020", "2020-2021"="2020-2021",
                              "2021-2022"="2021-2022"),
                  selected = c("2016-2017"="2016-2017")),
      
  
    ),
      mainPanel(
        
      
      plotlyOutput("Histograma", height = "900px", width = "1100px"),
      plotlyOutput("distPieGlobal", height = "600px", width = "700px")
      
      )
  )
))