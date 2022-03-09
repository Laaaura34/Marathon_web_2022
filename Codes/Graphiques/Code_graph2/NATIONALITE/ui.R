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


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Nombre alternants"),
  sidebarLayout(
    sidebarPanel(
      
      
      uiOutput("indirect_select_formation",
               ),
     
    ),
    
    mainPanel(
      plotlyOutput("distPie", height = "500px", width="600px"),

      #plotOutput("Histograma"),
      #plotOutput("Histograma2"),
      
      
    )
  )
))
