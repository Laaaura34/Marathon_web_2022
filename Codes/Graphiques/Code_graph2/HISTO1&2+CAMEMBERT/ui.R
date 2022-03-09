library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(car)
library(nortest)
library(leaflet)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(plotly)
library(shinyWidgets)


datos <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_etudiants.csv",dec = ",")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Nombre alternants"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(inputId = "anInput", 
                  label = "Sélectionner une année : ", 
                  choices = c("2016-2017"="2017", "2017-2018"="2018", "2018-2019"="2019", "2019-2020"="2020", "2020-2021"="2021",
                              "2021-2022"="2022"),
                  selected = c("2016-2017"="2017")
                 ),
      uiOutput("indirect_select_formation")
      
      

      
    ),
    mainPanel(
      plotlyOutput("distPie"),
      plotlyOutput("distPieGlobal"),
      plotlyOutput("distPieSec"),
      
      
      
      plotOutput("Histograma"),
      plotOutput("Histograma2")
      
      
    )
  )
))
