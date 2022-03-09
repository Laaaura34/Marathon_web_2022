#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(DT)
library(plotly)
library(shiny)
library(dplyr)
library(shinyWidgets)



etudiants <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_etudiants.csv",dec = ",")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(""),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("indirect_select_formation")
    ),
    mainPanel(
      plotlyOutput("distPlot")
    )
  )
))

