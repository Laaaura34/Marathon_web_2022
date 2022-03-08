#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plyr)
library(plotly)
library(scales)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data

etudiants <- read.csv(file = 'D:/MARATHON_WEB_Group7/Codes/Graphiques/data/donnees_etudiants.csv', dec = ",")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Nombre alternants"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "anInput", 
                        label = "Sélectionner une année : ", 
                        choices = c("2016-2017"="2017", "2017-2018"="2018", "2018-2019"="2019", "2019-2020"="2020", "2020-2021"="2021",
                                    "2021-2022"="2022"),
                        selected = c("2016-2017"="2017", "2017-2018"="2018", "2018-2019"="2019", "2019-2020"="2020", "2020-2021"="2021",
                                     "2021-2022"="2022"),
                        multiple = TRUE
                        ),
            
    
            
        ),
        mainPanel(
            plotOutput("plot"),
            plotOutput('recrut')
        )
    )
))