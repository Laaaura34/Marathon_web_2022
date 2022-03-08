
library(shiny)
library(plyr)
library(plotly)
library(scales)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(tidyverse)
library(hrbrthemes)


etudiants <- read.csv(file = 'D:/MARATHON_WEB_Group7/Codes/Graphiques/data/donnees_etudiants.csv', dec = ",", encoding='UTF-8-BOM')
recruteurs <- read.csv(file = 'D:/MARATHON_WEB_Group7/Codes/Graphiques/data/donnees_recruteurs.csv', dec = ",", encoding='UTF-8')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  output$plot <- renderPlot({
    etudiants1 <- filter(etudiants, Regime_inscription %in% c("apprentis", "cont prof" ), Annee==input$anInput) 
    
    ggplot(etudiants1, aes(x = Formation)) +
      geom_histogram(stat = 'count')+
      theme(axis.text.x = element_text(angle=65, hjust=1) , plot.title = element_text(color='#A42300', size = 18, hjust = 0.5)) +
      labs(x= 'Formations', "Nombre d'étudiants") +
      ggtitle("Le nombre d'alternant dans chaque formations année par année")
  })
  
  
    

  
  
  output$recrut <- renderPlot ({
    # write code here to read data from csv file
    aprenticecount <- as.data.frame(table(recruteurs$NOM_ENTREPRISE))
    
    
    #top 10
    
    top10<- aprenticecount[order(aprenticecount$Freq, decreasing=T)[1:10],]
    
    # Set x and y axis and display data in bar chart using plotly
    ggplot(top10, aes(x= reorder(Var1, Freq), y=Freq)) + 
      geom_col() +
      theme(axis.text.x = element_text(angle=45, hjust=1), plot.title = element_text(color='#A42300', size = 18, hjust = 0.5)) +
      scale_fill_ipsum() +
      coord_flip() +
      labs(x = "Structure") +
      ggtitle("Top 10 des structures qui ont recrutés le plus d’alternants depuis 2016 toutes formations confondues")
      
      
  })

})
