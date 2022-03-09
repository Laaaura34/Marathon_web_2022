
library(shiny)
library(plyr)
library(plotly)
library(scales)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(tidyverse)
library(hrbrthemes)


etudiants <- read.csv(file = 'D:/Marathon_web_2022/Codes/Graphiques/data/donnees_etudiants.csv', dec = ",", encoding="UTF-8")
recruteurs <- read.csv(file = 'D:/Marathon_web_2022/Codes/Graphiques/data/donnees_recruteurs.csv', dec = ",", encoding="UTF-8")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  trend_data <- reactive({
    
    # `validate()` is additional; to prepare friendly message for error
    validate(
      need(input$anInput != " ", "Rentrez des données s'il vous plait.")
    )
    
    etudiants %>%
      filter(Regime_inscription %in% c("apprentis", "cont prof" ), Annee==input$anInput) 
    
    
  })
  
  
  output$plot <- renderPlotly({

    plot_line <- trend_data() %>%
 
    ggplot(aes(x = Formation)) +
      geom_histogram(stat = 'count')+
      theme(axis.text.x = element_text(angle=65, hjust=1) , plot.title = element_text(color='#A42300', size = 18, hjust = 0.5)) +
      labs(x= 'Formations', "Nombre d'étudiants") +
      ggtitle("Le nombre d'alternant dans chaque formations année par année")
    
    ggplotly(plot_line) %>%
      layout(title = list(x = 0.5)) %>% # adjust title to the center
      config(displayModeBar = F)
  })
  
  
  
  etudiants1 <- filter(etudiants, Regime_inscription %in% c("apprentis", "cont prof" )) 
  
  output$plot1 <- renderPlotly({
    input$bouton
    plot_line1 <- ggplot(etudiants1, aes(x = Formation)) +
      geom_histogram(stat = 'count')+
      theme(axis.text.x = element_text(angle=65, hjust=1) , plot.title = element_text(color='#A42300', size = 18, hjust = 0.5)) +
      labs(x= 'Formations', "Nombre d'étudiants") +
      ggtitle("Le nombre d'alternant dans chaque formations au global")
    
    ggplotly(plot_line1)
    
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
