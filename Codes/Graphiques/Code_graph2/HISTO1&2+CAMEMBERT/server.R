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
library(dplyr)
library(tidytable)



datos <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_etudiants.csv",dec = ",")

datosR <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_recruteurs.csv",sep = ";", na.strings=c(""," ") )


shinyServer(function(input, output) {

  
  output$Histograma <- renderPlot({
    
    # le top 5 des formations qui ont accueillies le + d?alternants (cont prof et apprentis) depuis 2015-2016

    
    datos1 <- filter(datos, datos$Regime_inscription == "apprentis" | datos$Regime_inscription == "cont prof" )
    
    #first get counts of deaths
    formationcount <- as.data.frame(table(datos1$Formation))
    
    #next put them in decreasing order
    topfive <- formationcount[order(formationcount$Freq, decreasing=T)[1:5],]
    
    #cool, so rhinos are dangerous mofos. Let's plot these results
   ggplot(topfive, aes(x=Var1, y=Freq)) + geom_bar(stat="identity",fill="#38B502", color = "black", width = 0.70) + geom_text(aes(label = Freq),   position=position_stack(vjust = 0.5), colour = "white", size = 8) + 
     ggtitle(" Les 5 formations qui ont accueillies le plus d'alternants depuis 2015/2016") + labs(x = "Nom de la formation", y = "Nombre d'alternants accueillis depuis 2015/2016") +
     theme(plot.title = element_text(color="#A42300", size=18, hjust = 0.5))
   
    
    

   
    
    
  })
  
  output$Histograma2 <- renderPlot({
    
    #  le top 5 des formations qui ont accueillies le - d?alternants (cont prof et apprentis)  depuis 2015-2016
    
    
    datos1 <- filter(datos, datos$Regime_inscription == "apprentis" | datos$Regime_inscription == "cont prof" )
    
    #first get counts of deaths
    formationcount <- as.data.frame(table(datos1$Formation))
    
    #next put them in decreasing order
    lastfive <- formationcount[order(formationcount$Freq, decreasing=F)[1:5],]
    
    #cool, so rhinos are dangerous mofos. Let's plot these results
    ggplot(lastfive, aes(x=Var1, y=Freq)) + geom_bar(stat="identity",fill="#E99212", color = "black", width = 0.70) + geom_text(aes(label = Freq), position=position_stack(vjust = 0.5), colour = "white", size = 8) +
      ggtitle(" Les 5 formations qui ont accueillies le moins d'alternants depuis 2015/2016") + labs(x = "Nom de la formation", y = "Nombre d'alternants accueillis depuis 2015/2016") +
      theme(plot.title = element_text(color="#A42300", size=18, hjust = 0.5))

    
    
    

    
    
    
  })
  
  
  output$distPie <- renderPlotly({
    
    
    getudiants1 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof"), Annee == input$anInput, Formation== input$formation2)
    gendercount <- as.data.frame(table( getudiants1$Sexe))
    

    
    
    plot_ly(gendercount,values=~Freq,labels=~factor(gendercount$Var1),marker=list(colors=c("#FDC591","#91D8FD")),type="pie")
  
})

  output$indirect_select_formation <- renderUI({
    getudiants1 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof" ), Annee==input$anInput)
    
    selectInput(
      inputId = "formation2",
      label = "Sélectionner une formation",  # Give the input a label to be displayed in the app
      choices = getudiants1$Formation
      
    )
  })
  
  
  output$distPieGlobal <- renderPlotly({
    
    
    datosGlobal <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof"))
    
    genderglobalcount <- as.data.frame(table(datosGlobal$Sexe))
    
    plot_ly(genderglobalcount,values=~Freq,labels=~factor(genderglobalcount$Var1),marker=list(colors=c("#FDC591","#91D8FD")),type="pie",title_text='Revenue', title_x=0.5) 
    
  })
  
  
  
})
  
  
  
