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

datos <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_recruteurs.csv",sep = ";", na.strings=c(""," ") )
datos2 <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_recruteurs.csv",sep = ",", na.strings=c(""," ") )



shinyServer(function(input, output) {
  
  output$Histograma <- renderPlotly({
    
    datavide=datos[complete.cases(datos), ]
    
    
    datos1 <- filter(datavide, Annee == input$anInput)
    
    
  formecount <- as.data.frame(table( datos1$FORME_JURIDIQUE_ENTREPRISE,  datos1$FORMATION ))
  Nombre = formecount$Freq
  Structure = formecount$Var1
  Formation = formecount$Var2 
  
  
  ggplot(formecount, aes(fill=Structure, y=Nombre, x=Formation)) +
    geom_bar(position="stack", stat="identity")+ 
    theme(axis.text.x = element_text(angle=80)) + labs(x = "Formations", y = "Nombre de structures par secteur juridique")
    
  })
  output$distPieGlobal <- renderPlotly({
    
    datavide=datos2[complete.cases(datos2), ]
    
    
    datos3 <- filter(datavide)
    
    formecount <- as.data.frame(table( datos3$Type ))
    Nombre = formecount$Freq
    Structure = formecount$Var1
    
    plot_ly(formecount,values=~Freq,labels=~factor(formecount$Var1),marker=list(colors=c("#FDC591","#91D8FD")),type="pie") 
    
    
    
    
    #il faut demander aux collègues de miashs s'ils peuvent produire un "camembert" avec regroupement les données comme ceci :Privé = Sas+SARL+SA+Sté actions simplifiées unipersonnelle+entreprise individuelle+SA CA+SCA+Société anonyme d’économie mixte
    #Public = Admin état+collectivité territoriale+établissement public+EPA+administration publique générale+commune et commune nouvelle+EPIC+etablissement public national scientif+établissement secondaire+FP territoriale
    #Association = association
  })
  
})