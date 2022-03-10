library(shiny)
library(shinythemes)
library(ggplot2)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(plotly)
library(rsconnect)
library(hrbrthemes)
library(leaflet)
library(plyr)
library(tidyverse)
# Bringing in the DataTables package to display the data in a nice format
library(DT)



datos <-read.csv("D:/Marathon_web_2022/Codes/Site/www/donnees_etudiants.csv",dec = ";", encoding = 'UTF-8')
recruteurs <-read.csv("D:/Marathon_web_2022/Codes/Site/www/donnees_recruteurs_VF.csv",dec = ",", encoding = 'UTF-8', na.strings=c(""," ") )
recruteurs1 <-read.csv("D:/Marathon_web_2022/Codes/Site/www/donnees_recruteurs_VF.csv",dec = ",", encoding = 'UTF-8', na.strings=c(""," ") )

shinyServer(function(input, output) {

  
  # Graphique sur les genres avec filtre
  
  output$distPie <- renderPlotly({
    
    getudiants1 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof" ), Annee==input$anInput1, Formation==input$formation1)
    
    gendercount <- as.data.frame(table(getudiants1$Sexe))
    
    
    
    plot_ly(gendercount,values=~Freq,labels=~factor(gendercount$Var1),marker=list(colors=c("#F76987","#c63e27")),type="pie")
    
  })
  #Filtre formation 
  
  output$indirect_select_formation <- renderUI({
    getudiants1 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof" ), Annee==input$anInput1)
    
    selectInput(
      inputId = "formation1",
      label = "Sélectionner une formation",  # Give the input a label to be displayed in the app
      choices = getudiants1$Formation
      
    )
  })
  
  # Graphique sur les genres en global
  
  output$distPieGlobal <- renderPlotly({
    
    
    datosGlobal <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof"))
    
    genderglobalcount <- as.data.frame(table(datosGlobal$Sexe))
    
    plot_ly(genderglobalcount,values=~Freq,labels=~factor(genderglobalcount$Var1),marker=list(colors=c("#F76987","#c63e27")),type="pie",title_text='Revenue', title_x=0.5) 
    
  })
  
  #Graphique nationalités
  
  output$distPieNatio <- renderPlotly({
    
    
    frnationality <- as.data.frame(table(datos$Nationalite == "FRANCAIS(E)"))
    
    frcount <- as.data.frame(rbind(frnationality[2,2], frnationality[1,2]))
    names(frcount)
    plot_ly(frcount,values=~V1,labels=~c("Nationalité française","Nationalité étrangère"),marker=list(colors=c("#34897E","#4CC9B9")),type="pie")
    
  })
  
  
  #Message d'erreur quand graphique vide
  
  trend_data <- reactive({
    
    # `validate()` is additional; to prepare friendly message for error
    validate(
      need(input$anInput2 != " ", "Rentrez des données s'il vous plait.")
    )
    
    datos %>%
      filter(Regime_inscription %in% c("apprentis", "cont prof" ), Annee==input$anInput2) 
    
    
  })
  
  
  #Plot nombre total  alternants filtre année
  
  output$plot <- renderPlotly({
    
    plot_line <- trend_data() %>%
      
      ggplot(aes(x = Formation)) +
      geom_histogram(stat = 'count', fill='#157266')+
      theme(axis.text.x = element_text(angle=65, hjust=1) , plot.title = element_text(color='#A42300', size = 18, hjust = 0.5)) +
      labs(x= 'Formations', y= "Nombre d'étudiants") 
    ggplotly(plot_line) %>%
      layout(title = list(x = 0.5)) %>% # adjust title to the center
      config(displayModeBar = F)
  })
  
  
  
  #Plot nombre total d'alternant global 
  
  etudiants1 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof" )) 
  
  output$plot1 <- renderPlotly({
    input$bouton
    plot_line1 <- ggplot(etudiants1, aes(x = Formation)) +
      geom_histogram(stat = 'count', fill = '#157266')+
      theme(axis.text.x = element_text(angle=65, hjust=1) , plot.title = element_text(color='#A42300', size = 18, hjust = 0.5)) +
      labs(x= 'Formations', y= "Nombre d'étudiants") 
    ggplotly(plot_line1)
    
  })
  
  
  ###### CARTE ETUDIANTS 
  output$mymap1 <- renderLeaflet({
    
    etudiants1 <- filter(datos,  Regime_inscription %in% c("apprentis", "cont prof" ), Annee==input$anInput3)
    
    
    datos<- etudiants1 %>% 
      mutate(Lat=as.numeric(str_replace(Lat,",","."))) %>% 
      mutate(Long=as.numeric(str_replace(Long,",","."))) 
    
    # Using the datatable function from the DT package to see the first 6 rows of data
    datatable(head(datos))
    
    
    carte_groupe1 <- leaflet()
    carte_groupe1 <- addTiles(carte_groupe1)
    #On ajoute éles marqueurs.
    carte_groupe1 <- addMarkers(
      map = carte_groupe1,
      lng = datos$Long,
      lat = datos$Lat,
      popup = paste("Statut: Alternant"),
      clusterOptions = markerClusterOptions()
    )
    
    #On ajoute un bouton qui permet de revenir au zoom initial. Le niveau utilisé est 4 et le bouton est en forme de planète.
    carte_groupe1 <- addEasyButton(
      map = carte_groupe1,
      button = easyButton(
        icon = "fa-globe",
        title = "Zoom initial",
        onClick = JS("function(btn, map){ map.setZoom(4); }")
      )
    )
    carte_groupe1
    
    
    
    
  })
  
  
  output$Histograma <- renderPlotly({
    
    datavide=recruteurs[complete.cases(recruteurs), ]
    
    datos1 <- filter(datavide, Annee == input$anInputa)
    
    formecount <- as.data.frame(table(datos1$FORME_JURIDIQUE_ENTREPRISE,  datos1$FORMATION_COMPLET ))
    Nombre = formecount$Freq
    Structure = formecount$Var1
    Formation = formecount$Var2 
    
    form2<- formecount[order(formecount$Freq, decreasing=T),]
    
    ggplot(form2, aes(fill=Structure, y=Nombre, x=Formation)) +
      geom_bar(position="stack", stat="identity")+ 
      theme(axis.text.x = element_text(angle=80)) + labs(x = "Formations", y = "Nombre de structures par secteur juridique")
    
  })
  
  output$distPieGlobal1 <- renderPlotly({
    
    datavide1=recruteurs[complete.cases(recruteurs), ]
    
    datos3 <- filter(datavide1)
    
    formecount <- as.data.frame(table(datos3$Type))
    Nombre = formecount$Freq
    Structure = formecount$Var1#EC7860
    
    plot_ly(formecount,values=~Freq,labels=~factor(formecount$Var1),marker=list(colors=c("#C70039","#FF9957", "#EC7860")),type="pie") 

  })
  
  output$recrut1 <- renderPlotly ({
    # write code here to read data from csv file
    aprenticecount <- as.data.frame(table(recruteurs$NOM_ENTREPRISE))
    
    #top 10
    
    top10<- aprenticecount[order(aprenticecount$Freq, decreasing=T)[1:10],]
    Effectif = top10$Freq
    Structure = top10$Var1

    
    # Set x and y axis and display data in bar chart using plotly
    ggplot(top10, aes(x= reorder(Structure, Effectif), y=Effectif)) + 
      geom_col(fill="#157266") +
      theme(axis.text.x = element_text(angle=45, hjust=1), plot.title = element_text(color='#A42300', size = 18, hjust = 0.5)) +
      scale_fill_ipsum() +
      coord_flip() +
      labs(x = "Structure") 
    
    
  })
  
  ##### CARTE RECRUTEURS
  
  output$mymap <- renderLeaflet({
    
    data1 <- filter(recruteurs1, Annee==input$anInputb, FORMATION_COMPLET==input$formationa)
    
    recruteurs1<- data1 %>% 
      mutate(Lat=as.numeric(str_replace(Lat,",","."))) %>% 
      mutate(Long=as.numeric(str_replace(Long,",","."))) 
    
    # Using the datatable function from the DT package to see the first 6 rows of data
    datatable(head(recruteurs1))
    
    
    carte_groupe <- leaflet()
    carte_groupe <- addTiles(carte_groupe)
    #On ajoute éles marqueurs.
    carte_groupe <- addMarkers(
      map = carte_groupe,
      lng = recruteurs1$Long,
      lat = recruteurs1$Lat,
      popup = paste(recruteurs1$NOM_ENTREPRISE),
      clusterOptions = markerClusterOptions()
    )
    
    #On ajoute un bouton qui permet de revenir au zoom initial. Le niveau utilisé est 4 et le bouton est en forme de planète.
    carte_groupe <- addEasyButton(
      map = carte_groupe,
      button = easyButton(
        icon = "fa-globe",
        title = "Zoom initial",
        onClick = JS("function(btn, map){ map.setZoom(4); }")
      )
    )
    carte_groupe
    
    
    
    
  })
  
  observe({
    print(input$locInput)
  })
  
  
  output$indirect_select_formationA <- renderUI({
    data <- filter(recruteurs1, Annee==input$anInputb)
    
    selectInput(
      inputId = "formationa",
      label = "Sélectionner une formation",  # Give the input a label to be displayed in the app
      choices = unique(data$FORMATION_COMPLET),

    )
  })
  
  # le top 5 des formations qui ont accueillies le + d?alternants (cont prof et apprentis) depuis 2015-2016
  
  output$Histograma5 <- renderPlot({
    
    
    
    datos1 <- filter(datos, datos$Regime_inscription == "apprentis" | datos$Regime_inscription == "cont prof" )
    
    #first get counts of deaths
    formationcount <- as.data.frame(table(datos1$Formation))
    
    #next put them in decreasing order
    topfive <- formationcount[order(formationcount$Freq, decreasing=T)[1:5],]
    
    #cool, so rhinos are dangerous mofos. Let's plot these results
    ggplot(topfive, aes(x=Var1, y=Freq)) + geom_bar(stat="identity",fill="#157266", color = "black", width = 0.70, stackdir = 'center') + geom_text(aes(label = Freq),   position=position_stack(vjust = 0.5), colour = "white", size = 8) + 
      labs(x = "Nom de la formation", y = "Nombre d'alternants accueillis depuis 2015/2016") +
      theme(plot.title = element_text(color="#A42300", size=18, hjust = 0.5), axis.text.x = element_text(angle=45, hjust=1))
 
    
  })


output$distPlot2 <- renderPlotly({
  
  etudiants4 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof"), Formation == input$inputform)
  
  
  etu3 <- aggregate(etudiants4$Formation, by=list(etudiants4$Annee), FUN = length) 
  Annee <- etu3$Group.1
  Effectif <- etu3$x
  ggplot(etu3, aes(x=Annee, y=Effectif)) +
    geom_line() + 
    scale_x_continuous(name="Annee", limits=c(2016,2022)) + 
    scale_y_continuous(name="Evolution", limits=c(0, 40))
  
})

# Filtre formation

output$indirect_select_formation3 <- renderUI({
  etu2 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof" ))
  
  selectInput(
    inputId = "inputform",
    label = "Sélectionner une formation",  # Give the input a label to be displayed in the app
    choices = unique(etu2$Formation))
  
  
})

output$recrut2 <- renderPlotly({
  
  etudiants66 <- filter(datos, Regime_inscription %in% c("apprentis", "cont prof"))
  
  
  etu1 <- aggregate(etudiants66$Formation, by=list(etudiants66$Annee), FUN = length)
  Annee <- etu1$Group.1
  Effectif <-  etu1$x
  
  ggplot(etu1, aes(x=Annee, y=Effectif)) +
    geom_line()+
    labs(x='Année', y="Nombre d'alternants")
  
})
  

})
