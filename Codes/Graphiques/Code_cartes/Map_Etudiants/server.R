library(leaflet)
library(plyr)
library(tidyverse)
# Bringing in the DataTables package to display the data in a nice format
library(DT)

shinyServer(function(input, output) {
    data <-read_csv(file = "/Users/Aname/Desktop/Marathon/Donnees_Lat_Long/donnees_etudiants.csv")
    
    
    output$mymap <- renderLeaflet({
        
        etudiants1 <- filter(data, Annee==input$anInput, Statut==input$Statut2)
        
        
        data<- etudiants1 %>% 
            mutate(Lat=as.numeric(str_replace(Lat,",","."))) %>% 
            mutate(Long=as.numeric(str_replace(Long,",","."))) 
        
        # Using the datatable function from the DT package to see the first 6 rows of data
        datatable(head(data))
        
        
        carte_groupe <- leaflet()
        carte_groupe <- addTiles(carte_groupe)
        #On ajoute éles marqueurs.
        carte_groupe <- addMarkers(
            map = carte_groupe,
            lng = data$Long,
            lat = data$Lat,
            popup = paste("Statut: ", data$Statut),
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
    
    output$indirect_select_Statut <- renderUI({
        data <- filter(data,  Annee==input$anInput)
        
        selectizeInput(
            inputId = "Statut2",
            label = "Sélectionner un Statut",  # Give the input a label to be displayed in the app
            choices = c("Apprentis"="Apprentis", "Non apprentis"="Non apprentis"),
            selected = c("Apprentis"="Apprentis"),

        )
        
    })
})
