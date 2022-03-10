
    shinyUI(fluidPage(
        titlePanel("Nombre alternants"),
        sidebarLayout(
            sidebarPanel(
                selectInput(inputId = "anInput", 
                               label = "Sélectionner une année : ", 
                               choices = c("2016-2017"="2016-2017", "2017-2018"="2017-2018", "2018-2019"="2018-2019", "2019-2020"="2019-2020", "2020-2021"="2020-2021"),
                ),
                uiOutput("indirect_select_formation")
                
                
                
            ),
            mainPanel(
                leafletOutput("mymap")
                
            )
        )
    ))
    
