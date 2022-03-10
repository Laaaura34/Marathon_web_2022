

shinyUI(fluidPage(

    titlePanel("Localisation des étudiants"),
    sidebarLayout(
        sidebarPanel(
            
            tags$head(
                tags$style(HTML("

      .selectize-input {
        height: 30px;
        width: 200px;
        font-size: 12px;
        padding-top: 5px;
        color: black;
      }
              .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }


    "))
            ),
            selectInput(inputId = "anInput", 
                        label = "Sélectionner une année : ", 
                        choices = c("2016-2017"="2017", "2017-2018"="2018", "2018-2019"="2019", "2019-2020"="2020", "2020-2021"="2021", "2021-2022"="2022")
            ),
            uiOutput("indirect_select_Statut")
        
            
       
            
            
            
            , width = 3),
        mainPanel(
            leafletOutput("mymap")
            
        )
    )
))

