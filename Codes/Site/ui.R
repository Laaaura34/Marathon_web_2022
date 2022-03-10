library(shiny)
library(shinythemes)
library(ggplot2)
library(nortest)
library(leaflet)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(plotly)
library(rsconnect)
library(plyr)
library(tidyverse)
# Bringing in the DataTables package to display the data in a nice format
library(DT)



datos <-read.csv("D:/Marathon_web_2022/Codes/Site/www/donnees_etudiants.csv",dec = ",", encoding = "UTF-8")
recruteurs <-read.csv("D:/Marathon_web_2022/Codes/Site/www/donnees_recruteurs_VF.csv",dec = ",", encoding = 'UTF-8')

shinyUI(fluidPage(
  tags$style( type='text/css',
              
              
              "#title { 
                    
                      margin: auto; 
                      width: 50%; 

                      }

                      #panel{
                      color:white;
                      height: 50px;
                      }

                      hr {border-top: 1px solid #d7d7d7;}
                      
                      "
      
  ),

  tags$div(id="title", titlePanel(div(tags$img(src = "affiche.png", height = "10%", width= "90%")))),
  
  navbarPage(header="",title=NULL ,collapsible = TRUE,fluid = TRUE,inverse = TRUE, 
             tags$head(
               
               tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                         
                            height: 55px;
                            
                            }
                           .navbar {min-height:30px;
                                             color: white;
                                             font-family: @import url("https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap");
                           font-size: 18px;
                           border-radius: 15px;}
                                            '),
               )
             ),
             
             tabPanel(icon("home"),
                      
                      
                      fluidRow(
                        
                        br(),
                        p("L’Alternance est un rythme d’études spécifique qui intègre des phases d’apprentissage à l’Université et 
                                                   des phases d’applications pratiques au sein d’organisations professionnelles : entreprises privées, entreprises/entités publiques, 
                                                   associations, GIP, EPIC, ONG, etc.",style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap');
              text-align:center;color:black;font-size:17px;padding:15px;border-radius:10px; width:50%; margin: auto;"),
                        br(),
                        
                        
                        
                        fluidRow(
                          column(12, align="center",
                                 div(style="display: inline-block;","L’Université Paul Valéry propose plus de soixante formations en alternance depuis 2016. 
                                                                 L’UPVM3 permet une professionnalisation des cursus à travers : 
                                                                   - la préparation d’un diplôme dans le cadre d’une formation organisée en alternance (contrat d’apprentissage ou de professionnalisation),
                                                                 - l’expérience professionnelle acquises lors de stage réalisés dans le milieu socio-économique en lien avec les formations des étudiants.",style="text-align:center;font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap');margin: auto;font-size:15px;width:40%;color:black;background-color:#82C6BD;padding:15px;border-radius:10px"),
                                 div(style="display: inline-block;", "Vous trouverez sur ce site trois onglets Étudiants, Recruteurs et Formation. 
Si vous êtes étudiant, nous vous proposons des statistiques afin de vous orienter dans vos recherches d’alternance. 
Si vous êtes recruteurs, vous aurez a disposition des informations sur la compatibilité entre votre entreprise et les formations proposées par l’UMPV3.
L’onglet Formation comprend des statistiques sur l’évolution des alternants depuis 2016.",style="text-align:center;font-size:15px;font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap');margin: auto;width:40%;color:black;background-color:#82C6BD;padding:15px;border-radius:10px")
                                 
                                 
                                 
                          )),
                        
                        br(),
                        
                        br(),
                        
                        
                        
                        p("Dans le cadre du Marathon du web 2022, une équipe constituée de 8 étudiants de master 1 CNO & MIASHS ont participé au projet « Les chiffres de l’Alternance à l’université Paul Valéry Montpellier 3 ». 
Les étudiants ont conçu cet outil dans le but de révéler les chiffres clés de l’alternance à l’UPVM3. 
Ces indicateurs répondront aux interrogations des étudiants et des recruteurs potentiels et ainsi d’apporter une aides sur les recherches de contrats de ces derniers.
                                                                 C’est sur la base des données de l’UMPV3 que les étudiants en MIASHS ont pu analyser et établir des statistiques afin de visualiser et de mettre en lumière les chiffres clés de l’alternance. 

De leur côté les étudiants en CNO ont du établir des supports de communication afin de valoriser l’alternance. 

Nous vous présentons ici un prototype du site qui évoluera dans le but d’être publié officiellement. ",style="font-size:15px;text-align:center;font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap');margin: auto;width:90%;color:black;background-color:#FAE9C3;padding:15px;border-radius:10px"),
                        
                      ),
                      

                      br(),
                      hr(),

                      fluidRow(
                        column(12, align="center",
                               div(style="display: inline-block;",img(src="70.png", height="300px", width="300px",)),
                               div(style="display: inline-block;",img(src="315.png", height="300px", width="300px")),
                               div(style="display: inline-block;",img(src="3.png", height="300px", width="320px")),
                               div(style="display: inline-block;",img(src="4.png", height="300px", width="320px")),
                               
                        )

                      )),
                               tabPanel("Étudiants", id="panel",
                                        
                                        tabsetPanel(
                                          
                                          tabPanel("Total",
                                                   br(),
                                                   br(),
                                                   p('Ici, le nombre d’alternants par formation par année, passez la souris pour voir le détail.
'),
                                                   br(),
                                                   tabsetPanel(
                                                     
                                                     tabPanel("Par année", 
                                                              
                                                              titlePanel(div("Nombre d'alternants",style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                              sidebarLayout(
                                                                sidebarPanel(
                                                                  selectInput(inputId = "anInput2", 
                                                                              label = "Sélectionner une année : ", 
                                                                              choices = c("2016-2017"="2017", "2017-2018"="2018", "2018-2019"="2019", "2019-2020"="2020", "2020-2021"="2021",
                                                                                          "2021-2022"="2022"),
                                                                              selected = c("2016-2017"="2017"),
                                                                  )
                                                                ),
                                                                mainPanel(
                                                                  plotlyOutput("plot", height="1700px", width ="100%")
                                                                ))
                                                     ),
                                                     tabPanel("Global",
                                                              
                                                              titlePanel(div("Nombre d'alternants",style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                              
                                                              mainPanel(
                                                                plotlyOutput("plot1", height="1700px", width ="120%")
                                                              )
                                                              
                                                     )
                                                     
                                                   )),
                                          tabPanel("Genres", 
                                                   br(),
                                                   br(),
                                                   p("Ici, la proportion hommes et femmes des alternants 
par années et formations et au global, passez la souris pour voir le détail."),
                                                   br(),
                                                   tabsetPanel(
                                                     tags$style(
                                                       HTML("
                                         
                                        
                                        .tabbable > .nav > li   > a {
  color: black;
  text-decoration: none;
  font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap');
  font-size: 15px;
}
                                        
                                          .tabbable > .nav > li[class=active]    > a {background-color: #ffdc91; color:black; font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap');
  font-size: 15px;}
                                        
                                        ")),
                                                     tabPanel("Par année ou par formation", 
                                                              titlePanel(div("Genres des alternants",style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                              
                                                              sidebarLayout(
                                                                sidebarPanel(
                                                                  selectInput(inputId = "anInput1", 
                                                                              label = "Sélectionner une année : ", 
                                                                              choices = c("2016-2017"="2017", "2017-2018"="2018", "2018-2019"="2019", "2019-2020"="2020", "2020-2021"="2021",
                                                                                          "2021-2022"="2022"),
                                                                              selected = c("2016-2017"="2017")),
                                                                  
                                                                  
                                                                  uiOutput("indirect_select_formation")
                                                                ),
                                                                mainPanel(
                                                                  plotlyOutput("distPie")

                                                                )
                                                              
                                                              ),
                                                     ),
                                                     
                                                     
                                                     tabPanel("Global",
                                                              titlePanel(div("Genres des alternants",style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                              
                                                              
                                                              
                                                              mainPanel(
                                                                
                                                                
                                                                plotlyOutput("distPieGlobal")
                                                                
                                                              )
                                                     )
                                                     
                                                   )    
                                                   
                                          ),
                                          
                                          tabPanel("Nationalités", 
                                                   br(),
                                                   br(),
                                                   p('Ici, la répartition de la nationalité des alternants, passez la souris pour voir le détail.'),
                                                   br(),
                                                   br(),
                                                   hr(),
                                                   
                                                   
                                                   titlePanel(div("Nationalités des alternants", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                   
                                                   
                                                   mainPanel(
                                                     
                                                     plotlyOutput("distPieNatio"),
                                                     
                                                     
                                                     
                                                     
                                                   )),
                                          
                                        
                                        tabPanel("Localisation", 
                                                 br(),
                                                 br(),
                                                 p("Ici, la localisation géographique à l’inscription des alternants par années, cliquez sur les marqueurs et zoomez pour voir le détail."),
                                                 br(),
                                                 br(),
                                                 hr(),
                                                   titlePanel(div("Localisation des alternants", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                   sidebarLayout(
                                                     sidebarPanel(
                                                       selectInput(inputId = "anInput3", 
                                                                   label = "Sélectionner une année : ", 
                                                                   choices = c("2016-2017"="2017", "2017-2018"="2018", "2018-2019"="2019", "2019-2020"="2020", "2020-2021"="2021", "2021-2022"="2022")
                                                       ),
                                                       ),
                                                     mainPanel(
                                                       leafletOutput("mymap1")
                                                       
                                                     )
                                                   
                                                   
                                                   
                                                 )
                                                 
                                        )
                                        )
                             ),
                             
                             tabPanel("Recruteurs",
                                      
                                      tabsetPanel(
                                        tabPanel("Secteurs juridiques des entreprises",
                                                 br(),
                                                 br(),
                                                 p('Ici, le nombre de structures par formation et filtré par années et au global (toutes années confondues), passez la souris pour voir le détail '),
                                                 br(),
                                                 br(),
                                                 tabsetPanel(
                                                   
                                                   tabPanel("Par année", 
                                                            titlePanel(div("Secteurs juridiques par formations", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                            
                                                            sidebarLayout(
                                                              sidebarPanel(
                                                                selectInput(inputId = "anInputa", 
                                                                            label = "Sélectionner une année : ", 
                                                                            choices = c("2016-2017"="2016-2017", "2017-2018"="2017-2018", "2018-2019"="2018-2019", "2019-2020"="2019-2020", "2020-2021"="2020-2021",
                                                                                        "2021-2022"="2021-2022"),
                                                                            selected = c("2016-2017"="2016-2017")),
                                                                
                                                                                                                              ),
                                                              mainPanel(
                                                                plotlyOutput("Histograma", height = "1000px", width = "100%"),
                                                                
                                            
                                                              )
                                                              
                                                              
                                                            ),
                                                   ),
                                                   
                                                   
                                                   tabPanel("Global",
                                                            titlePanel(div("Secteurs juridiques au global", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                        
                                                            mainPanel(
                                                              
                                                              plotlyOutput("distPieGlobal1")

                                                            )
                                                   )
                                                   
                                                 )    
                                        ),
                                        tabPanel("Entreprises", 
                                                 br(),
                                                 br(),
                                                 p("Ici, le top 10 des structures les plus accueillantes, passez la souris pour voir le détail "),
                                                 br(),
                                                 br(),
                                                 hr(),

                                                            titlePanel(div("Top 10 des recruteurs", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                            
                                                          
                                                              mainPanel(
                                                                plotlyOutput("recrut1", height = "1000px", width = "100%"),
                                                                
                                                                
                                                              )
                                                    
                                                 ),
                                        tabPanel("Localisation", 
                                                 br(),
                                                 br(),
                                                 p("Ici, la localisation géographique des recruteurs filtrée par années et par formations, cliquez sur les marqueurs et zoomez pour voir le détail."),
                                                 br(),
                                                 br(),
                                                 hr(),
                                                   titlePanel(div("Localisation des entreprises", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                   br(),
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                       selectInput(inputId = "anInputb", 
                                                                   label = "Sélectionner une année : ", 
                                                                   choices = c("2016-2017"="2016-2017", "2017-2018"="2017-2018", "2018-2019"="2018-2019", "2019-2020"="2019-2020", "2020-2021"="2020-2021"),
                                                       ),
                                                       uiOutput("indirect_select_formationA")
                                                       
                                                       
                                                       
                                                     ),
                                                     mainPanel(
                                                       leafletOutput("mymap"),
                                                       br(),
                                                     )
                                                   
                                                 )
                                        )
                                      )
                                    ),
                             tabPanel("Formation",
                                      
                                      tabsetPanel(
                                        tabPanel("Top 5", 
                                                 br(),
                                                 br(),
                                                 p("Top 5 des formation les plus accueillantes depuis 2016"),
                                                 br(),
                                                 br(),
                                                 hr(),
                                                 
                                                 titlePanel(div("Top 5 des formations", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                 
                                                 mainPanel( 
                                                   plotOutput("Histograma5", height="1000px", width="100%")
                                                   
                                                 )),
                                        
                                        tabPanel("Évolutions", 
                                                 br(),
                                                 br(),
                                                 p("Ici, Evolution du nombre d’alternants par années, filtrée par formation et en global (toutes formations confondues), passez la souris pour voir le détail"),
                                                 br(),
                                                 br(),
                                                 # Application title
                                                 tabsetPanel(
                                                   
                                                   tabPanel("Par formation", 
                                                            titlePanel(div("Évolution de la tendance d'alternants", style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                            
                                                            # Sidebar with a slider input for number of bins
                                                            sidebarLayout(
                                                              sidebarPanel(
                                                                uiOutput("indirect_select_formation3")
                                                              ),
                                                              mainPanel(
                                                                plotlyOutput("distPlot2")
                                                              )
                                                            )),
                                                   
                                                   tabPanel("Global", 
                                                            titlePanel(div("Évolution de la tendance d'alternants en global",style="font-family: @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap'); text-align:center; ")),
                                                            
                                                            mainPanel(
                                                              
                                                              div(plotlyOutput('recrut2', height = "600px", width = '100%'), align = "center")
                                                            )
                                                            
                                                   ))
                                        )
                                        
                                        
                                        ))
                             
                             
                             
                             
                             
                                      
                             ),
                             
                             
                             
                  )
)