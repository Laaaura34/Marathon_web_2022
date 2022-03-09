#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(DT)
library(plotly)
library(shiny)
library(dplyr)
library(shinyWidgets)



etudiants <-read.csv("/Users/lisabeteille/Desktop/Marathon_web_2022/Codes/Graphiques/data/donnees_etudiants.csv",dec = ",")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$distPlot <- renderPlotly({
    
    etudiants1 <- filter(etudiants, Regime_inscription %in% c("apprentis", "cont prof"), Formation == input$inputform)
    
    
    etu <- aggregate(etudiants1$Formation, by=list(etudiants1$Annee), FUN = length) 
    
    ggplot(etu, aes(x=Group.1, y=x)) +
      geom_line() + 
      scale_x_continuous(name="Annee", limits=c(2016,2022)) + 
      scale_y_continuous(name="Evolution", limits=c(0, 40))
    
  })
  
  
  output$indirect_select_formation <- renderUI({
    etu <- filter(etudiants, Regime_inscription %in% c("apprentis", "cont prof" ))
    
    selectInput(
      inputId = "inputform",
      label = "Sélectionner une formation",  # Give the input a label to be displayed in the app
      choices = c('DUT CS Opt Serv.pers.1è a'='DUT CS Opt Serv.pers.1è a', 'DUT CS Opt Serv.pers.2è a'='DUT CS Opt Serv.pers.2è a',
                            'L3 AES Pcs DS'='L3 AES Pcs DS', 'LP IS Pcs Gest. géronto'='LP IS Pcs Gest. géronto',
                            'LP MI Pcs Dis.info nm EAD'='LP MI Pcs Dis.info nm EAD', 'LP MI Pcs Dispos.info num'='LP MI Pcs Dispos.info num',
                            'LP Mét. animation soc.'='LP Mét. animation soc.', 'M1 DC Pcs Gest. info-doc'='M1 DC Pcs Gest. info-doc',
                            'M1 DC Pcs Info-doc EAD'= 'M1 DC Pcs Info-doc EAD', 'M1 DC Pcs Médiat.doc. EAD'='M1 DC Pcs Médiat.doc. EAD',
                            'M1 DC Pcs Médiation doc.'='M1 DC Pcs Médiation doc.', 'M1 DC Pcs Val.inf num EAD'='M1 DC Pcs Val.inf num EAD',
                            'M1 DC Pcs Valor.info num'='M1 DC Pcs Valor.info num', 'M1 GRH Pcs Ress. humaines'='M1 GRH Pcs Ress. humaines',
                            'M1 HN Pcs Ingén. sav. EAD'='M1 HN Pcs Ingén. sav. EAD', 'M1 HN Pcs Médiat. num EAD'='M1 HN Pcs Médiat. num EAD',
                            'M1 IC Pcs Chang orga pers'='M1 IC Pcs Chang orga pers', 'M1 IC Pcs Com. Numérique'='M1 IC Pcs Com. Numérique',
                            'M1 IC Pcs Com. numériques'='M1 IC Pcs Com. numériques', 'M1 IC Pcs Com.publique'='M1 IC Pcs Com.publique',
                            'M1 IC Pcs Com.publiques'='M1 IC Pcs Com.publiques', 'M1 IC Pcs Strat.du changt'='M1 IC Pcs Strat.du changt',
                            'M1 IDS Pcs Dév. social'='M1 IDS Pcs Dév. social', 'M1 IDS Pcs Interv. & dév.'='M1 IDS Pcs Interv. & dév.', 'M1 MIASHS'='M1 MIASHS',
                            'M1 TO Pcs Tour.& dév. dur'='M1 TO Pcs Tour.& dév. dur', 'M1 Éco. Sociale Solidaire'='M1 Éco. Sociale Solidaire',
                            'M2 ASV Pcs Théâtre sp.viv'='M2 ASV Pcs Théâtre sp.viv', 'M2 CN Pcs Mét.de la prod'='M2 CN Pcs Mét.de la prod',
                            'M2 CN Pcs Métiers diffus.'='M2 CN Pcs Métiers diffus.', 'M2 CN Pcs Métiers prod.'='M2 CN Pcs Métiers prod.',
                            'M2 DC Pcs Médiat.doc. EAD'='M2 DC Pcs Médiat.doc. EAD', 'M2 DC Pcs Médiation doc.'='M2 DC Pcs Médiation doc.',
                            'M2 DC Pcs Val.inf num EAD'='M2 DC Pcs Val.inf num EAD', 'M2 DC Pcs Valor.info num'='M2 DC Pcs Valor.info num',
                            'M2 GRH Pcs Audit social'='M2 GRH Pcs Audit social', 'M2 GRH Pcs Exp. socio-éco'='M2 GRH Pcs Exp. socio-éco',
                            'M2 GRH Pcs GRH sector.'='M2 GRH Pcs GRH sector.', 'M2 GRH Pcs Mang intern.RH'='M2 GRH Pcs Mang intern.RH',
                            'M2 GRH Pcs Sc. organisat.'='M2 GRH Pcs Sc. organisat.', 'M2 Gest. environnement'='M2 Gest. environnement',
                            'M2 HN Pcs Ingén. sav. EAD'='M2 HN Pcs Ingén. sav. EAD', 'M2 HN Pcs Médiat. num EAD'='M2 HN Pcs Médiat. num EAD',
                            'M2 IC Pcs Chang orga pers'='M2 IC Pcs Chang orga pers', 'M2 IC Pcs Com. Numérique'='M2 IC Pcs Com. Numérique',
                            'M2 IC Pcs Com. numériques'='M2 IC Pcs Com. numériques', 'M2 IC Pcs Com.publique'='M2 IC Pcs Com.publique',
                            'M2 IC Pcs Com.publiques'='M2 IC Pcs Com.publiques', 'M2 IC Pcs Strat.du changt'='M2 IC Pcs Strat.du changt',
                            'M2 IDS Pcs Eco soc.&solid'='M2 IDS Pcs Eco soc.&solid', 'M2 IDS Pcs Projets, innov'='M2 IDS Pcs Projets, innov',
                            'M2 MIASHS'='M2 MIASHS', 'M2 Psycho soc. du travail'='M2 Psycho soc. du travail', 'M2 SS Pcs Asso,coop & act'='M2 Psycho soc. du travail',
                            'M2 TO Pcs Tour.& dév. dur'='M2 TO Pcs Tour.& dév. dur'), selected = " ")  # Create the choices that can be selected. e.g. Display "A" and link to value "a"
  

      
    
  })
  
  
  
})