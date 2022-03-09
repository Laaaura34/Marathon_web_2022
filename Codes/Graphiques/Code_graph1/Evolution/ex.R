library(tidyverse)
library(hrbrthemes)
library(viridis)
library(DT)
library(plotly)
library(shiny)
library(dplyr)

etudiants <- read.csv(file = 'D:/Marathon_web_2022/Codes/Graphiques/data/donnees_etudiants.csv', dec = ",", encoding="UTF-8")

etudiants1 <- filter(etudiants, Regime_inscription %in% c("apprentis", "cont prof"))


etu <- aggregate(etudiants1$Formation, by=list(etudiants1$Annee), FUN = length)

ggplot(etu, aes(x=Group.1, y=x)) +
    geom_line()+
    labs(x='Année', y="Nombre d'alternants")
  
