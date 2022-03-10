library(readxl)  
library(dplyr)
library(magrittr)
library(xlsx)
library(tidyr)
#Stockons le chemin ici dans path.

################# 1ere méthode: Créons notre propore fonction #########################

#https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
#################################################
# Fonction pour importer les différentes sheets #
#################################################

read_excel_allsheets <- function(path,type = "One_dataframe", drop_sheets = NULL) {
  if(type == "list_dataframe"){
    
    #Filename: -> repertoire de la base de données à charger.
    #Définir une table temporaire sheets qui recoit l'ensemble des sheets
    sheets <- readxl::excel_sheets(path)
     #Définir une table "x" qui recoit chaque sheet comme un path.  
    x <- lapply(sheets, function(X) readxl::read_excel(path, sheet = X))
    
    #Convertir toutes les tables en "data.frame"
    x <- lapply(x, as.data.frame)
    
    #Les noms des tables sont des lists dans "x"
    #et chacun a le même que le sheets.
    names(x) <- sheets
    
    #La fonction retourne un object x composés de toutes les sheets
    #et chaque sheet un une liste qu'on peut appeler pour afficher sa table.
    return(x)
  }
  
  else if(type == "One_dataframe"){
    sheets = excel_sheets(path)
    sheets <- sheets[!sheets %in% drop_sheets]
    
    df = lapply(setNames(sheets, sheets), function(x) read_excel(path, sheet=x))
    df = bind_rows(df, .id="Sheet")
    df = bind_cols(df)
    
    
    return(df)
  }
  
}

path <- "C:/Users/abdou/bureau/Bureau/Optim/marat_web/donnees_etudiantsr.xlsx"
#readAll_list <- read_excel_allsheets(path, type = "list_dataframe")

readAll_DF <- read_excel_allsheets(path, type = "One_dataframe", 
             drop_sheets = c("2016-2017", "2017-2018","2018-2019","2019-2020",
                             "2021-2022"))

readAll_DF$Sheet <- NULL



data <- read.csv("C:/Users/abdou/bureau/Bureau/Optim/marat_web/donnees_etudiantsr.csv", sep=';')

names(data)

count(data["Sexe"])

ggplot2.histogram(data=data["Sexe"], xName='Sexe')

count(data["Sexe"])

str(data["Sexe"])

table(data$Sexe)

hist(table(data$Nationalite))

names(data)

table(data$Regime.inscription)

tapply(data$Sexe, data$Regime.inscription)
V<-data$Sexe


ggplot(data, aes(x = Formation)) +
  geom_histogram(aes(color = Sexe), fill = "white",
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) 




