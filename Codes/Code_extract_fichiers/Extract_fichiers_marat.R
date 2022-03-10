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

path <- "Fichier_de_mesure_PALOURDE FINAL_corrige_des coquilles vides.xlsx"
#readAll_list <- read_excel_allsheets(path, type = "list_dataframe")

# application de la fonction pour obtenir une base formée par les strates qui ont des replicats
readAll_DF <- read_excel_allsheets(path, type = "One_dataframe", 
             drop_sheets = c("MEMO", "palourdes cassees","strates","EXEMPLE",
                             "STRATE LHn","STRATE LHs", "STRATE N", "STRATE M2",
                             "STRATE K2","STRATE C","STRATE D",
                              "STRATE I","STRATE J"))

readAll_DF$Sheet <- NULL

#creation de la variable input  qui regroupe toutes les strates qu'on a besoin 
# pour la creation de la deuxieme base

AddSheet = c("STRATE LHn","STRATE LHs", "STRATE N", "STRATE M2",
             "STRATE K2","STRATE C","STRATE D","STRATE I","STRATE J")
 
# application de la fonction pour obtenir une base qui formée
#par les strates qui n'ont pas de replicats
readAll_DF_1 <- read_excel_allsheets(path, type = "One_dataframe", 
                        drop_sheets = sheets[!sheets %in% AddSheet]) 

#renommée la variabe esp correspondant a R1 de la base readAll_DF_1 pour obtenir
# le meme nom de la base readAll_DF pour pouvoir faire le cbind
readAll_DF_1 %<>% rename(`esp (eur ou jau)...6` =`esp (eur ou jau)`)

readAll_DF_1$Sheet <-NULL

#creation de la variable id qui va partir de 1 jusqu'au nbre d'obs
readAll_DF <- readAll_DF %>%mutate(id =seq(1,dim(readAll_DF)[1]))

#creation de la variable id
readAll_DF_1 <- readAll_DF_1 %>%mutate(id = NA)

# rbind des 2 dataframe en enlevant les variables 6 et 7 
#qui sont dans readAll_DF et pas dans readAll_DF_1
dataframe <- rbind(readAll_DF[,-c(6,7)],readAll_DF_1)

#la jointure de la dataframe et les colonne de la readAll_DF c(6,7,8
dataframe %<>% left_join(readAll_DF[,c(6,7,8)]) %>% select(-id)
dataframe %<>% rename(`esp1 (eur ou jau)` ="esp (eur ou jau)...6",
                         `esp2 (eur ou jau)` = "esp (eur ou jau)...8" )

View(dataframe)

                              #Cleaning Data

# Pour supprimer les ligne vides
library(tidyverse)
test <-dataframe %>% filter(complete.cases(point,R1,R2) |
                               complete.cases(point,R1) | 
                               complete.cases(point,R2) |
                               complete.cases(R1,R2) |
                               complete.cases(point)|
                               complete.cases(R1) |
                               complete.cases(R2))

#pour corriger la date qui est sous formet 2018-05-14T00:00:00Z en 2018-05-14
test$date <- gsub(" UTC",paste0(""), test$date)

# pour mettre la date en format jour/mois/année
test$date <- as.Date(test$date)

# Création d'une fonction qui permet de faire l'imputation
imput <- function(data, column){
res = list()
for (row in as.character(data[[column]])){
  
  if(is.na(row) == FALSE){
    value = row
    res = append(res, value)
  }
  else if(is.na(row) == TRUE){
    row = value
    res = append(res, row)
  }
  res = as.character(res)
}

 return(res)
}
test$date <- as.Date(imput(test, "date"))
test$date <- format(test$date,"%d-%m-%Y")

test$point <- imput(test, "point")
View(test)

# Création des variables
test$Esp1 <- ifelse(
 (is.na(test$`esp1 (eur ou jau)`) )== TRUE,
  "jap",
  test$`esp1 (eur ou jau)`
)


  
#pour voir la distribution des modalites dans une variable
table(test$Esp1 )
# eur  jap  jau 
#39 6268    3 
AddSheet1 = c("LHn","LHs", "N", "M2",
             "K2","C","D","I","J")

test$Esp2 <- NA
test$Esp2[!test$strate %in%c(AddSheet1)][is.na(test$`esp2 (eur ou jau)`) ==TRUE] <- "jap"
test$Esp2[test$`esp2 (eur ou jau)` == "jau"] <- "jau"
test$Esp2[test$`esp2 (eur ou jau)` == "eur"] <- "eur"

# definition de la variable PtR1
  test$PtR1 <- paste0(test$strate,"-",test$point,"-","R1")
  test$PtR2 <- paste0(test$strate,"-",test$point,"-","R2")
  
  test$R1_corr <- round(test$R1,digits = 0)
  test$R1_corr[is.na( test$R1_corr) ==TRUE] <- 0
  
  test$R2_corr <- round(test$R2,digits = 0)
  test$R2_corr[is.na( test$R2_corr) ==TRUE] <- 0

# exportation du fiichier
library(data.table)
fwrite(test,'E:/MSID/projet tutore/Projet_Palourde/DF.csv')





