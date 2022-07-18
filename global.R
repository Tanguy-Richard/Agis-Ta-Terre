

############
# Import des package necessaires (a trier)
############

library(shiny) # Pour l'appli
library(tidyverse) # Import de tibble
library(dplyr) # Manipulation de tibble
library(ggplot2) # Graphique
library(shinythemes) # Pour le style "journal"
library(httr) # Pour l'import
library(jsonlite) # Pour l'import
library(lubridate) # Gestion du format de date
library(cowplot) # Superposition de graphique
library(CPAT) # test de Darling Erdos
library(synchrony) # fonction peaks (synchronicité des pics)
library(forecast) # fonction ma : moving average
library(zoo) # fonction na.trim
library(plotly) # Pour le graphique des heures d'engorgement
library(readr) # Pour l'export encsv compatible excel

########
# Parametrage des package
########

options(lubridate.week.start = 1) # Pour que la semaine commence jour 1


#######################################################################################

# Liste des capteurs (a completer avec les nouveaux capteurs)

#######################################################################################

# /!\ L'odre des listes suivantes est importantes, il fait le lien entre l'id des capteurs et leur nom /!\

listeSegments <- c("9000002156", "9000001906", "9000001618","9000003090","9000002453","9000001844",
                   "9000001877","9000002666","9000002181","9000002707","9000003703",
                   "9000003746","9000003775","9000003736")
listeNom <- c("Burel","Leclerc","ParisMarché","rueVignes","ParisArcEnCiel","RteVitré",
              "RueGdDomaine","StDidierNord","rueVallee","StDidierSud","RuePrieuré",
              "RueCottage","RueVeronnière","RueDesEcoles")
listeNombis <- c("Burel-01","Leclerc-02","ParisMarché-03","rueVignes-04","ParisArcEnCiel-05","RteVitré-06",
              "RueGdDomaine-07","StDidierNord-08","rueVallee-09","StDidierSud-10","RuePrieuré-11",
              "RueCottage-12","RueVeronnière-13","RueDesEcoles-14")


#######################################################################################

# Import des donnees

#######################################################################################


##########################################################
### clef pour l'API Telraam (à générer su telraam.net) ###
##########################################################


key = "UZiPH7KKBY1TS4wqAV8LHaXbN2FlBDlp7s5aXTcV"


#################################################
### Import dates jours feries et des vacances ###
#################################################

# A partir d'une API gouvernementale (il faut readapter cette partie s'il y a un souci/changement cote gouv)

### Jours feries ###

# (importe sur 25 ans : 285 jours)
# Récupération des jours au format: "YYYY-MM-DD"
Jours <- names(fromJSON(rawToChar(GET("https://calendrier.api.gouv.fr/jours-feries/metropole.json")$content)))
# Mise au format date de lubridate
JoursFeries <- ymd(Jours)

### Import dates vacances ###

#Import sur les données en lignes: a la création entre 2017 et 2023 (35 vacances)
#récupération des donnees specification de l'academie dans location (a changer si changement d'academie).

url = "https://data.education.gouv.fr/api/v2/catalog/datasets/fr-en-calendrier-scolaire/exports/json"
Vac <-  GET(url, query = list(refine = "location:Rennes",
                              exclude = "population:Enseignants"))
Vacances <- fromJSON(rawToChar(Vac$content))
# selection des colonnes: nom, debut et fin
Vacances <- Vacances[,c("description","start_date","end_date")]
# Passage en format date de lubridate, creation d'un interval pour la methode within
Vacances$start_date <- ymd_hms(Vacances$start_date)
Vacances$end_date <- ymd_hms(Vacances$end_date)
Vacances$interval <- interval(Vacances$start_date,Vacances$end_date)
# Remarque: les dates des vacance finissent vers 22h ou 23h -> cela ne changent rien au vu des heures
# de fonctionnement des capteurs Telraam (ne fonctionne que s'il fait jour)

#######################################################################################

# Definition de fonction globale

#######################################################################################


########################
# Découpe d'un interval de 2 date en bout de 3 mois (capacité d'import maximal de l'API de telraam)
########################

#' Séparation d'une période en bout de 3 mois pour l'import de Telraam
#'
#' @param date1 date de début au format "YYYY/MM/DD HH:MM:SS"
#' @param date2 date de fin au format "YYYY/MM/DD HH:MM:SS"
#'
#' @return un dataframe à 2 colones une recensant les dates de début des périodes
#' de 3 mois eu une recensant les dates de fins 
#' @export
#'
#' @examples
#' Dates("2021/01/01 00:00:00","2022/06/06 00:00:00")
#' 
Dates=function(date1,date2){
  #recuperation des dates au bon format
  dat1=ymd_hms(date1) 
  dat2=ymd_hms(date2)
  #date de stockage temporaire de debut et de fin
  date_temp = dat1
  date_suiv = date_temp+months(3)-seconds(1)
  #liste stockant les dates de debut et de fin
  date_debut = c(date_temp)
  date_fin = c(date_temp+months(3)-seconds(1))
  
  
  #on recommence tant que la derniere date de fin enregistrer et avant la derniere date souhaitee
  while(date_suiv < dat2){ 
    date_temp = date_temp + months(3) #on rajoute alors 3 mois
    date_suiv = date_temp + months(3) - seconds(1) #on rajoute alors 3 mois
    date_debut = c(date_debut,date_temp) #on enregistre dans les listes
    date_fin = c(date_fin,date_suiv) #on enregistre dans les listes
  }
  
  #retour sous la forme d'un data frame avec une colonne debut et  une fin
  return(data.frame(debut = date_debut , fin = date_fin)) 
}


########################
# Fonction de test d'appartenance d'une date a une liste d'intervals lubridate
########################


#' Test d'appartenance d'une date a une liste d'intervals lubridate
#'
#' @param date Date au format lubridate 
#' @param Liste_interval Liste d'intervals lubridate
#'
#' @return Un booléen indiquant si la date appartient ou pas à la liste d'interval
#' @export 
#'
#' @examples
#' date=ymd_hms("2021/01/01 00:00:00")
#' Liste_interval = c(interval("2020/11/02 00:00:00","2021/03/01 00:00:00"), interval("2022/02/01 00:00:00","2021/04/01 00:00:00"))
#' Test_date(date,Liste_interval)


Test_date=function(date,Liste_interval){
  return(sum(date %within% Liste_interval)>0)
}

########################
# Fonction de selection de date (dans une liste d'interval)
########################

#' Filtrage sur l'appartenance à une liste de périodes d'un dataframe
#'
#' @param Donnees un dataframe avec une colonne "date" (format lubridate) 
#' @param Liste_interval une liste d'intervals lubridate
#'
#' @return Le data frame des lignes correspondants aux intervals et un dataframe complementaire 
#' sous la forme d'une liste a 2 elements: data1 et data2
#' @export
#'
Selection_Date=function(Donnees,Liste_interval){
  Valeurs_Bool = unlist(lapply(Donnees$date, FUN = function(x){Test_date(x,Liste_interval)}))
  data1 = Donnees[Valeurs_Bool,]
  data2 = Donnees[!Valeurs_Bool,]
  return(list(data1 = data1 ,data2 =data2))
}


########################
# Fonction de selection de date 2 (dans une liste de dates sans heure)
########################


#' Filtrage sur l'appartenance à une liste de dates d'un dataframe
#'
#' @param Donnees un dataframe avec une colonne "date" (format lubridate)  
#' @param Liste_date une liste de dates lubridate
#'
#' @returnLe data frame des lignes correspondants aux dates et un dataframe complementaire 
#' sous la forme d'une liste a 2 elements: data1 et data2
#' @export
#'
Selection_Date2=function(Donnees,Liste_date){
  Valeurs_Bool = unlist(lapply(Donnees$date, FUN = function(x){
    date(x) %in% Liste_date}))
  data1 = Donnees[Valeurs_Bool,]
  data2 = Donnees[!Valeurs_Bool,]
  return(list(data1 = data1 ,data2 =data2))
}

#########################
# Séparation de la tendance, du cycle et du bruit
#########################

#' Séparation d'un signal en 3 part ( tendance, cycle hebdomadaire et bruit statistique)
#'
#' @param tableau Un dataframe avec une colonne date et une colonne cible
#' @param col Nom de la colonne cible
#' @param model un modèle (multiplicatif ou additif) pour la séparation
#'
#' @return une liste de 3 vecteurs: la tendance, le cycle hebdomadaire et le bruit statistique
#' @export
#'
desaisonalite=function(tableau,col,model){
  tendance <- as.vector(ma(tableau[,col],order = 14))
  if(model=="mult"){
    sanstendance <- tableau[,col] / tendance
  }
  if(model=="add"){
    sanstendance <- tableau[,col] - tendance
  }
  tab_temp <- as_tibble(cbind(tableau$date,sanstendance))
  colnames(tab_temp) <- c("date","ma")
  tab_temp_week <- tab_temp %>% group_by(wday(date)) %>% mutate(moyday=mean(ma,na.rm = TRUE))
  tab_temp_week <- tab_temp_week %>% filter (! duplicated(wday(date))) %>% arrange(wday(date))
  colnames(tab_temp_week) <- c("date","ma","Jsem","moyday")
  
  decycle <- NULL
  cycle <- NULL
  for(i in 1:length(tab_temp$date)){
    wd <- wday(tab_temp$date[i])
    val_wd <- tab_temp_week[tab_temp_week$Jsem==wd,]$moyday
    cycle <- c(cycle,val_wd)
    if(model=="mult"){
      val <- tab_temp$ma[i]/val_wd
    }
    if(model=="add"){
      val <- tab_temp$ma[i]-val_wd
    }
    decycle <- c(decycle,val)
  }
  
  return(list(tendance=tendance,cycle=cycle,bruit=decycle))
}









