

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
library(readr) # Pour l'export en csv compatible excel

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

# Récupération de la clef de l'API
key = read.delim("clef.txt",header = FALSE)[[1]]


#################################################
### Import dates jours feries et des vacances ###
#################################################

# A partir d'une API gouvernementale (il faut readapter cette partie s'il y a un souci/changement cote gouv)

### Jours feries ###

# (importe sur 25 ans : 285 jours)
# Récupération des jours au format: "YYYY-MM-DD"

JoursFeries <- GET("https://calendrier.api.gouv.fr/jours-feries/metropole.json") %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON() %>% 
  names() %>% 
  ymd()

### Import dates vacances ###

#Import sur les données en lignes: a la création entre 2017 et 2023 (35 vacances)
#récupération des donnees specification de l'academie dans location (a changer si changement d'academie).

url = "https://data.education.gouv.fr/api/v2/catalog/datasets/fr-en-calendrier-scolaire/exports/json"

Vacances <- GET(
  url = url,
  query = list(refine = "location:Rennes",
               exclude = "population:Enseignants")
) %>%
  .$content %>%
  rawToChar() %>%
  fromJSON() %>%
  select(description,
         start_date,
         end_date) %>%
  mutate(
    start_date = ymd_hms(start_date),
    end_date = ymd_hms(end_date),
    interval = interval(start_date, end_date)
  )
# Remarque: les dates des vacance finissent vers 22h ou 23h -> cela ne changent rien au vu des heures
# de fonctionnement des capteurs Telraam (ne fonctionne que s'il fait jour)

#######################################################################################

# Definition de fonction globale

#######################################################################################


########################
# Découpe d'un interval de 2 date en bout de 3 mois (capacité d'import maximal de l'API de telraam)
########################


#' Découper une période en "tranches" de 3 mois
#'
#' @param date1 Caractère. Date de début au format "aaaa-mm-jj hh:mm:ss"
#' @param date2 Caractère. Date de fin au format "aaaa-mm-jj hh:mm:ss"
#'
#' @return Dataframe avec une ligne par période de 3 mois et deux colonnes "debut" et "fin"
#' @export
#'
#' @examples
#' date1 <- "2021-03-01 12:35:21"
#' date2 <- "2022-07-01 03:15:33"
#'
#' test_df <- decouper_periode(date1, date2) %>% 
#'   mutate(intervalle = interval(debut, fin))
#' 
#' 
decouper_periode <- function(date1, date2){
  
  debut <- seq(from = ymd_hms(date1), 
               to = ymd_hms(date2),
               by = "3 month")
  
  fin <- debut + months(3)
  
  data.frame(debut,
             fin)
  
}

########################
# Fonction de test d'appartenance d'une date a une liste d'intervals lubridate
########################
#' Test d'appartenance d'une date a une liste d'intervals lubridate
#'
#' @param date Date au format lubridate 
#' @param liste_interval Liste d'intervals lubridate
#'
#' @return Un booléen indiquant si la date appartient ou pas à la liste d'interval
#' @export 
#'
#' @examples
#' date <- ymd_hms("2021/01/01 00:00:00")
#' interval1 <- interval("2020/11/02 00:00:00","2021/03/01 00:00:00")
#' interval2 <- interval("2022/02/01 00:00:00","2021/04/01 00:00:00")
#' Liste_interval = c(interval1, interval2)
#' test_date(date,Liste_interval)

test_date <- function(date, liste_interval){
  return(sum(date %within% liste_interval) > 0)
}

########################
# Fonction de selection de date (dans une liste d'interval)
########################
#' Filtrage sur l'appartenance à une liste de périodes d'un dataframe
#'
#' @param donnees un dataframe avec une colonne "date" (format lubridate) 
#' @param liste_interval une liste d'intervals lubridate
#'
#' @return Le data frame des lignes correspondants aux intervals et un dataframe complementaire 
#' sous la forme d'une liste a 2 elements: donnees_correspondantes et donnees_complementaires
#' @export
#'
selection_date <- function(donnees, liste_interval){
  dates_correspondantes = unlist(lapply(donnees$date, FUN = function(x){test_date(x,liste_interval)}))
  donnees_correspondantes = donnees[dates_correspondantes,]
  donnees_complementaires = donnees[!dates_correspondantes,]
  return(list(donnees_correspondantes = donnees_correspondantes,
              donnees_complementaires = donnees_complementaires))
}


########################
# Fonction de selection de date 2 (dans une liste de dates sans heure)
########################


#' Filtrage sur l'appartenance à une liste de dates d'un dataframe
#'
#' @param donnees un dataframe avec une colonne "date" (format lubridate)  
#' @param liste_date une liste de dates lubridate
#'
#' @return Le data frame des lignes correspondants aux dates et un dataframe complementaire 
#' sous la forme d'une liste a 2 elements: donnees_correspondantes et donnees_complementaires
#' @export
#'
selection_date2=function(donnees,liste_date){
  dates_correspondantes = unlist(lapply(donnees$date, FUN = function(x){
    date(x) %in% liste_date}))
  donnees_correspondantes = donnees[dates_correspondantes,]
  donnees_complementaires = donnees[!dates_correspondantes,]
  return(list(donnees_correspondantes = donnees_correspondantes ,
              donnees_complementaires = donnees_complementaires))
}

#########################
# Séparation de la tendance, du cycle et du bruit
#########################

#' Séparation d'un signal en 3 part ( tendance, cycle hebdomadaire et bruit statistique)
#'
#' @param donnees Un dataframe avec une colonne "date" (format lubridate) et une colonne cible
#' @param col Nom de la colonne cible
#' @param model un modèle (multiplicatif ou additif) pour la séparation
#'
#' @return une liste de 3 vecteurs: la tendance, le cycle hebdomadaire et le bruit statistique
#' @export
#'
desaisonalite=function(donnees,col,model){
  # Calcul de la tendance à l'aide d'une moyenne glissante
  tendance <- as.vector(ma(donnees[,col],order = 14))
  # Extraction de la tendance selon le choix de modèle
  if(model=="mult"){
    sanstendance <- donnees[,col] / tendance
  }
  if(model=="add"){
    sanstendance <- donnees[,col] - tendance
  }
  # Donnees sans tendance
  tab_temp <- as_tibble(cbind(donnees$date,sanstendance))
  colnames(tab_temp) <- c("date","ma")
  # Determination de l'impact du jour de la semaine
  jours_semaines <- tab_temp %>% group_by(wday(date)) %>% mutate(moyday=mean(ma,na.rm = TRUE))
  jours_semaines <- jours_semaines %>% filter (! duplicated(wday(date))) %>% arrange(wday(date))
  colnames(jours_semaines) <- c("date","ma","Jsem","moyday")
  # Séparation des données sans tendance en un cycle hebdomadaire et un signal restant (bruit) 
  decycle <- NULL
  cycle <- NULL
  for(i in 1:length(tab_temp$date)){
    # Récupération du jour de la semaine
    jour <- wday(tab_temp$date[i])
    # Récupération de la valeur du jour de la semaine
    val_jour <- jours_semaines[jours_semaines$Jsem==jour,]$moyday
    # Séparaison selon le modèle choisie
    cycle <- c(cycle,val_jour)
    if(model=="mult"){
      val_restante <- tab_temp$ma[i]/val_jour
    }
    if(model=="add"){
      val_restante <- tab_temp$ma[i]-val_jour
    }
    decycle <- c(decycle,val_restante)
  }
  
  return(list(tendance=tendance,cycle=cycle,bruit=decycle))
}









