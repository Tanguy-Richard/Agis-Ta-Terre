

shinyServer(function(session,input, output) {
  
##########################################################################################################################
#
# Définition des reactives
#
##########################################################################################################################

#######  

  # Test d'existence du capteur ajoute
  
#######   
  
  test_capt <- reactive({
    input$go_import #Pour conditionner la mise à jour
    isolate({
      
    if(input$captIDsup!=""){
      
      idseg = input$captIDsup
      status <- GET(paste0("https://telraam-api.net/v1/segments/id/", idseg),
                    add_headers("X-Api-Key" = key))$status_code
      if(status==200){
        FALSE
      }else{
        TRUE
      }
    }else{
      FALSE
    }
    })
  })
  

#######  
  
  # Creation de la liste de capteur selon le rajout ou non d'un capteur (pour choix de l'utilisateur)
  
#######
  
  liste_capteur <- reactive({
    # Gestion du potentiel capteur en plus
    if(input$captIDsup!=""){
      c(input$Capteurs, input$nomIDsup)
    }else{
      input$Capteurs
    }
  })
  
  
  
  
#######  
    
  # Import des donnees (dépendant des choix d'import)

#######   
  
  donnee_import <- reactive({
    
    
    input$go_import #Pour conditionner la mise à jour
    isolate({
      
    listres <- list(geomet = NULL, donnee = NULL)
    
    try({
      
    idCapteurs <- listeSegments[listeNom %in% input$Capteurs]
    
    # Gestion du potentiel capteur en plus
    if(input$captIDsup!=""){
      idCapteurs <- c(idCapteurs,input$captIDsup)
      listecapt <- c(listeSegments,input$captIDsup)
      listeNom_temp <- c(listeNom, input$nomIDsup)
    }else{
      listecapt <- listeSegments
      listeNom_temp <- listeNom
    }
    
    
    ### Fonction recup date (l'API ne permet de recupere les donnees que par laps de 3 mois)
    ### On segmente la période en bout de 3 mois pour importer par petits bouts
      
    
    
    # initialisation sous la forme de dataframes vides
    dfglob=data.frame() # pour les donnees totales
    dfgeo=data.frame() # pour les information geographiqe + donnees des dernieres 24h d'enregistrement
    
    
    withProgress(message = 'Import', value = 0,{
    incProgress(0, detail = paste("Capteurs", input$Capteurs[1]))
    k <- 2             
    # iteration sur l'ensemble des capteurs renseignes
    for(segs in idCapteurs){
      #recuperation des donnees sur le capteurs: localisation et date d'emission
      df <- fromJSON(rawToChar(GET(paste0("https://telraam-api.net/v1/segments/id/", segs),
                                   add_headers("X-Api-Key" = key))$content))$features
      # Recuperation des coordonnees (geometry) des routes isolees
      lat=df$geometry$coordinates[[1]][,,2]
      lon=df$geometry$coordinates[[1]][,,1]
      id <- cbind(lat, lon,df$properties)
      # Propriete correspondant au mesure sur les dernieres 24h
      df <- df$properties
      # Recuperation des dates d'emission de la premiere et derniere donnee
      dateInf = df$first_data_package
      dateSup = df$last_data_package  
      date1 <- input$daterange[1]
      date2 <- input$daterange[2]
      
      dateInf <- max(ymd_hms(dateInf),ymd_hms(paste(date1," 00:00:00")))
      dateSup <- min(ymd_hms(dateSup),ymd_hms(paste(date2," 23:00:00")))
      
      dfgeo = rbind(dfgeo,id)
      
      #recuperation des listes de dates separees par maximum 3 mois (pour iterer lors de l'import)
      dates = Dates(dateInf,dateSup)
      
      #recuperation des donnees mesurees par le capteur
      for(i in 1:length(dates$debut)){
        #recuperation des donnees
        resTraffic <- POST("https://telraam-api.net/v1/reports/traffic", add_headers("X-Api-Key" = key)
                           , body = paste0('{
    "level": "segments",
    "format": "per-hour",
    "id": "',segs,'",
    "time_start": "',dates$debut[i],'", 
    "time_end": "',dates$fin[i],'"                          
    }'))
        # On rend le fichier exploitable par R
        dataTraffic = fromJSON(rawToChar(resTraffic$content))
        df <- dataTraffic$report
        # On change la classe de date (caractère) en Date avec un decalade horaire de 2
        df$date <- ymd_hms(df$date, tz = df$timezone[1])
        dfglob=rbind(dfglob,df)
      }
      incProgress(1/length(idCapteurs), detail = paste("Capteurs", liste_capteur()[k]))
      k <- k+1
    }
    })
    
    #gestion du typage
    dfglob$segment_id <- as.character(dfglob$segment_id)
    dfgeo$lat <- as.numeric(as.character(dfgeo$lat))
    dfgeo$lon <- as.numeric(as.character(dfgeo$lon))
    
    
    #renomage des capteurs
    for(i in 1:length(listecapt)){
      dfglob$segment_id[dfglob$segment_id==listecapt[i]] <-listeNom_temp[i] 
    }
    
    #################
    # Selection des donnees non nuls
    #################
    
    donnees_nn <- dfglob[apply(dfglob[,6:17], MARGIN = 1, FUN = function(x){return(sum(x)!=0)}),]
    donnees_nn <- donnees_nn[donnees_nn$uptime>0.5,]
    
    listres <- list(geomet = dfgeo, donnee = donnees_nn)
    
    })
      
    })
    
    listres
    
  })
  
  
  
  
################################################################################################################# 
   
#######  
  
# Onglet test par période
  
#######    
  
  
  # Premiere sélection de tableau pour la comparaison de période 
  
  tableau_travail <- reactive({ 
    input$mise_a_j #Pour conditionner la mise à jour
    isolate({
    
    donnees_nn_plus <- donnee_import()$donnee #récupération des données d'import
    
    # Filtrage sur le capteur sélectionnée 
    Tableau <- donnees_nn_plus[donnees_nn_plus$segment_id==input$capteur,]
    
    
    # Filtrage sur le sens choisie 
    if(input$sens=="Toute"){
      Tableau <- Tableau[,c("date","uptime","heavy","car","bike","pedestrian")]
    }
    if(input$sens=="Rgt"){
      Tableau <- Tableau[,c("date","uptime","heavy_rgt","car_rgt","bike_rgt","pedestrian_rgt")]
      colnames(Tableau) <- c("date","uptime","heavy","car","bike","pedestrian")
    }
    if(input$sens=="Lft"){
      Tableau <- Tableau[,c("date","uptime","heavy_lft","car_lft","bike_lft","pedestrian_lft")]
      colnames(Tableau) <- c("date","uptime","heavy","car","bike","pedestrian")
    }
    
    # Filtrage sur la sélection de mobilités 
    if(length(input$mobilite)>1){
      interet <- apply(Tableau[,input$mobilite], MARGIN = 1 ,FUN = sum)
      Tableau$total <- interet
    }else{
      Tableau$total <- Tableau[,input$mobilite]
    }
    Tableau})
  })
  
  # Creation du tableau pour la periode de référence
  
  tableau_P1 <- reactive({ 
      # Récupération des données préfiltrées
      Tableau <- tableau_travail()
      
      # Test pour savoir si la sélection est vide
      if(length(Tableau$date)==0){
        Tableau="Selectionnez un capteur et au moins une mobilité, puis appuyez sur Mettre à jour"}else{
      
          
      # Sélection de la période temporelle
      date <- input$daterange1
      periode <- interval(ymd_hms(paste(date[1],"00:00:00")),ymd_hms(paste(date[2],"23:59:00")))
      Tableau <- Selection_Date(Tableau,periode)$data1
      
      # Sélection des jours de toute la semaine
      Tableau <- Tableau[wday(Tableau$date) %in% input$SM1 ,]
      
      if(length(Tableau$date)==0){ # Test pour savoir si la sélection est vide
        Tableau <- "Pas de données pour la selection de la période de référence"
      }else{ # Sélection de vacances
        if(input$Vacance1=="Non"){  
          Tableau <- Selection_Date(Tableau,Vacances$interval)$data2
        }
        if(input$Vacance1=="Seulement les vacances"){
          Tableau <- Selection_Date(Tableau,Vacances$interval)$data1
        }
        if(length(Tableau$date)==0){  # Test pour savoir si la sélection est vide
          Tableau <- "Pas de données pour la selection de la période de référence"
        }else{ # Sélection de jours fériés
          if(input$JF1=="Non"){
            Tableau <- Selection_Date2(Tableau,JoursFeries)$data2
          }
          if(input$JF1=="Seulement les jours fériés"){
            Tableau <- Selection_Date2(Tableau,JoursFeries)$data1
          }
          if(length(Tableau$date)==0){  # Test pour savoir si la sélection est vide
            Tableau <- "Pas de données pour la selection de la période de référence"
          }
        }
      }}
      Tableau
  })

  # Creation du tableau pour la première periode de comparaison
  
  tableau_P2 <- reactive({ 
    # Récupération des données préfiltrées
    Tableau <- tableau_travail()
    
    # Test pour savoir si la sélection est vide
    if(length(Tableau$date)==0){
      Tableau=""}else{
    
    # Sélection de la période temporelle
    date <- input$daterange2
    periode <- interval(ymd_hms(paste(date[1],"00:00:00")),ymd_hms(paste(date[2],"23:59:00")))
    Tableau <- Selection_Date(Tableau,periode)$data1
    
    # Sélection des jours de toute la semaine
    Tableau <- Tableau[wday(Tableau$date) %in% input$SM2 ,]
    
    if(length(Tableau$date)==0){# Test pour savoir si la sélection est vide
      Tableau <- "Pas de données pour la selection de la première période de comparaison"
    }else{ # Sélection de vacances
      if(input$Vacance2=="Non"){
        Tableau <- Selection_Date(Tableau,Vacances$interval)$data2
      }
      if(input$Vacance2=="Seulement les vacances"){
        Tableau <- Selection_Date(Tableau,Vacances$interval)$data1
      }
      if(length(Tableau$date)==0){ # Test pour savoir si la sélection est vide
        Tableau <- "Pas de données pour la selection de la première période de comparaison"
      }else{ # Sélection de jours fériés
        if(input$JF2=="Non"){
          Tableau <- Selection_Date2(Tableau,JoursFeries)$data2
        }
        if(input$JF2=="Seulement les jours fériés"){
          Tableau <- Selection_Date2(Tableau,JoursFeries)$data1
        }
        if(length(Tableau$date)==0){ # Test pour savoir si la sélection est vide
          Tableau <- "Pas de données pour la selection de la première période de comparaison"
        }
      }
    }}
    Tableau
  })
  
  # Creation du tableau pour la seconde periode de comparaison
  
  tableau_P3 <- reactive({ 
    # Récupération des données préfiltrées
    Tableau <- tableau_travail()
    
    # Test pour savoir si la sélection est vide
    if(length(Tableau$date)==0){
      Tableau=""}else{
        
        # Sélection de la période temporelle
        date <- input$daterange4
        periode <- interval(ymd_hms(paste(date[1],"00:00:00")),ymd_hms(paste(date[2],"23:59:00")))
        Tableau <- Selection_Date(Tableau,periode)$data1
        
        # Sélection des jours de toute la semaine
        Tableau <- Tableau[wday(Tableau$date) %in% input$SM3 ,]
        
        if(length(Tableau$date)==0){# Test pour savoir si la sélection est vide
          Tableau <- "Pas de données pour la selection de la seconde période de comparaison"
        }else{ # Sélection de vacances
          if(input$Vacance3=="Non"){
            Tableau <- Selection_Date(Tableau,Vacances$interval)$data2
          }
          if(input$Vacance3=="Seulement les vacances"){
            Tableau <- Selection_Date(Tableau,Vacances$interval)$data1
          }
          if(length(Tableau$date)==0){ # Test pour savoir si la sélection est vide
            Tableau <- "Pas de données pour la selection de la seconde période de comparaison"
          }else{ # Sélection de jours fériés
            if(input$JF3=="Non"){
              Tableau <- Selection_Date2(Tableau,JoursFeries)$data2
            }
            if(input$JF3=="Seulement les jours fériés"){
              Tableau <- Selection_Date2(Tableau,JoursFeries)$data1
            }
            if(length(Tableau$date)==0){ # Test pour savoir si la sélection est vide
              Tableau <- "Pas de données pour la selection de la seconde période de comparaison"
            }
          }
        }}
    Tableau
  })
  
  
  
  # Creation du titre du graphique de comparaison de période
  
  ord_txt <- reactive({ 
    input$mise_a_j #Pour conditionner la mise à jour
    isolate({ 
      # Récupération des termes en français
      fr <-  c("Véhicules Légers","Poids Lourds","Piétons","Vélos")
      eng <-  c("car","heavy","pedestrian","bike")
      mobfr <- fr[eng %in% input$mobilite]
      # Concaténation des mobilités séparées par "+"
      paste(mobfr,collapse = " + ")
    })
  })
  
  # Préparation des données pour l'export
  
  prep_tabl <- reactive({
    
    
    # Import du tableau de données pour la période de référence
    Tableau_1 <- tableau_P1()
    
    # Réalisation de moyenne par tranche horaire sur la période
    n_1 <- Tableau_1 %>% group_by(hour(date)) %>% summarise(n = n())
    Donnee_1 <- Tableau_1 %>% group_by(hour(date)) %>%
      mutate(Moyenne=mean(total))
    Donnee_1 <- Donnee_1 %>% filter (!duplicated(hour(date))) %>% arrange(hour(date))
    # On ne garde que l'heure et la moyenne (et on ajoute l'effectif)
    Donnee_1 <- bind_cols(Donnee_1[,8:9],n_1[,2])
    
    colnames(Donnee_1) <- c("Heure","Période_Ref","Effectif_Ref")
    
    # Import du tableau de données pour la première période de comparaison
    Tableau_2 <- tableau_P2()
    # Import du tableau de données pour la période 2
    n_2 <- Tableau_2 %>% group_by(hour(date)) %>% summarise(n = n())
    # Réalisation de moyenne par tranche horaire sur la période
    Donnee_2 <- Tableau_2 %>% group_by(hour(date)) %>%
      mutate(Moyenne=mean(total))
    Donnee_2 <- Donnee_2 %>% filter (!duplicated(hour(date))) %>% arrange(hour(date))
    # On ne garde que l'heure et la moyenne (et on ajoute l'effectif)
    Donnee_2 <- bind_cols(Donnee_2[,8:9],n_2[,2])
    
    colnames(Donnee_2) <- c("Heure","Période_1","Effectif_P1")
    
    # Import du tableau de données pour la seconde période de comparaison
    Tableau_3 <- tableau_P3()
    # Import du tableau de données pour la période 2
    n_3 <- Tableau_3 %>% group_by(hour(date)) %>% summarise(n = n())
    # Réalisation de moyenne par tranche horaire sur la période
    Donnee_3 <- Tableau_3 %>% group_by(hour(date)) %>%
      mutate(Moyenne=mean(total))
    Donnee_3 <- Donnee_3 %>% filter (!duplicated(hour(date))) %>% arrange(hour(date))
    # On ne garde que l'heure et la moyenne (et on ajoute l'effectif)
    Donnee_3 <- bind_cols(Donnee_3[,8:9],n_3[,2])
    
    colnames(Donnee_3) <- c("Heure","Période_2","Effectif_P2")
    
    # Concaténation sur l'heure de la moyenne
    Donnee <-  inner_join(Donnee_1,Donnee_2,by="Heure")
    Donnee <-  inner_join(Donnee,Donnee_3,by="Heure")
    
    Donnee
    
  })
  
  
################################################################################################################# 
  
#######  
  
# Onglet Seuil d'engorgement
  
#######    
  
  
  # Premiere sélection pour la détermination de seuil d'engorgement
  
  Tabl_Engor <- reactive({
    input$mise_a_j2 #Pour conditionner la mise à jour
    isolate({
      #récupération des données d'import
      Tableau <- donnee_import()$donnee
    
      # Sélection du capteur choisit par l'utilisateur
      Id = input$capteur3
      tableau_temp <- Tableau[Tableau$segment_id==Id,]
      
      # Séléction de la période
      date <- input$daterange3
      periode <- interval(ymd_hms(paste(date[1],"00:00:00")),ymd_hms(paste(date[2],"23:59:00")))
      Selection_Date(tableau_temp,periode)$data1
    })
  })
  
  
  plot_seuil_prep <- reactive({
    input$mise_a_j2 #Pour conditionner la mise à jour
    isolate({
      # Récupération du sens sélectionné
      orientation = input$sens3
      # Récupération du tableau pré filtré 
      tableau_temp <- Tabl_Engor()
      
      # Création des vecteurs pour stocker les différentes courbes de vitesse
      Vit_moins10=NULL
      Vit_moins20=NULL
      Vit_moins30=NULL
      Vit_moins40=NULL
      for(i in tableau_temp$car_speed_hist_0to70plus){ # On parcours les répartitions de vitesse
        vitesse <- unlist(i)
        # Somme progressive sur les parts d'usagers selon la vitesse
        vitesse10 <- vitesse[1]
        vitesse20 <- sum(vitesse[1:2])
        vitesse30 <- sum(vitesse[1:3])
        vitesse40 <- sum(vitesse[1:4])
        # Rajout des parts calculés aux vecteurs
        Vit_moins10 <- c(Vit_moins10,vitesse10)
        Vit_moins20 <- c(Vit_moins20,vitesse20)
        Vit_moins30 <- c(Vit_moins30,vitesse30)
        Vit_moins40 <- c(Vit_moins40,vitesse40)
      }
      tableau_temp$vit_moins10 <- Vit_moins10
      tableau_temp$vit_moins20 <- Vit_moins20
      tableau_temp$vit_moins30 <- Vit_moins30
      tableau_temp$vit_moins40 <- Vit_moins40
      
      # Création du tableau selon la direction choisie
      if(orientation=="Toute"){
        tableau_temp <- tableau_temp[,c("car","heavy","vit_moins10","vit_moins20","vit_moins30","vit_moins40")]
        tableau_temp <- tableau_temp %>% mutate(vehic = car + heavy)
        tableau_temp <- tableau_temp %>% arrange(vehic)
      }
      if(orientation=="Rgt"){
        tableau_temp <- tableau_temp[,c("car_rgt","heavy_rgt","vit_moins10","vit_moins20","vit_moins30","vit_moins40")]
        tableau_temp <- tableau_temp %>% mutate(vehic = car_rgt + heavy_rgt)
        tableau_temp <- tableau_temp %>% arrange(vehic)
      }
      if(orientation=="Lft"){
        tableau_temp <- tableau_temp[,c("car_lft","heavy_lft","vit_moins10","vit_moins20","vit_moins30","vit_moins40")]
        tableau_temp <- tableau_temp %>% mutate(vehic = car_lft + heavy_lft)
        tableau_temp <- tableau_temp %>% arrange(vehic)
      }
      # Calcul de moyenne glissante sur les parts
      vitesse10 <- embed(tableau_temp$vit_moins10,50)
      vitesse10 <- apply(vitesse10,1,mean)
      vitesse10 <- 100-vitesse10
      vitesse20 <- embed(tableau_temp$vit_moins20,50)
      vitesse20 <- apply(vitesse20,1,mean)
      vitesse20 <- 100-vitesse20
      vitesse30 <- embed(tableau_temp$vit_moins30,50)
      vitesse30 <- apply(vitesse30,1,mean)
      vitesse30 <- 100-vitesse30
      vitesse40 <- embed(tableau_temp$vit_moins40,50)
      vitesse40 <- apply(vitesse40,1,mean)
      vitesse40 <- 100-vitesse40
      # Réalisation des abcisses
      vehicule <- embed(tableau_temp$vehic,50)
      vehicule <- apply(vehicule,1,mean)
      VehG <- rep(vehicule,4)
      # Préparation du tableau pour le graphique
      Vitesse <- c(vitesse10,vitesse20,vitesse30,vitesse40)
      #Cr&ation du tableau pour l'import
      TablRes <- as.data.frame(cbind(vehicule,vitesse10,vitesse20,vitesse30,vitesse40))
      colnames(TablRes) <- c("Nombre de vehicules","plus de 40km/h","plus de 30km/h","plus de 20km/h","plus de 10km/h")
      k=length(vehicule)
      Legende <- c(rep("Plus de 10km/h",k),rep("Plus de 20km/h",k),rep("Plus de 30km/h",k),rep("Plus de 40km/h",k))
      donnee <- tibble(VehG,Vitesse,Legende)
      # Récupération des courbes lissées à partir de la méthode smooth de R
      p <- ggplot(donnee)+aes(x=VehG, y=Vitesse, color = Legende, group=Legende)+stat_smooth()
      y <- ggplot_build(p)$data[[1]][,1:3]
      # Création du tableau pour stocké les données
      Donnee <- NULL 
      for(i in levels(as.factor(y$colour))){
        Donnee <- bind_cols(Donnee,y[y$colour==i,-1])
      }
      X=Donnee[,1]
      Y=Donnee[,c(2,4,6,8)]
      Y <- t(t(Y)[order(t(Y)[,1]),]) # Rangement des colonnes par vitesse
      Donnee <- as.data.frame(bind_cols(X,Y))
      colnames(Donnee) <- c("Nombre de vehicules","plus de 40km/h","plus de 30km/h","plus de 20km/h","plus de 10km/h") #Renomage des colonnes
      # Calcul des seuils pour chaque courbes à partir d'un test de Darling Erdos
      res2 <- NULL
      if(input$vit=="Toute"){
        for(i in c(2:5)){
          tt <- DE.test(Donnee[,i]) # Darling-Erdos
          x <- Donnee[tt$estimate,1]
          res2 <- c(res2,x)
          moyenne <- mean(res2)
        }
      }else{
        D <- c("plus de 40km/h","plus de 30km/h","plus de 20km/h","plus de 10km/h")
        ind <- which(D==input$vit)
        tt <- DE.test(Donnee[,ind+1])
        x <- Donnee[tt$estimate,1]
        moyenne <- x
      }
      # Préparation de l'indexation de l'abscisse
      mmV <- max(donnee$VehG)
      if(mmV<100){
        absi <- seq(0,100,10)
      }else{
        if(mmV<500){
          absi <- seq(0,500,50)
          
        }else{
          maxi <- floor(mmV/100)*100
          absi <- seq(0,maxi,100)
          
        }
      }
      
      # ordonnée de l'afichage de la valeur du seuil
      miny <- min(vitesse40)
      ordMoy <- (100+miny)/2
      
      # Graphique du seuil
      graph <- ggplot(donnee)+aes(x=VehG, y=Vitesse, color = Legende, group=Legende)+geom_line(color="black")+
        geom_smooth()+labs(x="Nombre de véhicules sur une tranche horaire", y = "Pourcentage de véhicule dépassant la vitesse données")+
        ggtitle("Evolution de la vitesse de conduite selon le nombre d'usagers")+
        geom_vline(xintercept=moyenne,color="red", size = 1.5)+
        scale_x_continuous(breaks=c(absi), labels=c(absi))+
        geom_text(aes(x=moyenne, y=ordMoy,label=round(moyenne)),size=5,angle=-90, vjust=-0.5,color="red")
    
      # Donnees tracées
      Donnees <- list(Courbes_brute=TablRes,Courbes_lissees=Donnee)
      
      # Retour
      list(graph=graph,Donnees=Donnees)
    })
  })
    
    
    
  
################################################################################################################# 
  
#######  
  
# Onglet comparaison de deux capteurs
  
#######    
  
  
  # Premiere sélection pour la comparaison de capteurs
  
  Compar_tabl <- reactive({ 
    
    tableau <- donnee_import()$donnee #récupération des données d'imports
    
    # Filtrage sur les segments et l'heure sélectionnées
    segments=c(input$capteur1,input$capteur2)
    heure = input$heure
    Seg1 <- tableau[tableau$segment_id==segments[1] & hour(tableau$date)==heure,]
    Seg2 <- tableau[tableau$segment_id==segments[2] & hour(tableau$date)==heure,]
    # Jointure sur les dates communes
    tabjoin <- inner_join(Seg1,Seg2,by="date",suffix=c("1","2"))
    # Sélectioon du suffixe par rapport aux directions choisies
    if(input$sens1=="Toute"){
      S1 <- ""
    }
    if(input$sens1=="Rgt"){
      S1 <- "_rgt"
    }
    if(input$sens1=="Lft"){
      S1 <- "_lft"
    }
    
    if(input$sens2=="Toute"){
      S2 <- ""
    }
    if(input$sens2=="Rgt"){
      S2 <- "_rgt"
    }
    if(input$sens2=="Lft"){
      S2 <- "_lft"
    }
    # Filtrage selon les mobilités et la direction (totA : capteur 1 et totB : capteur 2)
    if(length(input$mobilite2)>1){
      totA <- apply(tabjoin[,paste(input$mobilite2,S1,1,sep = "")], MARGIN = 1 ,FUN = sum)
      totB <- apply(tabjoin[,paste(input$mobilite2,S2,2,sep = "")], MARGIN = 1 ,FUN = sum)
    }else{
      totA <- tabjoin[,paste(input$mobilite2,S1,1,sep = "")]
      totB <- tabjoin[,paste(input$mobilite2,S2,2,sep = "")]
    } # Vérification sur le nombre de données sélectionnée (il y en a-t-il assez pour des analyses pertinente)
    if(length(tabjoin$date)<28){
      "Période commune des deux capteurs trop courte"
    }else{ # Préparation des données pour la suite : tableau et non de colonnes
      res <- bind_cols(tabjoin$date,totA,totB)
      colnames(res) <- c("date",paste(segments,c(input$sens1,input$sens2),sep="_"))
      res
    }
  })
  
  # Séparation de la tendance, de la partie cyclique et du bruit pour la comparaison de capteurs 
  
  Compar_tabl2 <- reactive({ 
    # Récupération des données du reactive précédent
    Tableau <- Compar_tabl()
    cap1 <- paste(input$capteur1,input$sens1,sep="_") # Recreation du nom de la colonne du capteur 1
    cap2 <- paste(input$capteur2,input$sens2,sep="_") # Recreation du nom de la colonne du capteur 2
    trait1 <- desaisonalite(Tableau,cap1,"add") # Séparation du signal en tendance, cycle et bruit (période 1)
    trait2 <- desaisonalite(Tableau,cap2,"add") # Idem Période 2
    
    list(C1=trait1,C2=trait2) # Retour sous la forme d'une liste à 2 éléments
  
  })
  
  
  #Preparation du tableau pour l'import
  
  prep_tabl2 <- reactive({
    input$mise_a_j3 #Pour conditionner la mise à jour
    isolate({
    Date <- Compar_tabl()$date # Recuperation du vecteur de date
    # recupereration du nom des capteurs
    cap1 <- paste(input$capteur1,input$sens1,sep="_")
    cap2 <- paste(input$capteur2,input$sens2,sep="_")
    # Recuperation des donnees desaisonnalisees
    cp1 <- Compar_tabl2()$C1
    cp2 <- Compar_tabl2()$C2
    # Creation du tableau
    tableau <- bind_cols(Date,cp1$tendance,cp1$cycle,cp1$bruit,cp2$tendance,cp2$cycle,cp2$bruit)
    colnames(tableau) <- c("Date",paste(cap1,"tendance"),paste(cap1,"cycle"),paste(cap1,"bruit"),
                           paste(cap2,"tendance"),paste(cap2,"cycle"),paste(cap2,"bruit"))
    tableau
    })
  })
  
  
  
  
  
  
  
################################################################################################################# 
  
#######  
  
# Onglet Heure d'engorgement
  
#######
  
  
  # Définition du Graphique (character si vide)
  
  plot_eng_react <- reactive({
    
    #Séléction du capteur sélectionnée
    don_1=subset(donnee_import()$donnee, segment_id==input$capteur4)
    
    #Séléction de la période
    date <- input$daterange5
    
    periode <- interval(ymd(date[1]),ymd(date[2]))
    don_1=Selection_Date(don_1,periode)$data1
    
    if(length(don_1$date)==0){  # Test pour savoir si la sélection est vide
      "Pas de données pour la selection de la période"
    }else{
      if(input$Vacance4=="Non"){
        don_1 <- Selection_Date(don_1,Vacances$interval)$data2
      }
      if(input$Vacance4=="Seulement les vacances"){
        don_1 <- Selection_Date(don_1,Vacances$interval)$data1
      }
      if(length(don_1$date)==0){  # Test pour savoir si la sélection est vide
        don_1 <- "Pas de données pour la selection de la période"
      }else{ # Sélection de jours fériés
        if(input$JF4=="Non"){
          don_1 <- Selection_Date2(don_1,JoursFeries)$data2
        }
        if(input$JF4=="Seulement les jours fériés"){
          don_1 <- Selection_Date2(don_1,JoursFeries)$data1
        }
        if(length(don_1$date)==0){  # Test pour savoir si la sélection est vide
          "Pas de données pour la selection de la période"
        }else{
    # Séléction des jours de la semaine
    jours <- input$SM4
    # Calcul des moyenne par créneau horaire
    v=don_1%>%filter(wday(date) %in% jours)%>% group_by(hour(date)) %>% summarise(n=mean(v85,na.rm=TRUE))
    colnames(v)=c("Heure", "Vitesse")
    bu_rgt=don_1%>%filter(wday(date) %in% jours)%>% group_by(hour(date)) %>% summarise(n_rgt=mean(car_rgt+heavy_rgt,na.rm=TRUE))
    colnames(bu_rgt)=c("Heure", "Voiture_rgt")
    bu_lft=don_1%>%filter(wday(date) %in% jours)%>% group_by(hour(date)) %>% summarise(n_lft=mean(car_lft+heavy_lft,na.rm=TRUE))
    colnames(bu_lft)=c("Heure", "Voiture_lft")
    # Tableau final pour le graphique
    bu = full_join(bu_lft, bu_rgt, by = "Heure")
    bu = full_join(bu, v, by = "Heure")
    
    if(length(bu$Heure)==0){ # Test pour savoir si la sélection est vide
      "Pas de données pour la selection de la période"
    }else{
      # Création du graphique
      # Placement des courbes des véhicules
      fig <- plot_ly(bu, x = ~Heure)
      fig <- fig %>% add_trace(y= ~Voiture_rgt, mode = "lines+markers", name = "B vers A", 
                               line=list(color="blue", dash = "dot"),
                               marker=list(color="blue"))
      fig <- fig %>% add_trace(y= ~Voiture_lft, mode = "lines+markers", name = "A vers B", 
                               line=list(color="blue", dash = "dash"),
                               marker=list(color="blue"))
      # Création du second axe des ordonnées
      ay <- list(
        tickfont = list(color = "red"),
        overlaying = "y",
        side = "right",
        title = "Vitesse v85 moyenne (km/h)")
      # Placement de la courbe de vitesse
      fig <- fig %>% add_trace(y= ~Vitesse,  yaxis = "y2", mode = "lines+markers", name = "Vitesse v85 moyenne", 
                               line=list(color="red"),
                               marker=list(color="red"))
      # Mise en place du titre et des axes
      fig <- fig %>% layout(
        title = "", yaxis2 = ay,
        xaxis = list(title="Heure"),
        yaxis = list(title="Nombre de véhicules moyen")
      )%>%
        layout(plot_bgcolor='#e5ecf6',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 dtick=1),
               yaxis = list(
                 tickfont= list(color="blue"),
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff')
        )
      
      fig
      
    }    
    
    }
    
      }
    }
  })   
  
  
##########################################################################################################################
#
# Définition des outputs
#
##########################################################################################################################  
  
  
  
################################################################################################################# 
  
#######  
  
# Onglet Test par période
  
#######  
  
  # Graphique par heure et test de wilcoxon horaire (reference et première période de comparaison)
  
  output$plot <- renderPlot({ 
    
    # Récupération du tapleau de la période de référence
    Tableau_1 <- tableau_P1()
    # Moyenne par tranche horaire
    n_1 <- Tableau_1 %>% group_by(hour(date)) %>% summarise(n = n())
    Donnee_1 <- Tableau_1 %>% group_by(hour(date)) %>%
      mutate(Moyenne=mean(total), Var=var(total))
    Donnee_1 <- Donnee_1 %>% filter (!duplicated(hour(date))) %>% arrange(hour(date))
    # Sélection de la colonne heure et celle colonne horaire
    Donnee_1 <- bind_cols(Donnee_1[,8:10],n_1[,2])
    # Rajout d'une colonne répétant "Periode Ref"
    Donnee_1 <- cbind(Donnee_1,rep("Periode_Ref",length(Donnee_1[,1])))
    
    # Récupération du tapleau de la première période de comparaison
    Tableau_2 <- tableau_P2()
    # Moyenne par tranche horaire
    n_2  <- Tableau_2 %>% group_by(hour(date)) %>% summarise(n = n())
    Donnee_2 <- Tableau_2 %>% group_by(hour(date)) %>%
      mutate(Moyenne=mean(total), Var=var(total))
    Donnee_2 <- Donnee_2 %>% filter (!duplicated(hour(date))) %>% arrange(hour(date))
    # Sélection de la colonne heure et celle colonne horaire
    Donnee_2 <- bind_cols(Donnee_2[,8:10],n_2[,2])
    # Rajout d'une colonne répétant "Periode 1"
    Donnee_2 <- cbind(Donnee_2,rep("Periode_1",length(Donnee_2[,1])))
    
    # Concaténation à la suite des tableaux
    Donnee <- rbind(Donnee_1,Donnee_2)
    # Renomage des colonnes
    colnames(Donnee) <- c("Heure","Nombre_usagers","Variance","Effectif","Periode")
    
    # Selection des colonnes sur lesquelles on a une variance (2 valeurs au moins)
    Donnee <- Donnee[Donnee$Effectif>1,]
    
    # Calcul des bornes (loi de student)
    Donnee <- Donnee%>% mutate(q=qt(.975,df=Effectif-1))
    Donnee <- Donnee %>% mutate(Born1=Nombre_usagers-q*sqrt(Variance/Effectif),
                                Born2=Nombre_usagers+q*sqrt(Variance/Effectif))
    # Sélection des heures communes
    heure1 <- as.numeric(levels(as.factor(hour(Tableau_1$date))))
    heure2 <- as.numeric(levels(as.factor(hour(Tableau_2$date))))
    heure <- intersect(heure1,heure2)
    # Nombre d'heure
    k=length(heure)
    
    # Création du graphique faisant apparaitre les courbes des deux périodes
    l <- ggplot(Donnee)+aes(x = Heure, y=Nombre_usagers, group = Periode, color = Periode)+geom_line(aes(linetype=Periode),size=1.5)+
      labs(x="Heure", y = "Nombre moyen d'usagers")+
      geom_ribbon(aes(ymin=Born1, ymax=Born2, fill = Periode), linetype = "blank",alpha = 1/4)+
      ggtitle(ord_txt())+scale_x_continuous(breaks=heure,limits = c(heure[1]-0.5,heure[k]+0.5))+
      expand_limits(y = 0)
    
    # Création du vecteur qui va servir à stocker les valeurs des test de Wilcoxon
    Stat_wilcox <- NULL
    Couleur <- NULL
    # Boucle sur chaque heure
    for(i in heure){
      # Test de wilcoxon pour une heure donnée
      stata <- wilcox.test(Tableau_1[hour(Tableau_1$date)==i,]$total,
                           Tableau_2[hour(Tableau_2$date)==i,]$total)$p.value
      Stat_wilcox <- c(Stat_wilcox, stata)
      # Choix de la couleur en fonction de la p-valeur du test
      if(stata<0.05){
        Couleur <- c(Couleur,"Significatif")
      }
      if(stata>=0.1){
        Couleur <- c(Couleur,"Non-significatif")
      }
      if(stata>=0.05 & stata<0.1){
        Couleur <- c(Couleur,"Entre deux")
      }
      
    }
    
    # Création du tableau pour la barre indiquant la significativité des tests
    Don2 <- as_tibble(cbind(heure,Couleur))
    colnames(Don2) <- c("heure","couleur")
    
    # Graphique: histogramme d'une unité de hauteur, indiquant la valeur de la significativité
    h <- ggplot(Don2)+aes(x = as.double(heure) , color = couleur, fill = couleur) +
      geom_histogram(bins = k+1)+scale_x_continuous(breaks=as.double(heure),limits = c(as.double(heure[1])-0.5,as.double(heure[k])+0.5))+
      scale_color_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
      scale_fill_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
      theme(
        title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
      labs(title="Significativité d'une différence de comportement (*)",
           x="Heure",
           y="")
    
    # Alignement des deux graphiques 
    cowplot::plot_grid(l, h, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
    
  })
  
  
  # Graphique par heure et test de wilcoxon horaire (reference et seconde période de comparaison)
  
  output$plot2 <- renderPlot({ 
    
    # Récupération du tapleau de la période de référence
    Tableau_1 <- tableau_P1()
    # Moyenne par tranche horaire
    n_1 <- Tableau_1 %>% group_by(hour(date)) %>% summarise(n = n())
    Donnee_1 <- Tableau_1 %>% group_by(hour(date)) %>%
      mutate(Moyenne=mean(total), Var=var(total))
    Donnee_1 <- Donnee_1 %>% filter (!duplicated(hour(date))) %>% arrange(hour(date))
    # Sélection de la colonne heure et celle colonne horaire
    Donnee_1 <- bind_cols(Donnee_1[,8:10],n_1[,2])
    # Rajout d'une colonne répétant "Periode Ref"
    Donnee_1 <- cbind(Donnee_1,rep("Periode_Ref",length(Donnee_1[,1])))
    
    # Récupération du tapleau de la seconde période de comparaison
    Tableau_2 <- tableau_P3()
    # Moyenne par tranche horaire
    n_2  <- Tableau_2 %>% group_by(hour(date)) %>% summarise(n = n())
    Donnee_2 <- Tableau_2 %>% group_by(hour(date)) %>%
      mutate(Moyenne=mean(total), Var=var(total))
    Donnee_2 <- Donnee_2 %>% filter (!duplicated(hour(date))) %>% arrange(hour(date))
    # Sélection de la colonne heure et celle colonne horaire
    Donnee_2 <- bind_cols(Donnee_2[,8:10],n_2[,2])
    # Rajout d'une colonne répétant "Periode 2"
    Donnee_2 <- cbind(Donnee_2,rep("Periode_2",length(Donnee_2[,1])))
    
    # Concaténation à la suite des tableaux
    Donnee <- rbind(Donnee_1,Donnee_2)
    # Renomage des colonnes
    colnames(Donnee) <- c("Heure","Nombre_usagers","Variance","Effectif","Periode")
    
    # Selection des colonnes sur lesquelles on a une variance (2 valeurs au moins)
    Donnee <- Donnee[Donnee$Effectif>1,]
    
    # Calcul des bornes (loi de student)
    Donnee <- Donnee%>% mutate(q=qt(.975,df=Effectif-1))
    Donnee <- Donnee %>% mutate(Born1=Nombre_usagers-q*sqrt(Variance/Effectif),
                                Born2=Nombre_usagers+q*sqrt(Variance/Effectif))
    # Sélection des heures communes
    heure1 <- as.numeric(levels(as.factor(hour(Tableau_1$date))))
    heure2 <- as.numeric(levels(as.factor(hour(Tableau_2$date))))
    heure <- intersect(heure1,heure2)
    # Nombre d'heure
    k=length(heure)
    
    # Création du graphique faisant apparaitre les courbes des deux périodes
    l <- ggplot(Donnee)+aes(x = Heure, y=Nombre_usagers, group = Periode, color = Periode)+geom_line(aes(linetype=Periode),size=1.5)+
      labs(x="Heure", y = "Nombre moyen d'usagers")+
      geom_ribbon(aes(ymin=Born1, ymax=Born2, fill = Periode), linetype = "blank",alpha = 1/4)+
      ggtitle(ord_txt())+scale_x_continuous(breaks=heure,limits = c(heure[1]-0.5,heure[k]+0.5))+
      expand_limits(y = 0)
    
    # Création du vecteur qui va servir à stocker les valeurs des test de Wilcoxon
    Stat_wilcox <- NULL
    Couleur <- NULL
    # Boucle sur chaque heure
    for(i in heure){
      # Test de wilcoxon pour une heure donnée
      stata <- wilcox.test(Tableau_1[hour(Tableau_1$date)==i,]$total,
                           Tableau_2[hour(Tableau_2$date)==i,]$total)$p.value
      Stat_wilcox <- c(Stat_wilcox, stata)
      # Choix de la couleur en fonction de la p-valeur du test
      if(stata<0.05){
        Couleur <- c(Couleur,"Significatif")
      }
      if(stata>=0.1){
        Couleur <- c(Couleur,"Non-significatif")
      }
      if(stata>=0.05 & stata<0.1){
        Couleur <- c(Couleur,"Entre deux")
      }
      
    }
    
    # Création du tableau pour la barre indiquant la significativité des tests
    Don2 <- as_tibble(cbind(heure,Couleur))
    colnames(Don2) <- c("heure","couleur")
    
    # Graphique: histogramme d'une unité de hauteur, indiquant la valeur de la significativité
    h <- ggplot(Don2)+aes(x = as.double(heure) , color = couleur, fill = couleur) +
      geom_histogram(bins = k+1)+scale_x_continuous(breaks=as.double(heure),limits = c(as.double(heure[1])-0.5,as.double(heure[k])+0.5))+
      scale_color_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
      scale_fill_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
      theme(
        title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
      labs(title="Significativité d'une différence de comportement (*)",
           x="Heure",
           y="")
    
    # Alignement des deux graphiques 
    cowplot::plot_grid(l, h, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))
    
  })
  
  # Affichage du nombre moyen de jours par périodes
  
  output$NbLignes <- renderUI({
    Valeur <- prep_tabl()
    nombreRef <- round(mean(Valeur$Effectif_Ref),3)
    nombreP1 <- round(mean(Valeur$Effectif_P1),3)
    nombreP2 <- round(mean(Valeur$Effectif_P2),3)
    ligneRef <- paste("Nombre moyen de valeur par heure pour la période de reférence :",nombreRef)
    ligne1 <- paste("Nombre moyen de valeur par heure pour la période 1 :",nombreP1)
    ligne2 <- paste("Nombre moyen de valeur par heure pour la période 2 :",nombreP2)
    
    HTML(paste(ligneRef,ligne1,ligne2,sep="<br/>"))
  })
  
  
  # Bouton d'import des données
  
  output$downloadData <- downloadHandler(
    filename = "Comparaison_periode.csv", # Nom du fichier importé
    content = function(file) {
      write_excel_csv2(prep_tabl(), file)
    }
  )
  
  
  
  
  
################################################################################################################# 
  
#######  
  
# Onglet Seuil d'engorgement
  
#######

  
  # Graphique du seuil d'engorgement
  
  output$plot_seuil <- renderPlot({
      plot_seuil_prep()$graph
  })
  
  output$downloadbrut <- downloadHandler(
    filename = "Courbes_brutes.csv", # Nom du fichier importé
    content = function(file) {
      write_excel_csv2(plot_seuil_prep()$Donnees$Courbes_brute, file)
    }
  )
  
  
  output$downloadlisse <- downloadHandler(
    filename = "Courbes_lissées.csv", # Nom du fichier importé
    content = function(file) {
      write_excel_csv2(plot_seuil_prep()$Donnees$Courbes_lissees, file)
    }
  )
  
  
################################################################################################################# 
  
#######  
  
# Onglet Comparaison de deux capteurs
  
#######
  
  # Graphique de la tendance
  
  output$plot_tend <- renderPlot({
    input$mise_a_j3 #Pour conditionner la mise à jour
    isolate({
      # Récupération des dates communes pour les abscisses
      Date <- Compar_tabl()$date
      # Récupération des données traitées
      Tableau <- Compar_tabl2()
      # Reconstruction du nom des capteurs et du sens
      cap1 <- paste(input$capteur1,input$sens1,sep="_")
      cap2 <- paste(input$capteur2,input$sens2,sep="_")
      if(input$Norm1=="Oui"){ # Construction du graphique pour le choix normé
        tend_1 <- scale(Tableau$C1$tendance) # Normalisation de la tendance pour le capteur 1
        tend_2 <- scale(Tableau$C2$tendance) # Normalisation de la tendance pour le capteur 2
        X <- c(Date,Date)
        Y <- c(tend_1,tend_2)
        Capteur <- c(rep(cap1,length(tend_1)),rep(cap2,length(tend_1)))
        
        # Graphique de tendance
        g <- ggplot()+geom_line(aes(x=X,y=Y,col=Capteur))+labs(title = "Tendance",
                                                               x = "Date",
                                                               y="")+
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
      }
      if(input$Norm1=="Non"){ # Construction du graphique pour le choix non normé
        tend_1 <- Tableau$C1$tendance # Récupération de la tendance de la courbe 1
        tend_2 <- Tableau$C2$tendance # Récupération de la tendance de la courbe 2
        X <- c(Date,Date)
        Y <- c(tend_1,tend_2)
        Capteur <- c(rep(cap1,length(tend_1)),rep(cap2,length(tend_1)))
        
        # Graphique non normé
        g <- ggplot()+geom_line(aes(x=X,y=Y,col=Capteur))+labs(title = "Tendance",
                                                               x = "Date",
                                                               y="Variation en nombre de véhicules")
      }
      
      g
    })
  })
  
  # Graphique du cycle
  
  output$plot_cycle <- renderPlot({
    input$mise_a_j3 #Pour conditionner la mise à jour
    isolate({
      # Récupération des dates communes pour les abscisses
      Date <- Compar_tabl()$date
      # Récupération des données traitées
      Tableau <- Compar_tabl2()
      # Reconstruction du nom des capteurs et du sens
      cap1 <- paste(input$capteur1,input$sens1,sep="_")
      cap2 <- paste(input$capteur2,input$sens2,sep="_")
      # Vecteur des jours de la semaine
      jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
      # Récupération des valeurs par jour:
      week1 <- NULL
      week2 <- NULL
      for(i in 1:7){
        week1 <- c(week1,Tableau$C1$cycle[wday(Date)==i][1])
        week2 <- c(week2,Tableau$C2$cycle[wday(Date)==i][1])
      }
      
      if(input$Norm1=="Oui"){# Construction du graphique pour le choix normé
        cycle_1 <- scale(week1) # Normalisation du cycle pour le capteur 1
        cycle_2 <- scale(week2) # Normalisation du cycle pour le capteur 2
        X <- c(1:7,1:7)
        Y <- c(cycle_1,cycle_2)
        Capteur <- c(rep(cap1,length(cycle_1)),rep(cap2,length(cycle_1)))
        
        g <- ggplot()+geom_line(aes(x=X,y=Y,col=Capteur))+labs(title = "Cycle",
                                                               x = "Date",
                                                               y="")+
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())+
          scale_x_continuous(breaks = 1:7, label=jours)
      }
      if(input$Norm1=="Non"){ # Construction du graphique pour le choix non normé
        cycle_1 <- week1 # Récupération du cycle de la courbe 1
        cycle_2 <- week2 # Récupération du cycle de la courbe 2
        X <- c(1:7,1:7)
        Y <- c(cycle_1,cycle_2)
        Capteur <- c(rep(cap1,length(cycle_1)),rep(cap2,length(cycle_1)))
        
        g <- ggplot()+geom_line(aes(x=X,y=Y,col=Capteur))+labs(title = "Cycle",
                                                               x = "Date",
                                                               y="Variation en nombre de véhicules")+
          scale_x_continuous(breaks = 1:7,  label = jours)
      }
      g
    })
  })
  
  # Graphique du bruit
  
  output$plot_bruit <- renderPlot({
    input$mise_a_j3 #Pour conditionner la mise à jour
    isolate({
      # Récupération des dates communes pour les abscisses
      Date <- Compar_tabl()$date
      # Récupération des données traitées
      Tableau <- Compar_tabl2()
      # Reconstruction du nom des capteurs et du sens
      cap1 <- paste(input$capteur1,input$sens1,sep="_")
      cap2 <- paste(input$capteur2,input$sens2,sep="_")
      if(input$Norm1=="Oui"){# Construction du graphique pour le choix normé
        bruit_1 <- scale(Tableau$C1$bruit) # Normalisation du bruit pour le capteur 1
        bruit_2 <- scale(Tableau$C2$bruit) # Normalisation du bruit pour le capteur 2
        X <- c(Date,Date)
        Y <- c(bruit_1,bruit_2)
        Capteur <- c(rep(cap1,length(bruit_1)),rep(cap2,length(bruit_2)))
        Don <- bind_cols(X,Y,Capteur)
        g <- ggplot(Don)+geom_line(aes(x=X,y=Y,col=Capteur))+labs(title = "Bruit",
                                                                  x = "Date",
                                                                  y ="") +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
      }
      if(input$Norm1=="Non"){ # Construction du graphique pour le choix non normé
        bruit_1 <- Tableau$C1$bruit # Récupération du bruit de la courbe 1
        bruit_2 <- Tableau$C2$bruit # Récupération du bruit de la courbe 2
        X <- c(Date,Date)
        Y <- c(bruit_1,bruit_2)
        Capteur <- c(rep(cap1,length(bruit_1)),rep(cap2,length(bruit_2)))
        Don <- bind_cols(X,Y,Capteur)
        g <- ggplot(Don)+geom_line(aes(x=X,y=Y,col=Capteur))+labs(title = "Bruit",
                                                                  x = "Date",
                                                                  y ="Variation en nombre de véhicules")
      }
      g
    })
  })
  
  # Coefficient de corrélation de Pearson pour le bruit
  
  output$correlation <- renderUI({
    input$mise_a_j3 #Pour conditionner la mise à jour
    isolate({
      # Récupération des données traitées
      Tableau <- Compar_tabl2()
      bruit_1 <- scale(Tableau$C1$bruit)# Normalisation du bruit pour le capteur 1
      bruit_2 <- scale(Tableau$C2$bruit)# Normalisation du bruit pour le capteur 2
      correl <- round(cor(bruit_1,bruit_2,use = "na.or.complete"),3) # Coefficient de correlation de Pearson 
      
      ligne1 <- paste("Coefficient de correlation :",correl) # Fabrication du texte pour l'affichage
      if(correl>0.5){
        ligne2 <- "C'est une valeur élevée, les deux courbes sont corrélées "
      }
      if(correl<=0.5){
        ligne2 <- "C'est une valeur moyenne, les deux courbes sont légèrement corrélées "
      }
      if(correl<0.2){
        ligne2 <- "C'est une valeur faible, les deux courbes ne sont pas corrélées "
      }
      HTML(paste(ligne1,ligne2,sep="<br/>"))
    })
  })
  
  
  # Coefficient de synchronicité des pics pour le bruit
  
  output$pics <- renderUI({
    input$mise_a_j3 #Pour conditionner la mise à jour
    isolate({
      # Récupération des données traitées
      Tableau <- Compar_tabl2()
      bruit_1 <- na.trim(scale(Tableau$C1$bruit))# Normalisation du bruit pour le capteur 1
      bruit_2 <- na.trim(scale(Tableau$C2$bruit))# Normalisation du bruit pour le capteur 2
      # Test de synchronicité des pics (on récupère la pvalue calculé à partir de 100 tirage)
      picVal <- peaks(bruit_1,bruit_2,nrands = 100)$pval 
      pic <- round(peaks(bruit_1,bruit_2)$obs,3)
      ligne1 <- paste("Taux de synchronicité des pics :",pic) # Fabrication du texte pour l'affichage
      if(picVal<0.05){
        ligne2 <- "Les pics des deux courbes sont atteints en même temps très souvent."
      }else{
        ligne2 <- "On ne peut pas dire que les pics des deux courbes sont souvent atteints en même temps."
      }
      HTML(paste(ligne1,ligne2,sep="<br/>"))
    })
  }) 
  
  
  output$downloadData2 <- downloadHandler(
    filename = "Comparaison_capteur.csv", # Nom du fichier importé
    content = function(file) {
      write_excel_csv2(prep_tabl2(), file, row.names = FALSE,fileEncoding = "UTF-8")
    }
  )
  
################################################################################################################# 
  
#######  
  
# Onglet Heure d'engorgement
  
#######
  
  
  # Graphique 
  
  output$plot_heure_eng <- renderPlotly({
    
    plot_eng_react()
    
  }) 
  
  
  
  
  
##########################################################################################################################
#
# Gestion de l'affichage de l'UI
#
##########################################################################################################################  
  
  
  
  output$Etat <- renderText( # Permet d'afficher l'état de l'import sur la page d'acceuil
    if(is.null(donnee_import()$geomet)){
      if(test_capt()){
        "L'identifiant n'est pas valide"  
      }else{
      "Import à faire"
      }
    }else{
      "Done"
    }
  )
  
  
  
################################################################################################################# 
  
#######  
  
# Onglet Test par période
  
#######  
  

  # Permet de faire apparaitre la liste de capteur parmis lesquelles l'utilisateur peut choisir une fois l'import fait
  output$Box1 = renderUI( 
    if (is.null(liste_capteur())){return()
    }else{selectInput( "capteur", 
                       label = "Choix du capteur", 
                       choices = liste_capteur(),
                       selected = liste_capteur()[1])}
  )
  # Permet de demander l'import s'il na pas été fait
  output$OutBox1 = renderUI(
    if (is.null(donnee_import()$donnee)){return("Import necessaire")
    }else{uiOutput("OutBox1_bis")}
  )
  # Permet de demander de selectionner un jour si aucun ne l'a été
  output$OutBox1_bis = renderUI(
    if (length(input$SM1)==0|length(input$SM2)==0){return("Selectionner au moins 1 jour pour les deux périodes")
    }else{uiOutput("OutBox1_ter")}
  )
  # Permet de renvoyer un message d'erreur si les données pour la periode 1 sont vides
  output$OutBox1_ter = renderUI(
    if (mode(tableau_P1())=="character"){return(tableau_P1())
    }else{uiOutput("OutBox1_quad")}
  )
  # Permet de renvoyer un message d'erreur si les données pour la periode 2 sont vides
  output$OutBox1_quad = renderUI(
    if (mode(tableau_P2())=="character"){return(tableau_P2())
    }else{
      plotOutput("plot")} #Et afficher le graphique sinon
  )
  
  # N'affiche rien si l'import n'a pas été fait
  output$OutBox12 = renderUI(
    if (is.null(donnee_import()$donnee)){return(
    )
    }else{uiOutput("OutBox12_bis")}
  )
  # Permet de demander de selectionner un jour si aucun ne l'a été
  output$OutBox12_bis = renderUI(
    if (length(input$SM1)==0|length(input$SM3)==0){return("Selectionner au moins 1 jour pour les deux périodes")
    }else{uiOutput("OutBox12_ter")}
  )
  # Ne renvoit rien si les données pour la periode 1 sont vides (le message d'erreur est déjà afficher)
  output$OutBox12_ter = renderUI(
    if (mode(tableau_P1())=="character"){return()
    }else{uiOutput("OutBox12_quad")}
  )
  # Permet de renvoyer un message d'erreur si les données pour la periode 2 sont vides
  output$OutBox12_quad = renderUI(
    if (mode(tableau_P3())=="character"){return(tableau_P3())
    }else{
      plotOutput("plot2")} #Et afficher le graphique sinon
  )
  
  
  #Permet d'afficher le nombre de valeurs moyen si possible
  output$OutBox11 = renderUI(
    if (is.null(liste_capteur())|mode(tableau_P1())=="character"|mode(tableau_P2())=="character"|mode(tableau_P3())=="character"|length(input$SM1)==0|length(input$SM2)==0|length(input$SM3)==0){return()
    }else{
      htmlOutput("NbLignes")
    }
  )
  
  #Permet d'afficher le bouton d'import si toutes les conditions sont bonnes
  output$OutBox9 = renderUI(
    if (is.null(liste_capteur())|mode(tableau_P1())=="character"|mode(tableau_P2())=="character"|mode(tableau_P3())=="character"|length(input$SM1)==0|length(input$SM2)==0|length(input$SM3)==0){return()
    }else{
      downloadButton("downloadData", "Import des données")
    }
  )
  
  #Permet d'afficher le titre du Premier graphique si tout va bien
  output$OutBox13 = renderUI(
    if (is.null(liste_capteur())|mode(tableau_P1())=="character"|mode(tableau_P2())=="character"|length(input$SM1)==0|length(input$SM2)==0){return()
    }else{
      h2("Comparaison avec la première période")
    }
  )
  
  #Permet d'afficher le titre du Second graphique si tout va bien
  output$OutBox14 = renderUI(
    if (is.null(liste_capteur())|mode(tableau_P1())=="character"|mode(tableau_P3())=="character"|length(input$SM1)==0|length(input$SM3)==0){return()
    }else{
      h2("Comparaison avec la seconde période")
    }
  )
  
  # Affiche la remarque pour l'interprttaion des test si tout va bien.
  output$OutBox15 = renderUI(
    if (is.null(liste_capteur())|mode(tableau_P1())=="character"|mode(tableau_P3())=="character"|length(input$SM1)==0|length(input$SM3)==0){return()
    }else{
      HTML("(*) Remarques relatives à la significativité de la différence de comportement : <br/>
      Pour chaque créneau horaire, la couleur indique s'il y a un comportement différent des usagers entre les deux périodes.
      Si le résultat est <i>Significatif</i>, c'est qu'il y a très probablement un changement de comportement entre les deux périodes (pour l'heure concernée).
      Si le résultat est <i>Entre deux</i>, alors il est possible qu'il y ait une différence.
      Si le résultat est <i>Non-significatif</i>, on ne peut pas dire qu'il y ait une différence.")
    }
  )
  
################################################################################################################# 
  
#######  
  
# Onglet Seuil d'engorgement
  
####### 
  

  # Permet de faire apparaitre la liste de capteur parmis lesquelles l'utilisateur peut choisir après l'import
  output$Box4 = renderUI( 
    if (is.null(liste_capteur())){return()
    }else{selectInput( "capteur3", 
                       label = "Choix du capteur", 
                       choices = liste_capteur(),
                       selected = liste_capteur()[1])}
  )
  
  # Permet de demander l'import s'il na pas été fait
    output$OutBox2 = renderUI(
    if (is.null(donnee_import()$donnee)){return("Import necessaire")
    }else{uiOutput("OutBox8")}
  )
  
  # Permet d'afficher un message d'erreur si il n'y a pas assez de données pour calculer un seuil
  output$OutBox8 = renderUI(
    if (length(Tabl_Engor()[,1])<100){return("Attention : période trop courte ou pour laquelle le capteur ne possede pas de données !")
    }else{
      plotOutput("plot_seuil") # sinon affiche le seuil
    }
  )
  
  output$OutBox17 = renderUI(
    if(is.null(donnee_import()$donnee)){return()
      }else{
    if (length(Tabl_Engor()[,1])<100 ){return()
    }else{
      downloadButton("downloadbrut", "Import des données des courbes brutes (noire)")
      }}
  )
  
  output$OutBox18= renderUI(
    if(is.null(donnee_import()$donnee)){return()
    }else{
      if (length(Tabl_Engor()[,1])<100 ){return()
      }else{
        downloadButton("downloadlisse", "Import des données des courbes lissées")
      }}
  )
   
################################################################################################################# 
  
#######  
  
# Onglet Comparaison de deux capteurs
  
#######   
  
  # Pour le choix du premier capteur:
  # Permet de faire apparaitre la liste de capteur parmis lesquelles l'utilisateur peut choisir apres l'import
  output$Box2 = renderUI( 
    if (is.null(liste_capteur())){return()
    }else{selectInput( "capteur1", 
                       label = "Choix du premier capteur", 
                       choices = liste_capteur(),
                       selected = liste_capteur()[1])}
  )
  
  # Pour le choix du second capteur:
  # Permet de faire apparaitre la liste de capteur parmis lesquelles l'utilisateur peut choisir apres l'import
  output$Box3 = renderUI( 
    if (is.null(liste_capteur())){return()
    }else{selectInput( "capteur2", 
                       label = "Choix du second capteur", 
                       choices = liste_capteur(),
                       selected = liste_capteur()[1])}
  )
  

  # Permet de demander l'import s'il na pas été fait
  output$OutBox3 = renderUI(
    if (is.null(donnee_import()$donnee)){return("Import necessaire")
    }else{
      uiOutput("OutBox3_bis")
    }
  )
  
  
  
  
  
  # Permet d'afficher un message d'erreur s'il n'y a pas assez de données
  output$OutBox3_bis = renderUI(
    if (mode(Compar_tabl())=="character"){return(Compar_tabl())
    }else{plotOutput("plot_tend")}
  )
  
  # Permet de demander l'import s'il na pas été fait
  output$OutBox4 = renderUI(
    if (is.null(donnee_import()$donnee)){return("Import necessaire")
    }else{
      uiOutput("OutBox4_bis")
    }
  )
  # Permet d'afficher un message d'erreur s'il n'y a pas assez de données
  output$OutBox4_bis = renderUI(
    if (mode(Compar_tabl())=="character"){return(Compar_tabl())
    }else{plotOutput("plot_cycle")}
  )
  
  
  # Permet de demander l'import s'il na pas été fait
  output$OutBox5 = renderUI(
    if (is.null(donnee_import()$donnee)){return("Import necessaire")
    }else{
      uiOutput("OutBox5_bis")
    }
  )
  # Permet d'afficher un message d'erreur s'il n'y a pas assez de données
  output$OutBox5_bis = renderUI(
    if (mode(Compar_tabl())=="character"){return(Compar_tabl())
    }else{plotOutput("plot_bruit")}
  )
  
  
  # N'affiche rien si l'import n'a pas été fait
  output$OutBox6 = renderUI(
    if (is.null(donnee_import()$donnee)){return()
    }else{
      uiOutput("OutBox6_bis")
    }
  )
  # N'affiche rien si il n'y a pas assez de données
  output$OutBox6_bis = renderUI(
    if (mode(Compar_tabl())=="character"){return()
    }else{htmlOutput("correlation")} #Sinon affiche le taux de correlation
  )
  # N'affiche rien si l'import n'a pas été fait
  output$OutBox7 = renderUI(
    if (is.null(donnee_import()$donnee)){return()
    }else{
      uiOutput("OutBox7_bis")
    }
  )
  # N'affiche rien si il n'y a pas assez de données
  output$OutBox7_bis = renderUI(
    if (mode(Compar_tabl())=="character"){return()
    }else{htmlOutput("pics")} #Sinon affiche le taux de synchronicité des pics
  )
  
  #Permet d'afficher le bouton d'import si toutes les conditions sont bonnes
  output$OutBox10 = renderUI(
    if (is.null(donnee_import()$donnee)){return()
    }else{
      if(mode(Compar_tabl())=="character"){return()
        }else{downloadButton("downloadData2", "Import des données")}
    }
  )
  
################################################################################################################# 
  
#######  
  
# Onglet Heure d'engorgement
  
#######  
  
  # Permet de faire apparaitre la liste de capteur parmis lesquelles l'utilisateur peut choisir après l'import
  output$Box5 = renderUI(
    if (is.null(input$Capteurs)){return()
    }else selectInput( "capteur4", 
                       label = "Choix du capteur", 
                       choices = input$Capteurs,
                       selected = input$Capteurs[1])
  )
  
  # Permet de demander l'import s'il na pas été fait, affiche le graphique si l'import est bon
  output$OutBox16 = renderUI(
    if (is.null(donnee_import()$donnee)){return("Import necessaire")
    }else{
      uiOutput("OutBox16_bis")
    }
  )
  
  output$OutBox16_bis = renderUI(
    if (mode(plot_eng_react())=="character"){return(plot_eng_react())
    }else{plotlyOutput("plot_heure_eng")}
  )
  
  
  
  
  
})



