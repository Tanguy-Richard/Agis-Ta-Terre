

shinyUI(
  fluidPage(
    theme = shinytheme("journal"),
    # navbarPage
    navbarPage(
      tags$img(src="AGT.png",height=35),
      tabPanel("Import",
               h2("Les données"),
               p("L'application importe des données depuis le site de Telraam :",
                 tags$a(href="https://telraam.net/#14/48.1014/-1.3862","ici"),
                 ". L'import de toute les données est relativemant lourd et peut prendre du temps. Veuillez donc ne selectionner que les données que vous utiliserez :"), 
               fluidRow(
                 column(5,
               checkboxGroupInput(
                 inputId="Capteurs",
                 label="Choix des capteurs",
                 choiceNames = listeNombis,
                 choiceValues = listeNom
               ),
               h5("Capteur supplémentaire:"),
               fluidRow(
               column(6,
                      textInput("captIDsup", "Identifiant", value = "")),
               column(6,
                      textInput("nomIDsup", "Nom", value = "")))
               ,
               dateRangeInput("daterange", "Période",
                              start  = "2021-01-01",
                              end    = Sys.Date()-days(1),
                              min    = "2021-01-01",
                              max    = Sys.Date()-days(1)),
               actionButton("go_import", "Import"),
               textOutput("Etat")),
               column(7,
                      tags$img(src="carte capteur apli.jpg",height=500)))
                      
               ),
      tabPanel("Comparaison de periodes", 
               fluidRow(
                 column(3,wellPanel(
                   uiOutput("Box1"),
                   selectInput("sens", label = "Direction", 
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft"),
                               selected = "Toute"),
                   checkboxGroupInput(
                     "mobilite",
                     "Choix du type de mobilité",
                     selected = "car",
                     choiceNames = c("VL","PL","Piéton","Vélo"),
                     choiceValues = c("car","heavy","pedestrian","bike")
                   ),
                   actionButton("mise_a_j", "Mettre à jour"),
                   h2("Periode de référence"),
                   dateRangeInput("daterange1", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   radioButtons(inputId = "Vacance1", label = "Vancances comprises :",
                                choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
                   radioButtons(inputId = "JF1", label = "Jours fériés compris :",
                                choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
                   checkboxGroupInput(
                     "SM1",
                     "Choix des jours",
                     selected = 1:7,
                     choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
                     choiceValues = 1:7,
                     inline = TRUE
                   ),
                   h2("Première période de comparaison "),
                   dateRangeInput("daterange2", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   radioButtons(inputId = "Vacance2", label = "Vancances comprises :",
                                choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
                   radioButtons(inputId = "JF2", label = "Jours fériés compris :",
                                choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
                   checkboxGroupInput(
                     "SM2",
                     "Choix des jours",
                     selected = 1:7,
                     choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
                     choiceValues = 1:7,
                     inline = TRUE
                   ),
                   h2("Seconde période de comparaison"),
                   dateRangeInput("daterange4", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   radioButtons(inputId = "Vacance3", label = "Vancances comprises :",
                                choices = c("Oui","Non","Seulement les vacances"),selected = "Oui"),
                   radioButtons(inputId = "JF3", label = "Jours fériés compris :",
                                choices = c("Oui","Non","Seulement les jours fériés"),selected = "Oui"),
                   checkboxGroupInput(
                     "SM3",
                     "Choix des jours",
                     selected = 1:7,
                     choiceNames = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"),
                     choiceValues = 1:7,
                     inline = TRUE
                   )
                 )), 
                 column(width = 9,
                        uiOutput("OutBox13"),
                        uiOutput("OutBox1"),
                        uiOutput("OutBox14"),
                        uiOutput("OutBox12"),
                        uiOutput("OutBox11"),
                        uiOutput("OutBox9"),
                        htmlOutput("OutBox15")
                        
                 )
               )
      ),
      tabPanel("Seuil d'engorgement",
               fluidRow(
                 column(3,wellPanel(
                   uiOutput("Box4"),
                   selectInput("sens3", label = "Direction du capteur", 
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft")),
                   selectInput("vit", label = "Choix de la courbe servant à déterminer le seuil",
                               choices = c("Toute","plus de 10km/h","plus de 20km/h","plus de 30km/h","plus de 40km/h"),
                               selected = "Toute"),
                   dateRangeInput("daterange3", "Période",
                                  start  = "2021-01-01",
                                  end    = Sys.Date()-days(1),
                                  min    = "2021-01-01",
                                  max    = Sys.Date()-days(1)),
                   actionButton("mise_a_j2", "Mettre à jour")
                 )), 
                 column(width = 9,
                        h5("Précisions sur le graphique"),
                        p("Le graphique suivant indique pour chaque courbe le pourcentage de conducteurs (véhicules légers
                     et poids lourds) qui arrivent à dépasser la vitesse spécifier en fonction du nombre d'autres 
                     conducteurs sur la route durant une même période horaire."),
                        p("Un changement brusque dans les courbes peut indiquer une présence régulière d'embouteillage 
                     lorsqu'on dépasse la valeur du changement. La barre rouge indique cette valeur."),
                        br(),
                        p("Attention : la barre apparait toujours, même pour les routes sans embouteillages. Le calcul conduisant
                     au placement de la barre souffre de défaut, elle peut être mal placée."),
                        uiOutput("OutBox2")
                 )
               )
      ),
      tabPanel("Comparaison de deux capteurs",
               fluidRow(
                 column(3,wellPanel(
                   uiOutput("Box2"),
                   selectInput("sens1", label = "Direction du capteur",  
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft")),
                   uiOutput("Box3"),
                   selectInput("sens2", label = "Direction du capteur",  
                               choices = c("Toute"="Toute","B vers A"="Rgt","A vers B" ="Lft")),
                   selectInput("heure", label = "Choix de l'heure", choices = 7:20,selected = 9),
                   checkboxGroupInput(
                     "mobilite2",
                     "Choix du type de mobilité",
                     selected = c("car","heavy"),
                     choiceNames = c("VL","PL","Piéton","Vélo"),
                     choiceValues = c("car","heavy","pedestrian","bike")
                   ),
                   radioButtons(inputId = "Norm1", label = "Normaliser :",
                                choices = c("Oui","Non"),selected = "Non",inline = TRUE),
                   actionButton("mise_a_j3", "Mettre à jour")
                 )), 
                 column(width = 9,
                        tabsetPanel(
                          tabPanel("Tendance",
                          h3("Comment évolue la circulation au long de la présence du capteur?"),
                          uiOutput("OutBox3")),
                          tabPanel("Cycle hebdomadaire",
                          h3("Quel est l'effet des jours de la semaines ?"),
                          uiOutput("OutBox4")),
                          tabPanel("Bruit",
                          h3("Peut-on dire que les usagers ont le même comportement sur les deux segments de routes ?"),
                          uiOutput("OutBox5"),
                          uiOutput("OutBox6"),
                          uiOutput("OutBox7"))),
                        uiOutput("OutBox10")
                 )
               )
      ),
      tabPanel("Avertissement et trivia",
          h3("Avertissement relatif à la qualité des données :"),
          p("Les données des capteurs Telraam ne sont pas issues d’une mesure continue sur une heure. Pour améliorer la qualité des données futures, les capteurs dédient une partie de leur temps d’activité à l’apprentissage. Les données totales sont reconstituées à partir du temps de mesures. Plus cette période de mesure est longue plus la qualité des données est grande. Telraam donne un outil de mesure de ce temps de mesure : l’uptime. Dans cette application, nous avons conservé que les données d’uptime supérieur à 0.5 (seuil conseillé par Telraam). Toutefois, les capteurs placés récemment (en période d’apprentissage) et les données matinales ou dans la soirée (visibilité réduite à cause de la nuit)  peuvent présenter des uptimes plus faible. 
          De plus la suppression des données à l’uptime trop faible fait qu’on possède moins de données pour les périodes à risque. La qualité des estimations et des tests est moins bonne sur ces périodes.
          Il faut donc être prudent en interprétant ces données."),
          h3("Avertissement relatif aux catégories de mobilités :"),
          p("Les capteurs Telraam utilisés ont des difficultés à différencier les grosses voitures comme les SUV des poids lourds. Le nombre de poids lourds est donc sur-évalué et le nombre de voitures sous-évalué. Toutefois, le total voitures + camions est précis.
          De la même façon, il faut être prudent dans la différenciation entre vélos et piétons."),
          h3("Explicitation des statistiques mis en place (nécessite des compétences en statistique) :"),
          h4("Comparaison de périodes :"),
          p("Méthode pour tracer les courbes :") ,
          p("Selon la sélection de l’utilisateur, on filtre les données pour ne garder que le trafic correspondant aux mobilités, capteur, direction et contraintes de dates sélectionnés. On réalise ensuite une moyenne pour chaque créneau horaire.
          On a rajouté un intervalle de confiance à 95% autour de nos courbes. Pour chaque créneau horaire, on a estimé la variance des données, ce qui nous a permis d’obtenir l’intervalle de confiance (à partir d’une loi de Student)."),
          br(),
          p("Méthode pour la significativité de la différence :"),
          p("On s’appuie sur un test de Wilcoxon Mann Whitney. L’idée est de comparer, pour chaque créneau horaire, la répartition des valeurs de chacune des périodes. Le test consiste à regarder la distance entre les fonctions de répartition empirique, si elles sont éloignées, le test rejette l’hypothèse nulle :  l’égalité des lois. L’option « Significatif » indique une p-value inférieure à 0.05, celle « Entre deux » une  p-value entre 0.05 et 0.1 et celle « Non significatif » une p-value supérieure à 0.1."),
          h4("Seuil d’engorgement :"),
          p("Méthode pour tracer les courbes :"),
          p("On commence par filtrer les données selon les sélections de l’utilisateur. On isole la partie correspondant au pourcentage de conducteur dépassant chaque vitesse. On range les données dans l’ordre croissant du nombre de véhicules (voitures + camions). On a pré-lissé les données à l’aide d’une moyenne glissante d’une amplitude de 50 pour dégager un début tendance (courbe noir du graphique). À partir de cette tendance, on a lissé nos données à l’aide de l’outil geom_smooth de R. Ces courbes de lissages sont les courbes colorées du graphique."),
          br(),
          p("Méthode pour trouver le seuil :"),
          p("L’objectif est de déterminer un seuil de rupture dans la courbe de lissage. Pour cela, on utilise un test de Darling Erdös (dérivé du test de CUSUM). La fonction est implémentée dans le Package",
            tags$a(href="https://github.com/ntguardian/CPAT","CPAT"), 
            "(Curtis Miller)."),
          h4(" Comparaison de période :"),
          p("Séparation en tendance, cycle et bruit :"),
          p("Après un filtrage des données selon les choix de l’utilisateur, on détermine la période d’activité commune des deux capteurs sélectionnées. 
          Pour trouver la tendance (évolution générale du flux),  pour chaque capteur, on réalise une moyenne glissante sur 14 jours (2 périodes hebdomadaires), c’est le graphique du premier onglet. 
          On soustrait la tendance au flux total pour avoir de données sans tendance. Pour ces données, on fait une moyenne sur tous les lundis, puis les mardis, ect. Cela nous donne les valeurs associées au cycle de la semaine (second onglet). La partie restante après la soustraction du cycle hebdomadaire correspond au bruit statistique (troisième onglet)."),
          br(),
          p("Indicateurs du lien entre les bruits :"),
          br(),
          p("Le premier indicateur est le coefficient de corrélation de Pearson. On a fait choix de seuils pour afficher différents commentaires :"),
          p("1. Pour un coefficient plus grand que 0.5 on considère que les courbes sont corrélées."),
          p("2. Pour un coefficient entre 0.2 et 0.5 on considère que la corrélation est légère."),
          p("3. Pour un coefficient inférieur à 0.2 on considère que les courbes sont non corrélées."),
          br(),
          p('Le second indicateur est un indicateur de la proportion d’extremum commun entre les deux courbes. Pour cela, on utilise la fonction « peaks » du package «', 
            tags$a(href="https://github.com/tgouhier/synchrony","synchrony"),
          '». Cette fonction compte le nombre de fois où les deux séries atteignent un maximum en même temps, puis les minimums pour ramener cela à la proportion total de pics (déterminer en sommant le nombre de maxima de la série en comptant le plus à celui de minima).
          Pour tester si ce nombre est important la fonction procède à une estimation via  une méthode de Monte Carlo, en mélangeant plusieurs fois les deux séries pour observer le nombre de pics communs dans chaque cas, et voir si ces valeurs sont éloignées ou non de la proportion initiale.
          Si on rejette l’hypothèse que la synchronicité des pics est du au hasard, on affiche "Les pics des deux courbes sont atteints en même temps très souvent.", sinon "On ne peut pas dire que les pics des deux courbes sont souvent atteints en même temps.".')
      )
    ))     
)