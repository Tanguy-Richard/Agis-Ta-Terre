

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
               p("Capteur supplémentaire:"),
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
      tabPanel("Test par periode", 
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
                        uiOutput("OutBox9")
                        
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
                          uiOutput("OutBox3")),
                          tabPanel("Cycle",
                          uiOutput("OutBox4")),
                          tabPanel("Bruit",
                          uiOutput("OutBox5"),
                          uiOutput("OutBox6"),
                          uiOutput("OutBox7"))),
                        uiOutput("OutBox10")
                 )
               )
      )
    ))     
)