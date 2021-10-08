library(mapedit)
library(mapview)
library(shiny)
library(leaflet)
library(leafpop)
library(tidyverse)
library(data.table)
library(shinydashboard)
library(survey)
library(questionr)
library(rAmCharts)
library(rsconnect)
library(DT)
library(plotly)
library(htmlwidgets)
library(plyr)
library(dplyr)
library(sqldf)
library(shiny)
library(shinythemes)
library(stringi)
library(ggplot2)
library(plotly)
library(devtools)
library(Rcpp)
library(RCurl)
library(RJSONIO)
library(DT)
library(rjson)
require(devtools)
library(treemap)
library(d3Tree)
library(writexl)
library(shinyWidgets)
library(gridExtra)
library(stringr)
library(sf)
library(leaflet.extras)
library(highcharter)
library(urltools)
library(stringr)
library(rvest)
# load("datanet.RData")
# load("data_tendance.RData")

data_net<-readRDS("data_net_complet")
IBD_net<-readRDS("IBD_net22")
IBD_per<-readRDS("IBD_per")
t_ssbv<-readRDS("t_ssbv")
tend_nat<-readRDS("tend_nat")
t_nat<-readRDS("t_nat")
tendance_IBD<-readRDS("tendance_IBD")
AG<-readRDS("tendance_bv")

tauxbv<-readRDS("tableaubv")
tauxssbvb<-readRDS("tableau_ssbv_ibd")
tauxbvb<-readRDS("tableau_bv_bis_ibd")
tauxssbv<-readRDS("tableau_ssbv")
data_net$ipr<-round(data_net$ipr,2)
data_net$ipr2<-format(round(data_net$ipr,2),nsmall=2)
data_net$ope_date<-as.Date(data_net$ope_date)
data_net$com_libelle<-str_to_title(str_to_lower(data_net$com_libelle))
data_net$sta_libelle_sandre<-str_to_sentence(str_to_lower(data_net$sta_libelle_sandre))
# data_net$url<-list(paste("www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:StationMesureEauxSurface:FRA:code:",data_net$sta_code_sandre,":::::html"))
data_net<-data_net%>%mutate(
  type= case_when(
    classe_ipr=="Tres bon" ~ "Tr\u00e8s bon",
    classe_ipr=="Bon" ~ "Bon",
    classe_ipr=="Moyen" ~ "Moyen",
    classe_ipr=="Mediocre" ~"M\u00e9\U0064iocre",
    classe_ipr=="Mauvais" ~ "Mauvais"
  )
)
IBD_net<-IBD_net%>%mutate(
  type1= case_when(
    classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
    classe_ibd=="Bonne" ~ "Bonne",
    classe_ibd=="Passable" ~ "Passable",
    classe_ibd=="Mauvaise" ~"Mauvaise",
    classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
  )
)
qualite<-c("Très"="Tres bon", "Bon","Moyen","Mediocre","Mauvais")
data_tendance<-readRDS("data_tendance")
data_net2<-data_net%>%select('Annee'=annee,'Point prelevement'=pop_id,'Nom de la station'=sta_libelle_sandre,'Code SANDRE de la station'=sta_code_sandre,'Fiche detaillee de la station'=url_site,
                             'Valeur IPR'=ipr2,'Classe de qualite'=type,'Date de prelevement'=ope_date,'Sous-Bassin'=NomSsBassi,
                             'Region'=reg_libelle,'Departement'=dep_libelle,'Code Insee'=dep_code_insee,
                             'Commune'=com_libelle,"Cours d'eau"=enh_libelle_sandre,"Code SANDRE du cours d'eau"=enh_code_sandre)
IBD_net2<-IBD_net%>%select('Annee'=annee,'Nom de la station'=LbStationMesureEauxSurface,'Code SANDRE de la station'=CODE_SANDRE,
                        'Valeur IBD'=IBD,'Classe de qualite'=type1,'Date de prelevement'=DATE_DEBUT,'Sous-Bassin'=NomSsBassi,
                        'Region'=LbRegion,'Departement'=LbDepartement,
                        'Commune'=LbCommune)
tend_ssbv<-sqldf::sqldf('select distinct(NomSsBassi) from tendance_IBD order by NomSsBassi asc')
tend_bv<-sqldf::sqldf('select distinct(BV) from tendance_IBD order by BV asc')

# data_tendance<-res_bv
bv_tend<-readRDS("bv_tend")
ssbv_tend<-readRDS("ssbv_tend")
# Choix de Zone Géographique
zone<-list("France métropolitaine","Bassin hydrographique","Sous-bassin hydrographique","Région", "Département","Cours d'eau"="zon")
tend_nat<-readRDS("tend_nat")
zs<-list("France métropolitaine","Bassin hydrographique","Sous-bassin hydrographique","Région", "Département","Cours d'eau"="zon")
saveRDS(tend_ssbv,"tend_ssbv")
tend_ssbv<-readRDS("tend_ssbv")
periode<-readRDS("periode")
periode

# # Les questions réparties en thématiques
station<-readRDS("station")

Region<-readRDS("Region")
Departement<-readRDS("Departement")
Ssbv<-readRDS("Ssbv")
# qualite<-readRDS("qualite")
bv<-readRDS("BV")
cd<-readRDS("cd")
leg<-readRDS("leg")
color<-readRDS("color")
BV<-readRDS("bvv")
CD<-readRDS("cdb")
year<-2010:2020
years<-2010:2020


zoni<-list("France métropolitaine","Bassin hydrographique","Sous-bassin hydrographique","Région", "Département","Cours d'eau"="zon")
stationb<-sqldf('select distinct(LbStationMesureEauxSurface) from IBD_net order by LbStationMesureEauxSurface asc')

Regionb<-sqldf('select distinct(LbRegion) from IBD_net order by LbRegion asc')
Departementb<-sqldf('select distinct(LbDepartement) from IBD_net order by LbDepartement asc')
qlt<-sqldf('select distinct(classe_ibd) from IBD_net order by  CASE classe_ibd when classe_ibd=="Tres bonne" then 1
                 when classe_ibd=="Bonne" then 2
                when classe_ibd=="Passable" then 3
                when classe_ibd=="Mauvaise" then 4
                else 5 end ')

Ssbvb<-sqldf('select distinct(NomSsBassi) from IBD_net order by NomSsBassi asc')

# ui object

ui <-fluidPage(theme = "bootstrap.css",
               header = tagList(
                 useShinydashboard()
               ),
               includeCSS("styles.css"),
               fluidRow(
                 br(),
                 column(1,offset=1,
                        imageOutput("image2",height="70px")
                 ),
                 
                 column(8, align="center",
                        strong(p("Site de visualisation des données de l'Indice Poisson Rivière (IPR) et de l'Indice Biologique Diatomées (IBD)",style="color:black;font-size:22px;padding-top:15px;text-align:center;color:#466964")),
                 ),
               ),
               tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);"),
               tags$head(tags$style(
                 type="text/css",
                 "#image2 img {height:70px;}"
               )),
                 
               br(),
               
  navbarPage("",selected="Présentation",windowTitle="IPR-IBD 2010-2020",
             
             
             header = tagList(
               useShinydashboard()
             ),
             
             setBackgroundColor(
               color = rgb(0.96,0.96,1),
             ),      
             
             
             tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);"),
             
             
             # mise en page des tableaux de données #
             
             tags$head(tags$style("#DataTables_Table_0_length{
                                 text-align:left;
                                 
                                 }"
             )
             ),
             
             tags$head(tags$style("#DataTables_Table_1_length{
                                 text-align:left;
                                 
                                 }"
             )
             ),
             tags$head(tags$style("#DataTables_Table_2_length{
                                 text-align:left;
                                 
                                 }"
             )
             ),
             tags$head(tags$style("#DataTables_Table_3_length{
                                 text-align:left;
                                 
                                 }"
             )
             ),
             tags$head(tags$style("#DataTables_Table_4_length{
                                 text-align:left;
                                 
                                 }"
             )
             ),
             tags$head(tags$style("#DataTables_Table_5_length{
                                 text-align:left;
                                 
                                 }"
             )
             ),
             tags$head(tags$style("#DataTables_Table_6_length{
                                 text-align:left;
                                 
                                 }"
             )
             ),
             
             tags$head(tags$style(
               type="text/css",
               "#image2 img {height:70px;width:90px;text-align:center}"
             )),
             
             
             tags$head(tags$style(
               type="text/css",
               "#image1 img {height:70px;}"
             )),
             
             
             ### INTRO ###    
             
             
             tabPanel("Présentation",
                      header = tagList(
                        useShinydashboard()
                      ),
                      fluidPage(

                        
                        fluidRow(
                          
                          column(12,style="border-radius: 10px;padding-left:
                                 300px;padding-right:300px;height:100px",
                                 imageOutput("image3")
                          ),
                        ),
                        
                        tags$head(tags$style("#image3 img{
                                 max-width: 100%; width: 100%;
                                 
                                 height:100px;
                                 }
                                 }"
                        )
                        ),
                        
                        
                        
                        fluidRow(
                          column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px",
                                 
                                 
                                 strong(p("L’Indice Poisson Rivière (IPR)",style="text-align:justify;color:black;padding:10px;font-size:20px")),
                                 
                                 
                                 p("Organisme intégrateur situé à l’extrémité de la chaîne alimentaire, le poisson est un bon indicateur des perturbations rencontrées dans le milieu aquatique (altérations de régimes d’écoulements, augmentation de la température, débits, etc). Les peuplements piscicoles pêchés par pêche à pieds électrique au niveau national (France métropolitaine hors Corse) permettent de déterminer un indice émanant de la Directive cadre sur l’Eau (Indice Poisson Rivière - IPR) visant à caractériser la qualité des cours d’eau. ",style="text-align:justify;color:black;padding-left:15px"),
                                 
                                 
                                 p("Les paramètres pris en compte sont réputés refléter l'état écologique de la masse d'eau (composition du peuplement piscicole, richesse taxonomique, structure trophique, abondance). 34 espèces les mieux représentées en France modélisent la répartition en situation de référence. 5 classes de qualité allant de 0 (lorsque le peuplement est parfaitement conforme au peuplement de référence) à 150 (dans les situations les plus altérées) sont établies.",style="text-align:justify;color:black;padding-left:15px"),
                                 strong(p("Méthodologie",style="text-align:justify;color:black;padding-left:15px;text-decoration:underline;")),
                                 
                                 p("L'IPR est calculé à partir d'échantillons de peuplements de poissons obtenus par pêche à l'électricité. Lorsque des pêches à plusieurs passages successifs sont mises en œuvre (méthode de Lury, par exemple), seuls les résultats du premier passage sont utilisés pour le calcul de l'indice. Son calcul nécessite de connaître : la surface échantillonnée exprimée, le nombre d'individus capturés pour chaque espèce ou groupe, et les 9 variables environnementales : surface du bassin versant, distance source, largeur moyenne, pente, profondeur moyenne, altitude, température moyenne interannuelle de l'air en juillet et janvier, unité hydrographique. La valeur de l'indice est fixée en sommant les scores obtenus par les 7 métriques : nombre total d'espèces, nombre d'espèces lithophiles, nombre d'espèces rhéophiles (vivant dans le courant), densité d'individus « tolérants », densité d'individus invertivores, densité d'individus omnivores et densité totale d'individus.",style="text-align:justify;color:black;padding-left:15px"),
                                 p(a( "Accéder à la notice de présentation de l'indice",href="http://oai.afbiodiversite.fr/cindocoai/download/PUBLI/399/1/2006_B008.pdf_1860Ko", target="_blank"),style="text-align:justify;color:black;padding-left:15px"),
                                 
                                  ),
                        ),
                        
                        br(),
                        
                        fluidRow(
                          
                          column(12,align = "center",style="background-color:#d1dad8;border-radius: 10px",
                                 strong(p("L’Indice Biologique Diatomées (IBD)",style="text-align:justify;color:black;padding:10px;font-size:20px")),
                                 
                                 
                                 

                                 p("Les diatomées, algues brunes unicellulaires sont très répandues dans le milieu aquatique (environ 100 000 espèces connues). Sensibles aux compositions physico-chimiques, elles colonisent les substrats durs des cours d’eau.",style="text-align:justify;color:black;padding-left:15px"),
                                 
                                 
                                 p("L’établissement d’un indice (Indice Biologique Diatomées – IBD) national (France métropolitaine dont Corse) allant d’une  de 0 à 20 permet d’établir la qualité biologique de ces milieux au travers de 5 classes (de très bonne à mauvaise). ",style="text-align:justify;color:black;padding-left:15px"),
                                 
                                 p("L’interprétation de sa valeur permet de caractériser l’existence d’une pollution de l’écosystème en nutriments azotés, phosphorés et en éléments organiques pouvant conduire à une eutrophisation, une salinité ou une acidification de l’eau. L’évolution des populations de diatomées peut renseigner, en outre, sur les effets du changement climatique. ",style="text-align:justify;color:black;padding-left:15px"),
                                 strong(p("Méthodologie",style="text-align:justify;color:black;padding-left:15px;text-decoration:underline;")),
                                 p("L’échantillonnage est réalisé entre les mois de mai et octobre (période d’étiage), dans les cours d’eau naturels ou artificialisés peu profond (à l’exception des zones naturellement salées) et prélevé de préférence au centre du lit majeur, avant et après un seuil dans la mesure du possible. Le matériel benthique est récupéré par brossage de substrats durs naturels. L’échantillonnage se fait généralement sur des surface bien éclairées, préférentiellement sur des pierres, cailloux ou blocs immergés de façon à présenter un peuplement diatomique représentatif des conditions du milieu.
La fréquence d’échantillonnage dépend des attendus du suivi. En règle générale il est recommandé de réaliser un prélèvement l’année avant les travaux (état zéro), puis un prélèvement les années suivants les travaux. L’Indice Biologique Diatomées est calculé à partir des taxons contributifs. Il prend en compte l’abondance de chaque taxon et la répartition des différentes espèces suivant sept classes de qualité de l’eau.

",style="text-align:justify;color:black;padding-left:15px"),


                                 p(a( "Accéder à la notice de présentation de l'indice",href="https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwivnIOf1OryAhUNqxoKHU0gBFsQFnoECCUQAQ&url=https%3A%2F%2Fconsultation.eau-artois-picardie.fr%2FOAI_Docs%2Faegis%2F2496%2FB_16478_(12.04Mo).pdf&usg=AOvVaw02cqqgLeR10bakETDvFZUu", target="_blank"),style="text-align:justify;color:black;padding-left:15px"),
                                 
                                 
                          )
                          
                        ),
                        br(),
                      )
                      
                      
             ),
             
br(),
  tabPanel("Indice Poisson Rivière (IPR)",
           header = tagList(
             useShinydashboard()
           ),
  tabsetPanel( 
    tabPanel("Etat de l'indice",
             header = tagList(
               useShinydashboard()
             ),
             br(),
             tabsetPanel( 
             
               tabPanel("Carte",  
                        br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "zoneselected",
        label = "Sélectionner la zone géographique",
        choices =zone,selected = zone[1]
      ),

      conditionalPanel(
        condition = "input.zoneselected == 'Région'",
        
    selectInput(
        inputId = "region",
        label = "Sélectionner la région",
        choices =c("ensemble des régions",as.character(Region$reg_libelle)),multiple = T,selected =Region$reg_libelle[1]
      )),
    conditionalPanel(
      condition = "input.zoneselected == 'Département'",
      selectInput(
        inputId = "Departement",
        label = "Sélectionner le département",
        choices = c("ensemble des départements",as.character(Departement$dep_libelle)),selected =Departement$dep_libelle[1],multiple = T
      )),   

    conditionalPanel(
      condition = "input.zoneselected == 'Sous-bassin hydrographique'",
      selectInput(
        inputId = "Ssbv",
        label = "Sélectionner le sous-bassins hydrographique",
        choices =c("ensemble des sous-bassins hydrographiques",as.character(Ssbv$NomSsBassi)),multiple = T,selected=Ssbv$NomSsBassi[1]
      )),

    conditionalPanel(
      condition = "input.zoneselected == 'Bassin hydrographique'",
 selectInput(
        inputId = "Bv",
        label = "Sélectionner le bassin hydrographique",
        choices =c("ensemble des bassins hydrographiques",as.character(bv$BV)),multiple = T,selected=bv$BV[1]
      )),

   conditionalPanel(
      condition = "input.zoneselected =='zon'",
 selectInput(
        inputId = "cd",
        label = "Sélectionner le cours d'eau",
        choices =c("ensemble des cours d'eau",as.character(cd$enh_libelle_sandre)),multiple = T,selected=cd$enh_libelle_sandre[5]
      )),

selectInput(
  inputId = "yearselected",
  label = p("Sélectionner une ou deux années",br(),em("Si deux années sont sélectionnées, mettre l'année n puis l'année n+1",style="font-size:13px;text-align:justify")),
  choices =year,multiple =T,selected=2020),

selectInput(
  inputId = "qualiteIPR",
  label = "Sélectionner une ou plusieurs classe(s)",
  choices =c("ensemble  des classes de qualité", "Très bon", "Bon","Moyen","Médiocre","Mauvais"),selected ="ensemble  des classes de qualité",multiple = T
),

# sliderInput("ip", "IPR",
#             min =0,max =110,
#             value =c(0,20)),
uiOutput("PdT0"),
br(),
uiOutput("version2",align="center"),
     ),


    mainPanel(width=8,
      uiOutput("version"),
      br(),
      br(),
      leafletOutput("map",height = 800),

     
    ),

),),

  tabPanel("Tableau de données",
           header = tagList(
             useShinydashboard()
           ),
           br(),
           sidebarPanel(
           checkboxGroupInput("show_vars", "Sélectionner les champs à afficher dans le tableau de données :",
                              names(data_net2),selected = c("Annee","Nom de la station","Valeur IPR","Classe de qualite","Commune"))),
           mainPanel(width=8,dataTableOutput("v")
                                       
  ),),
tabPanel("Répartition",
         header = tagList(
           useShinydashboard()
         ),
         br(),
         sidebarLayout(
           sidebarPanel(
             selectInput(
               inputId = "zones",
               label = "Sélectionner la zone géographique",
               choices =zone[1:6],selected = zone[1]
             ),
             
             
             
             selectInput(
               inputId = "periodselected",
               label = "Sélectionner la période",
               choices =periode,selected = periode[]
             ),
             
             br(),
             br(),
             uiOutput("version4")
             
           ),
           
           
           mainPanel(width=8,
                     
                     uiOutput("version1"),
                     br(),
                     column(12,
                    column(3),
                     column(6,align="center",
                     plotlyOutput(outputId = "repartition")
                     ),
                    column(3)),
                     br(),
                     br(),
                     dataTableOutput("r"),
                     
           )),
         )

)
  
  ),


tabPanel("Tendances de l'indice",
         header = tagList(
           useShinydashboard()
         ),
         br(),
         sidebarLayout(
           sidebarPanel(
             selectInput(
               inputId = "zone",
               label = "Sélectionner la zone géographique",
               choices =zone[1:3],selected = zone[1]
             ),
             conditionalPanel(
               condition = "input.zone == 'Sous-bassin hydrographique'",
               selectInput(
                 inputId = "ssbv_tend",
                 label = "Sélectionner le Sous-bassin hydrographique",
                 choices =as.character(ssbv_tend$NomSsBassi)
               )),
             conditionalPanel(
               condition = "input.zone == 'Bassin hydrographique'",
               selectInput(
                 inputId = "bv_tend",
                 label = "Sélectionner le bassin hydrographique",
                 choices =as.character(bv_tend$BV)
               )),
             br(),
             br(),
             uiOutput("version5")
             ),
             mainPanel(width=8,
               uiOutput("version3"),
               br(),
               br(),
               uiOutput("note1"),
               br(),
               br(),


fluidRow(
  column(12,
  column(8, align="left",
         
         plotlyOutput("tendance",height="600px"),
  ),
  column(4,
         uiOutput("taux"),uiOutput("note")
         
  ))),


               br(),
               br(),
               dataTableOutput("a")
              
               
             )),
         )


)),
tabPanel("Indice Biologique Diatomées (IBD)",
         header = tagList(
           useShinydashboard()
         ),
         br(),
         
         tabsetPanel(
           tabPanel("Etat de l'indice",
                    br(),
                    tabsetPanel(
                      
                      tabPanel("Carte",
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(
                                     inputId = "zn",
                                     label = "Sélectionner la zone géographique",
                                     choices =zone,selected = zone[1]
                                   ),
                                 
                                   conditionalPanel(
                                     condition = "input.zn == 'Région'",
                                     
                                     selectInput(
                                       inputId = "reg",
                                       label = "Sélectionner la région. Plusieurs sous niveaux peuvent être séléctionnés.",
                                       choices =c("ensemble des régions",as.character(Regionb$LbRegion)),multiple = T,selected=Regionb$LbRegion[1],
                                     )),
                                   conditionalPanel(
                                     condition = "input.zn == 'Département'",
                                     selectInput(
                                       inputId = "Dep",
                                       label = "Sélectionner le département. Plusieurs sous niveaux peuvent être séléctionnés.",
                                       choices = c("ensemble des départements",as.character(Departementb$LbDepartement)),selected = Departementb$LbDepartement[1],multiple = T
                                     )),   
                                   
                                   conditionalPanel(
                                     condition = "input.zn == 'Sous-bassin hydrographique'",
                                     selectInput(
                                       inputId = "Ssb",
                                       label = "Sélectionner le Sous-bassins hydrographique. Plusieurs sous niveaux peuvent être séléctionnés.",
                                       choices =c("ensemble des sous-bassins hydrographiques",as.character(Ssbvb$NomSsBassi)),multiple = T,selected=Ssbvb$NomSsBassi[1],
                                     )),

                                   conditionalPanel(
                                     condition = "input.zn== 'Bassin hydrographique'",
                                     selectInput(
                                       inputId = "Bvb",
                                       label = "Sélectionner le bassin hydrographique",
                                       choices =c("ensemble des bassins hydrographiques",as.character(BV$BV)),multiple = T,selected=BV$BV[1]
                                     )),

                                   conditionalPanel(
                                     condition = "input.zn =='zon'",
                                     selectInput(
                                       inputId = "cdb",
                                       label = "Sélectionner le cours d'eau",
                                       choices =c("ensemble des cours d'eau",as.character(CD$cd)),multiple = T,selected=CD$cd[1]
                                     )),
                                   br(),
                                   selectInput(
                                     inputId = "years",
                                     label=p("Sélectionner une ou deux années",br(),em("Si deux années sont sélectionnées, mettre l'année n puis l'année n+1",style="font-size:13px;text-align:justify")),
,
                                     choices =years,multiple =T,selected=2019),
                                   
                                   selectInput(
                                     inputId = "qualiteIBD",
                                     label = "Sélectionner la classe de  qualité",
                                     choices =c("ensemble  des classes de qualité","Très bonne","Bonne","Passable","Mauvaise","Très mauvaise"),selected ="ensemble  des classes de qualité",multiple = T
                                   ),
                                   
                                   # sliderInput("ipr", "IPR",
                                   #             min =round(min(data_net$ipr),3),max =round(max(data_net$ipr),3),
                                   #             value =c(2,20)),
                                   br(),
                                   uiOutput("LCAR"),
                                   br(),
                                   uiOutput("IBDmap"),
                                 ),
                                 mainPanel(width=8,
                                           uiOutput("IBD_map"),
                                           br(),
                                           br(),
                                           leafletOutput("mapy",height = 800),
                                 ),
                                 
                               ),
                    
                               ),
                      
                      tabPanel("Tableau de données",
                               br(),
                               sidebarPanel(
                                 checkboxGroupInput("show", "Sélectionner les champs à afficher dans le tableau de données :",
                                                    names(IBD_net2),selected = c("Annee","Nom de la station","Valeur IBD","Classe de qualite","Commune"))),
                               mainPanel(width=8,dataTableOutput("Table")
                                         
                               ),),
                      tabPanel("Répartition",
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(
                                     inputId = "zoniis",
                                     label = "Sélectionner la zone géographique",
                                     choices =zoni[1:6],selected = zoni[1]
                                   ),
                                   selectInput(
                                     inputId = "time",
                                     label = "Sélectionner l'année ou la période",
                                     choices =c("Année","Période"),selected ="Année"
                                   ),
                                   
                                   br(),
                                   br(),
                                   uiOutput("IBD3")
                                 ),

                                 mainPanel(width=8,
                                           uiOutput("IBD4"),
                                           br(),
                                           plotlyOutput(outputId = "repartition_IBD",height =400),
                                           
                                           br(),
                                           br(),
                                           dataTableOutput("repart")
                                 )),)
                      
                      )
           ),
    

 tabPanel("Tendances de l'indice",
          header = tagList(
                       useShinydashboard()
                     ),
                     br(),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           inputId = "zs",
                           label = "Sélectionner la zone géographique",
                           choices =zs[1:3],selected = zs[1]
                         ),
                         conditionalPanel(
                                          condition = "input.zs== 'Sous-bassin hydrographique'",
                                          selectInput(
                                            inputId = "tend_ssbv",
                                            label = "Sélectionner le Sous-bassin hydrographique.",
                                            choices =as.character(tend_ssbv$NomSsBassi),
                                          )),
                         conditionalPanel(
                                          condition = "input.zs== 'Bassin hydrographique'",
                                          selectInput(
                                            inputId = "tend_bv",
                                            label = "Sélectionner le bassin hydrographique",
                                            choices =as.character(tend_bv$BV),
                                          )),
                         br(),
                         br(),
                         uiOutput("tend_note")
                         
          ),
                     mainPanel(width=8,
                               uiOutput("titre"),
                               br(),
                               br(),
                               uiOutput("note_manquant"),
                               br(),
                               br(),
                               fluidRow(
                                 column(12,
                                        column(8, align="left",

                                               plotlyOutput("tendance_IBD",height="600px"),
                                        ),
                                        column(4,
                                               uiOutput("tauxIBD"),uiOutput("note_tx")

                                        ))),


                               br(),
                               br(),
                               dataTableOutput("tendd")


                     )),
          )                     
         )
) ,
        
tabPanel("Mentions légales",
         header = tagList(
           useShinydashboard()
         ),
         fluidPage(
           fluidRow(
             column(2,offset=5,align ="center", style="background-color:#d1dad8;border-radius: 10px",
                    strong(p("Mentions légales",style="color:black;margin: 0 0 0px;font-size:23px")),
             ),
           ),
           br(),
           
           
           fluidRow(
             
             column(12, style="",
                    p("Les publications et données disponibles sur ce site sont des informations publiques dont la réutilisation est gratuite et n'est soumise à aucune autre condition que celles de la loi du 17 juillet 1978. ",style="text-align:justify;color:black;padding-left:15px"),
             )
             
           ),
           
           
           column(6,
                  
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                           strong(p("Objet",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("Cet outil de datavisualisation a pour objet d'assurer la diffusion, sur Internet, des données de l'Indice poisson rivière et l'Indice biologique diatomées. ",style="text-align:justify;color:black;padding-left:15px"),
                           p("Tout utilisateur connecté à ce site est réputé avoir pris connaissance des mentions légales et conditions d'utilisation ci-après et les accepter sans réserve.",style="text-align:justify;color:black;padding-left:15px"),

                    )  
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                           strong(p("Éditeur",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("Ce site est sous la responsabilité du SDES. ",style="text-align:justify;color:black;padding-left:15px"),
                    ),
                  ),
                  br(),
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                           strong(p("Développement",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("L'application « IPR-IBD 2010-2020 » a été développée par",span('Loubna Chahboune,',style="font-weight:bold")," dans le cadre d’un stage de Master II au sein de la sous-direction de l’information environnementale du SDES. Cette datavisualisation a fait l'objet d'un stage encadré par", span('Alexis Cerisier-Auger,',style="font-weight:bold")," chargé de mission biodiversité (espèces) et milieux humides à la sous-direction de l'information environnementale.",style="text-align:justify;color:black;padding-left:15px"),
                           p("Cet outil a été élaboré à partir du logiciel R version 4.0.0 (2020-04-24).  Le script, accessible depuis la plateforme GitHub, est disponible sur ",a("https://github.com/loub-ch/Dataviz",href="https://github.com/loub-ch/Dataviz", target="_blank"),".",style="text-align:justify;color:black;padding-left:15px"),
                    ),
                  ),
                  br(),
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                           strong(p("Réutilisation des informations publiques",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("Les conditions de libre réutilisation fixées ci-après s'appliquent uniquement aux contenus et données diffusés par cet outil de datavisualisation et constituant des informations publiques au sens du livre III du code des relations entre le public et l'administration.",style="text-align:justify;color:black;padding-left:15px"),
                           p("Les publications et bases de données disponibles sur ce site sont des œuvres originales dont le SDES concède des droits de reproduction et de diffusion dans les conditions de la licence ouverte version 2.0 accessible sur le site :",a( "www.data.gouv.fr/fr/licences",href="https://www.data.gouv.fr/fr/licences", target="_blank"),".",style="text-align:justify;color:black;padding-left:15px"),
                           p("Pour toute demande de dérogation à ces conditions, veuillez vous adresser à" ,a("diffusion.sdes.cgdd@developpement-durable.gouv.fr",href="mailto:diffusion.sdes.cgdd@developpement-durable.gouv.fr", target="_blank"),".",style="text-align:justify;color:black;padding-left:15px"),
                           
                    ),
                  ),
                  br(),
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                           strong(p("Données personnelles",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("Ce site enregistre votre adresse IP et l'associe à des cookies enregisTrès par votre navigateur Internet. Ces données sont utilisées à des fins statistiques, pour une durée limitée et ne sont en aucun cas transmises à des tiers.",style="text-align:justify;color:black;padding-left:15px"),
                    ),
                  ),
                  br(),
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",

                           strong(p("Liens vers ce site",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("L'administration est favorable à la création de liens hypertextes vers les pages de son site. De façon générale, tout lien établi doit indiquer de façon claire à l'internaute qu'il est dirigé vers cette application, en faisant notamment mention intégrale et visible de son adresse.",style="text-align:justify;color:black;padding-left:15px"),
                           p("Les liens directs et profonds vers les fichiers (PDF, XLS) téléchargeables des publications ou données diffusés sur ce site sont susceptibles de changer à tout moment : il est suggéré de créer principalement des liens vers les pages décrivant ces publications et données et donnant accès à ces fichiers.",style="text-align:justify;color:black;padding-left:15px"),
                           p("Le SDES se réserve le droit de demander la dissolution des liens dont elle estimera qu'ils sont de nature à porter préjudice à son image ou à ses droits.",style="text-align:justify;color:black;padding-left:15px"),

                    ),
                  ),
                  br(),
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                           strong(p("Disponibilité du service",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("L'administration s'efforce d'ouvrir l'accès à ce site 24 heures sur 24, 7 jours sur 7, sauf en cas de force majeure et sous réserve d'éventuelles pannes et d'interventions de maintenance. L'administration peut être amenée à interrompre ce site ou une partie des services, à tout moment et sans préavis.",style="text-align:justify;color:black;padding-left:15px"),
                           p("La responsabilité de l'administration ne saurait être engagée en cas d'impossibilité d'accès à ce site ou d'utilisation de ces services.",style="text-align:justify;color:black;padding-left:15px"),
                           
                    ),
                  ),
                  
                  br(),
                  fluidRow(
                    column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                           strong(p("Informations contenues sur ce site",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                           p("Les informations figurant sur ce site proviennent de sources considérées comme étant fiables. Toutefois, elles sont susceptibles de contenir des erreurs que l'administration se réserve le droit de corriger dès qu'elles sont portées à sa connaissance. Les informations disponibles sur ce site sont susceptibles d'être modifiées à tout moment, et peuvent avoir fait l'objet de mises à jour depuis leur dernière consultation.",style="text-align:justify;color:black;padding-left:15px"),
                           p("Les sites internet ou informations tierces référencées sur ce site par des liens ont été sélectionnés et mis à disposition des utilisateurs à titre d'information. Malgré le soin apporté à leur sélection, ceux-ci peuvent contenir des informations inexactes ou sujettes à interprétation.",style="text-align:justify;color:black;padding-left:15px"),
                           p("L'administration ne pourra en aucun cas être tenue responsable de tout dommage de quelque nature qu'il soit résultant de l'interprétation ou de l'utilisation des informations disponibles sur ce site ou sur les sites tiers.",style="text-align:justify;color:black;padding-left:15px"),
                    ),
                  ),
                  br(),
           ),
           
           
           column(6,
                  br(),
                  fluidRow(
                    column(4, offset=2, align="right",
                           br(),
                           imageOutput("image4", height="150px")
                    ),
                    
                    column(5, offset=1,
                           
                           imageOutput("image5", height="150px")
                    ),
                    
                    
                  ),
                  tags$head(tags$style(
                    type="text/css",
                    "#image4 img {height:150px;width:200px}"
                  )),
                  
                  tags$head(tags$style(
                    type="text/css",
                    "#image5 img {height:150px;width:125px}"
                  )),

                  column(12,
                         p("Ministère de la Transition écologique",style="text-align:center;color:black;padding-left:15px;font-style:italic"),
                         p("Commissariat général au développement durable (CGDD)",style="text-align:center;color:black;padding-left:15px;font-style:italic"),
                         p("Services des données et études statistiques (SDES)",style="text-align:center;color:black;padding-left:15px;font-style:italic"),
                         br(),
                         p("Site d'Orléans - 5 route d'Olivet",style="text-align:center;color:black;padding-left:15px"),
                         p("CS 16105 – 45061 Orléans cedex 2",style="text-align:center;color:black;padding-left:15px"),
                         
                  ),
                  
                  column(12,
                         br(),br(),
                         p("Directrice de publication : Béatrice Sedillot, Cheffe du service des données et études statistiques.",style="text-align:center;color:black;padding-left:15px"),
                         p("Contact : ",a("diffusion.sdes.cgdd@developpement-durable.gouv.fr",href="mailto:diffusion.sdes.cgdd@developpement-durable.gouv.fr", target="_blank"),style="text-align:center;color:black;padding-left:15px"),
                         
                  )
           ),

         )
         
),
p()
))
source("repartition_IBD.R")
source("filter_annee.R")
source("repartition.R")
source("map.R")
source("tendance.R")
source("test.R")
source("map1.R")
source("data_IBD.R")

server <- function(input, output, session) {
  #sortie table de donnees reactive
#table de donnees initiale IPR creation des map 
  filteredData <- reactive({
    if(input$zoneselected=="France métropolitaine"){
      if(input$qualiteIPR == "ensemble  des classes de qualité"){
        
        data_net<-dplyr::filter(data_net,annee%in%input$yearselected)

      }
      
      else
        data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
      data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
    }

    else if(input$zoneselected =="Région"){

        if(input$region=="ensemble des régions"){

        if(input$qualiteIPR == "ensemble  des classes de qualité"){
          data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }

        else
        data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
        data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }
      else{
        data_net<-data_net%>%dplyr::filter(reg_libelle%in%input$region)
        if(input$qualiteIPR == "ensemble  des classes de qualité"){
          data_net
          data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }

        else
          data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
        data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
      }
    }
     else if(input$zoneselected =="Département"){

        if(input$Departement=="ensemble des départements"){
          data_net
          if(input$qualiteIPR == "ensemble  des classes de qualité"){
            data_net
            data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
          }

          else
            data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
          data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }
        else{
          data_net<-data_net%>%dplyr::filter(dep_libelle%in%input$Departement)
          if(input$qualiteIPR == "ensemble  des classes de qualité"){
            data_net
            data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
          }

          else
            data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
          data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }
      }
    else if(input$zoneselected =="Sous-bassin hydrographique"){

          if(input$Ssbv=="ensemble des sous-bassins hydrographiques"){
            data_net
            if(input$qualiteIPR == "ensemble  des classes de qualité"){
              data_net
              data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
            }
            
            else
              data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
            data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
          }
          else{
            data_net<-data_net%>%dplyr::filter(NomSsBassi%in%input$Ssbv)
            if(input$qualiteIPR == "ensemble  des classes de qualité"){
              data_net
              data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
            }

            else
              data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
            data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
          }

        }
    else if(input$zoneselected =="Bassin hydrographique"){
      if(input$Bv=="ensemble des bassins hydrographiques"){
        data_net
        if(input$qualiteIPR == "ensemble  des classes de qualité"){
          data_net
          data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }
        
        else
          data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
        data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
      }
      else {
        data_net<-dplyr::filter(data_net,BV%in%input$Bv)
        if(input$qualiteIPR == "ensemble  des classes de qualité"){
          data_net
          data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }
        
        else
        {  
        data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        data_net<-dplyr::filter(data_net,classe_ipr%in%input$qualiteIPR)
        }}
      
}
    else if(input$zoneselected =="zon"){
      if(input$cd=="ensemble des cours d'eau"){
        data_net
        if(input$qualiteIPR == "ensemble  des classes de qualité"){
          data_net
          data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
        }
        
        else
          data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
        data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
      }
      else {
          data_net<-dplyr::filter(data_net,enh_libelle_sandre%in%input$cd)
          if(input$qualiteIPR == "ensemble  des classes de qualité"){
            data_net
            data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
          }
          
          else
          {  
            data_net<-dplyr::filter(data_net,annee%in%input$yearselected)
            data_net<-dplyr::filter(data_net,type%in%input$qualiteIPR)
          }
          }
  }
      
    data_net
  })
#table tendance IPR  
  tendanceData<- reactive({
    if(input$zone == "Bassin hydrographique"){
      data_tendance<-dplyr::filter(data_tendance,BV%in%input$bv_tend)
    }
    else if(input$zone == "Sous-bassin hydrographique"){
      data_tendance<-dplyr::filter(data_tendance,NomSsBassi%in%input$ssbv_tend)
    }
    
    else {
      tend_nat
    }
    

    })
  
  #table tendance IBD 
  tendanceDatab<- reactive({
    if(input$zs== "Bassin hydrographique"){
      if(input$tend_bv=="ARTOIS-PICARDIE"){
        
      }
      else if(input$tend_bv=="RHIN-MEUSE"){
        
      }
      else {
        AG<-dplyr::filter(AG,BV%in%input$tend_bv)
        AG
      }
    }
    else if(input$zs == "Sous-bassin hydrographique"){
      t_ssbv<-dplyr::filter(t_ssbv,NomSsBassi%in%input$tend_ssbv)
      t_ssbv
    }
    
    else {
      t_nat
    }
    
    
  })
#table repartition
  repData<-reactive({
  data_net
  })
#table mapIBD par annee
  repDataIBD<-reactive({
    if(input$zn=="France métropolitaine"){
      if(input$qualiteIBD == "ensemble  des classes de qualité"){
        
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
      }
      
      else
        IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
      IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
    }

    else if(input$zn =="Région"){
      if(input$reg=="ensemble des régions"){
          if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        
      }
      
 
        else{
          IBD_net<-IBD_net%>%dplyr::filter(LbRegion%in%input$reg)
        if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
      
        }
      }
    else if(input$zn =="Département"){
      if(input$Dep=="ensemble des départements"){
        if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        
      }
      
        else{
        IBD_net<-IBD_net%>%dplyr::filter(LbDepartement%in%input$Dep)
        if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
      }
    }
    else if(input$zn =="Sous-bassin hydrographique"){
      if(input$Ssb=="ensemble des sous-bassins hydrographiques"){
        if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        
      }
      else{
        IBD_net<-IBD_net%>%dplyr::filter(NomSsBassi%in%input$Ssb)
        if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
      }
    }
    else if(input$zn =="Bassin hydrographique"){
      if(input$Bvb=="ensemble des bassins hydrographiques"){
        if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        
      }
       else{
      IBD_net<-dplyr::filter(IBD_net,BV%in%input$Bvb)
      if(input$qualiteIBD == "ensemble  des classes de qualité"){
        IBD_net
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
      }

      else
      {
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
      }
}
    }
    else if(input$zn =="zon"){
      if(input$cdb=="ensemble des cours d'eau"){
        if(input$qualiteIBD == "ensemble  des classes de qualité"){
          IBD_net
          IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        }
        
        else
          IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        
      }
     else { 
       IBD_net<-dplyr::filter(IBD_net,cd%in%input$cdb)
      
      if(input$qualiteIBD == "ensemble  des classes de qualité"){
        IBD_net
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
      }
      
      else
      {
        IBD_net<-dplyr::filter(IBD_net,annee%in%input$years)
        IBD_net<-dplyr::filter(IBD_net,type1%in%input$qualiteIBD)
      }}
      
    }
    IBD_net
    })
#table IBD creation des maps  
  fDataIBD<-reactive({
      IBD_net
    })
#table repartition IBD par periode
  perDataIBD<-reactive({
    IBD_per
  })
#table des taux evolution IPR
  tauxbvData<-reactive({
    tauxbv<-tauxbv%>%dplyr::filter(x%in%input$bv_tend)

  })
  tauxssbvData<-reactive({
    tauxssbv<-tauxssbv%>%dplyr::filter(x%in%input$ssbv_tend)
  })
  #table des taux evolution IBD
  tauxbvDatab<-reactive({
    tauxbvb<-tauxbvb%>%dplyr::filter(x%in%input$tend_bv)
    
  })
  tauxssbvDatab<-reactive({
    tauxssbvb<-tauxssbvb%>%dplyr::filter(x%in%input$tend_ssbv)
  })  
  #sortie graphique
  
  ###IPR
#MAp IPR
  output$map<-renderLeaflet({
    validate(
      need(input$yearselected!= '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$Ssbv != '','Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$region!= '','Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$Bv != '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$Departement!= '', 'Veuillez remplir tous les champs pour afficher l\'information'),
       need(input$cd != '','Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$qualiteIPR != '','Veuillez remplir tous les champs pour afficher l\'information')
      
    )
    map(filteredData(),"stations",input$yearselected[1],input$yearselected[2])

  })
#Repartition IPR
  output$repartition<-renderPlotly({
    if(input$zones==zone[1]){
      
    for (i in 1:10){
      if(input$periodselected==periode[i]){
          rep<-repartition(repData(),2009+i,2010+i)
          
          print(rep)
        }
        
      }
    ggplotly(rep)
    }
    
    else if(input$zones=="Département"){
      
        
        for (i in 1:10){
          if(input$periodselected==periode[i]){
            rep1<-repartition_dep(repData(),2009+i,2010+i)
            
            print(rep1)
          }
          
        }
        ggplotly(rep1)
      
    }
    else if(input$zones=="Région"){
      
        
        for (i in 1:10){
          if(input$periodselected==periode[i]){
            rep2<-repartition_reg(repData(),2009+i,2010+i)
            
            print(rep2)
          }
          
        }
        ggplotly(rep2)
      
    }
    else if(input$zones==zone[3]){
      
      
      for (i in 1:10){
        if(input$periodselected==periode[i]){
          rep2<-repartition_ssbv(repData(),2009+i,2010+i)
          
          print(rep2)
        }
        
      }
      ggplotly(rep2)
      
    }
    else if(input$zones=="zon"){
      
      
      for (i in 1:10){
        if(input$periodselected==periode[i]){
          rep2<-repartition_cd(repData(),2009+i,2010+i)
          
          print(rep2)
        }
        
      }
      ggplotly(rep2)
      
    }
    else if(input$zones==zone[2]){
      
      
      for (i in 1:10){
        if(input$periodselected==periode[i]){
          rep2<-repartition_bv(repData(),2009+i,2010+i)
          
          print(rep2)
        }
        
      }
      ggplotly(rep2)
      
    }
  
   })

#Tendance IPR
  output$tendance<-renderPlotly({
    if(input$zone == "Bassin hydrographique"){
      gam_bv(tendanceData())
    }
    else if(input$zone == "Sous-bassin hydrographique"){
      if (input$ssbv_tend=="Meuse"){
      }
      else if (input$ssbv_tend=="Marne"){
      }
      else if (input$ssbv_tend=="Loire moyenne"){
      }
      else if (input$ssbv_tend=="Doubs"){
      }
      else if (input$ssbv_tend=="Sambre"){
      }
      else if (input$ssbv_tend=="Durance"){
      }
      else if (input$ssbv_tend=="Côtiers Côte d'Azur"){
      }
      else if (input$ssbv_tend=="Tarn - Aveyron"){
      }
      else {
      gam_ssbv(tendanceData())
      }
    }
    else
      gam_bis(tendanceData())
    
  })

  
  ###IBD
#Tendance
  output$tendance_IBD<-renderPlotly({
    if(input$zs == "Bassin hydrographique"){
      if (input$tend_bv=="ARTOIS-PICARDIE"){
      }
      else if (input$tend_bv=="RHIN-MEUSE"){
      }
      
      else{
        gam_ibd(tendanceDatab())
      }
      
    }
    else if(input$zs == "Sous-bassin hydrographique"){
      if (input$tend_ssbv=="Sambre"){
      }
      else if (input$tend_ssbv=="Moselle - Sarre"){
      }
      else if (input$tend_ssbv=="Rhin supérieur"){
      }
      
      else {
        gam_ibd(tendanceDatab())
      }
    }
    else{
      gam_ibd(tendanceDatab())
    }
  })
#RepartitionIBD
  output$repartition_IBD<-renderPlotly({
    if(input$zoniis=="France métropolitaine"){if(input$time=="Année"){
      repartition_IBD(fDataIBD(),2010,2020)
    }
      else 
        rep_IBD(perDataIBD())
    }
    else if(input$zoniis=="Région"){if(input$time=="Année"){
      repartition_IBD_reg(fDataIBD(),2010,2020)
    }
      else 
        rep_IBD_reg(perDataIBD())
    }
    else if(input$zoniis=="Département"){
    
    if(input$time=="Annee"){
      repartition_IBD_dep(fDataIBD(),2010,2020)
    }
     else 
       rep_IBD_dep(perDataIBD())
      
    }
    else if(input$zoniis==zoni[2]){if(input$time=="Année"){
      repartition_IBD_bv(fDataIBD(),2010,2020)
    }
      else 
        rep_IBD_bv(perDataIBD())
    }
    else if(input$zoniis==zoni[3]){if(input$time=="Année"){
      repartition_IBD_ssbv(fDataIBD(),2010,2020)
    }
      else 
        rep_IBD_ssbv(perDataIBD())
    }
    else if(input$zoniis==zoni[6])
      {if(input$time=="Année"){
      repartition_IBD_cd(fDataIBD(),2010,2020)
    }
      else 
        rep_IBD_cd(perDataIBD())
    }
  })
#Map IBD
  output$mapy<-renderLeaflet({
    validate(
      need(input$years!= '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$Ssb != '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$reg!= '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$Bvb != '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$Dep!= '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$cdb != '', 'Veuillez remplir tous les champs pour afficher l\'information'),
      need(input$qualiteIBD != '','Veuillez remplir tous les champs pour afficher l\'information')
      
    )
    mapy(repDataIBD(),"stations",input$years[1],input$years[2])
    
    
  })
  
  
  #Titre et text 
  ###IPR
#titre map ipr
  output$version<-renderUI({
    
    fluidRow(
      column(12,
             h3(("Qualité des cours d'eau vis-à-vis  de l'Indice Poisson Rivière (IPR)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
             ,style="background:#466964;border-radius:12px")
    )
    
  })
  
#Titre repartition
  output$version1<-renderUI({
    if(input$zones==zone[1]){
      
      if(input$periodselected==periode[1]){
        
          fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2010,2011)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[2]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2011,2012)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[3]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2012,2013)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[4]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2013,2014)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[5]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2014,2015)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[6]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2015,2016)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[7]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2016,2017)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[8]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2017,2018)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[9]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2018,2019)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[10]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_rep(repData(),2019,2020)$`Nombre de stations`) ,"stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
    }
    else if(input$zones==zone[2]){
      
        
        if(input$periodselected==periode[1]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2010,2011)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )        }
        else if(input$periodselected==periode[2]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2011,2012)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
        }
        else if(input$periodselected==periode[3]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2012,2013)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
        }
        else if(input$periodselected==periode[4]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2013,2014)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
        }
        else if(input$periodselected==periode[5]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2014,2015)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
        }
        else if(input$periodselected==periode[6]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2015,2016)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )

        }
        else if(input$periodselected==periode[7]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2016,2017)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
          
        }
        else if(input$periodselected==periode[8]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2017,2018)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
          
        }
        else if(input$periodselected==periode[9]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2018,2019)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
        }
        else if(input$periodselected==periode[10]){
          fluidRow(
            column(12,
                   h3(paste("Répartition des",sum(data_bv(repData(),2019,2020)$`Nombre de bassins hydrographiques`),"bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                   ,style="background:#466964;border-radius:8px")
          )
          
        }
      
      
    }
    
    else if(input$zones==zone[3]){
      if(input$periodselected==periode[1]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2010,2011)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[2]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2011,2012)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[3]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2012,2013)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[4]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2013,2014)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[5]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2014,2015)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[6]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2015,2016)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[7]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2016,2017)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[8]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2017,2018)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[9]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2018,2019)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[10]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_ssbv(repData(),2019,2020)$`Nombre de sous-bassins hydrographiques`),"sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      }
    else if(input$zones==zone[4]){ 
      if(input$periodselected==periode[1]){ 
      fluidRow(
      column(12,
             h3(paste("Répartition des",sum(data_reg(repData(),2010,2011)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
             ,style="background:#466964;border-radius:8px")
    )
      
    }
      else if(input$periodselected==periode[2]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2011,2012)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[3]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2012,2013)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[4]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2013,2014)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[5]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2014,2015)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[6]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2015,2016)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[7]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2016,2017)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[8]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2017,2018)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[9]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2018,2019)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
              }
      else if(input$periodselected==periode[10]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_reg(repData(),2019,2020)$`Nombre de regions`),"régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }}
    else if(input$zones==zone[5]){
      if(input$periodselected==periode[1]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2010,2011)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[2]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2011,2012)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[3]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2012,2013)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
        
      }
      else if(input$periodselected==periode[4]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2013,2014)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[5]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2014,2015)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[6]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2015,2016)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[7]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2016,2017)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[8]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2017,2018)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[9]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2018,2018)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      else if(input$periodselected==periode[10]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_dep(repData(),2019,2020)$`Nombre de departements`),"départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }
      }
    else{
      if(input$periodselected==periode[1]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2010,2011)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )          }
      else if(input$periodselected==periode[2]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2011,2012)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )          }
      else if(input$periodselected==periode[3]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2012,2013)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )    
      }
      else if(input$periodselected==periode[4]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2013,2014)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )          }
      else if(input$periodselected==periode[5]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2014,2015)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )    
      }
      else if(input$periodselected==periode[6]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2015,2016)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[7]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2016,2017)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[8]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2017,2018)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[9]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2018,2019)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )      }
      else if(input$periodselected==periode[10]){
        fluidRow(
          column(12,
                 h3(paste("Répartition des",sum(data_cd(repData(),2019,2020)$`Nombre de cours_d_eau`),"cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        )
      }

    }
  })
#titre des tendances
  #IPR
  output$version3<-renderUI({
    
    fluidRow(
      if(input$zone == "Bassin hydrographique"){
        if(input$bv_tend=="RHIN-MEUSE"){
          column(12,
                 h3(paste("Tendance pluriannuelle de l'indice du bassin hydrographique", input$bv_tend,"entre 2013 et 2020"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
        }
        else{
      column(12,
             h3(paste("Tendance pluriannuelle de l'indice du bassin hydrographique", input$bv_tend,"entre 2010 et 2020"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
             ,style="background:#466964;border-radius:8px")
        }
      }
      else if(input$zone == "Sous-bassin hydrographique"){
        column(12,
               h3(paste("Tendance pluriannuelle de l'indice du sous-bassin hydrographique", input$ssbv_tend,"entre 2010 et 2020"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      }
      else {
        column(12,
               h3(paste("Tendance pluriannuelle de l'indice en France métropolitaine entre 2010 et 2020"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      }
    )
    
  })
  #IBD
  output$titre<-renderUI({
    
    fluidRow(
      if(input$zs == "Bassin hydrographique"){
       
    
          column(12,
                 h3(paste("Tendance pluriannuelle de l'indice du bassin hydrographique", input$tend_bv,"entre 2010 et 2019"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
                 ,style="background:#466964;border-radius:8px")
       
      }
      else if(input$zs == "Sous-bassin hydrographique"){
        column(12,
               h3(paste("Tendance pluriannuelle de l'indice du sous-bassin hydrographique", input$tend_ssbv,"entre 2010 et 2019"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      }
      else {
        column(12,
               h3(paste("Tendance pluriannuelle de l'indice en France métropolitaine entre 2010 et 2019"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      }
    )
    
  })
#note carto IPR
  output$version2<-renderUI({
    fluidRow(
      column(12,
      column(12, align ="center",style="background-color:#d1dad8;border-radius: 12px;padding-left:0px;padding-right:10px;",
             p("Un ensemble de stations de mesure permet de dresser une cartographie de l'état de l'indice au niveau du réseau hydrographique (cours d'eau de France continentale hors Corse). L'écart entre la composition du peuplement piscicole sur une station donnée (échantillonnage réalisé par pêche électrique) et la composition du peuplement attendue en situation de référence est mesuré et donne lieu à  cinq classes de qualité allant d'excellente (<7) à très mauvaise (>36). Les réseaux de mesures suivants ont été sélectionnés : Réseau National de Bassin (RNB), Contrôle opérationnel (CO), Réseau de Contrôle de Surveillance (RCS), Référence (DCE), Réseau Hydrobiologique Piscicole (RHP) et  Réseau de Référence Pérenne (RRP). Les données collectées proviennent de la base ASPE (Application de Saisie des données Piscicoles et Environnementales). Seuls les prélèvements dont les données sont jugées correctes, non définissables, indifférentes et non qualifiées  sont retenus. Lorsqu'une même station fait l'objet de plusieurs prélèvements la même année ou que deux années sont sélectionnées, la moyenne des indices est calculée et affichée sur la carte. Une information complémentaire est accessible à partir de l'infobulle (nom de la station, fiche SANDRE détaillée, valeur et classe de l'indice, cours d'eau associé, etc.). Les données issues de la sélection peuvent être téléchargées dans le module 'Table'.",style="text-align:justify;color:black;padding-left:15px"),
      )),
    )
  })
#note repartition IPR
  output$version4<-renderUI({
 
    fluidRow(
      column(12,
      column(12, align ="center",style="background-color:#d1dad8;border-radius: 12px;padding-left:0px;padding-right:10px;",

             p("Un ensemble de stations de mesure permet de dresser une cartographie de l'état de l'indice au niveau du réseau hydrographique (cours d'eau de France continentale hors Corse). L'écart entre la composition du peuplement piscicole sur une station donnée (échantillonnage réalisé par pêche électrique) et la composition du peuplement attendue en situation de référence est mesuré et donne lieu à cinq classes de qualité allant d'excellente (<7) à très mauvaise (>36). Les réseaux de mesures suivants ont été sélectionnés : Réseau National de Bassin (RNB), Contrôle opérationnel (CO), Réseau de Contrôle de Surveillance (RCS), Référence (DCE), Réseau Hydrobiologique Piscicole (RHP) et Réseau de Référence Pérenne (RRP). Les données collectées proviennent de la base la base ASPE (Application de Saisie des données Piscicoles et Environnementales).Seuls les prélèvements dont les données sont jugées correctes, non définissables, indifférentes et non qualifiées sont retenus. Lorsqu'une même station fait l'objet de plusieurs prélèvements la même année ou que deux années sont sélectionnées, la moyenne des indices est calculée et représentée. En l'absence de données suffisantes, certaines régions, bassins, départements et sous-bassins ne sont pas comptabilisés. Le nombre de stations pour chaque classe est disponible en passant le curseur sur une portion du donut. Les classes souhaitées peuvent être affichées en les sélectionnant dans la légende. Les données issues de la sélection peuvent être téléchargées dans la table sous le donut.",style="text-align:justify;color:black;padding-left:15px"),

      )),
    )
  })
#Note tendance ipr
  output$version5<-renderUI({
    
    fluidRow(
      column(12,
      column(12, align ="center",style="background-color:#d1dad8;border-radius: 12px;padding-left:0px;padding-right:10px;",

             p("L'ensemble des stations de mesure permet de dresser l'évolution pluriannuelle de l'indice au niveau du réseau hydrographique (cours d'eau de France continentale hors Corse). L'écart entre la composition du peuplement piscicole sur une station donnée (échantillonnage réalisé par pêche électrique) et la composition du peuplement attendue en situation de référence est mesuré et donne lieu à  cinq classes de qualité allant d'excellente (<7) à très mauvaise (>36). Les réseaux de mesures suivants ont été sélectionnés : Réseau National de Bassin (RNB), Contrôle opérationnel (CO), Réseau de Contrôle de Surveillance (RCS), Référence (DCE), Réseau Hydrobiologique Piscicole (RHP) et  Réseau de Référence Pérenne (RRP). Les données collectées proviennent de la base ASPE (Application de Saisie des données Piscicoles et Environnementales). Seuls les prélèvements dont les données sont jugées correctes, non définissables, indifférentes et non qualifiées  sont retenus. Lorsqu'une même station fait l'objet de plusieurs prélèvements la même année ou que deux années sont sélectionnées, la moyenne des indices est calculée et représentée. Seules les stations rencontrées plus de trois fois entre août et octobre, au cours de la période permettent de calculer la tendance. Les données disponibles sont représentées par le nuage de points (valeur de l'indice en indice base 100 - année de référence 2010). La tendance de l'indice est matérialisée par la courbe bleu, son intervalle de confiance au seuil de 95% est repésenté par l'aire grisée. Sur les 33 sous-bassins hydrographiques, les données disponibles ont permis de représenter les tendances de 24 sous-bassins hydrographiques. Plus un indice est élevé, plus il traduit une bonne qualité du milieu aquatique. L'intervalle de confiance est matérialisé par l'aire grisée. L'année et la valeur de l'indice sont affichées en passant le curseur sur la courbe. Les données issues de la sélection peuvent être téléchargées dans la table sous la courbe. Le taux d'évolution de cet indice est calculé à partir des périodes triennales 2010-2012 & 2018-2020 (excepté pour le bassin hydrographique Rhin-Meuse où l'absence de données suffisantes entre 2010 et 2012 a conduit à calculer le taux d'évolution à partir des périodes triennales 2013-2015 & 2018-2020).",style="text-align:justify;color:black;padding-left:15px"),
            
      )),
    )
  })
  #IBD
  output$tend_note<-renderUI({
    
    fluidRow(
      column(12,
             column(12, align ="center",style="background-color:#d1dad8;border-radius: 12px;padding-left:0px;padding-right:10px;",
                    
                    p("L'ensemble des stations de mesure permet de dresser l'évolution pluriannuelle de l'indice au niveau du réseau hydrographique (cours d'eau de France continentale, Corse comprise). L'écart entre les relevés sur une station donnée et la composition en situation de référence est mesuré et donne lieu à cinq classes de qualité allant de très bonne (>17) à très mauvaise (<5). Une qualité supérieure à 17 traduira une faible pollution organique, une faible eutrophisation, une salinité ou une acidification moindre du milieu. Les données collectées proviennent de la base",a("NAIADES",href="http://www.naiades.eaufrance.fr/", target="_blank") ,"(données sur la qualité des eaux de surface). Seuls les prélèvements dont les données sont jugées correctes, indifférentes, non définissable et non qualifiées sont retenus. Lorsqu'une même station fait l'objet de plusieurs prélèvements la même année ou que deux années sont sélectionnées, la moyenne des indices est calculée et représentée. Seules les stations rencontrées plus de trois fois entre mai et octobre, au cours de la période permettent de calculer la tendance. Les données disponibles sont représentées par le nuage de points (valeur de l'indice en indice base 100 - année de référence 2010). La tendance de l’indice est matérialisée par la courbe bleu, son intervalle de confiance au seuil de 95% est représentée par l’aire grisée. Sur les 33 sous-bassins hydrographiques, les données disponibles ont permis de représenter les tendances de 28 sous-bassins hydrographiques. Plus un indice est élevé, plus il traduit une bonne qualité du milieu aquatique. L'intervalle de confiance est matérialisé par l'aire grisée. L'année et la valeur de l'indice sont affichées en passant le curseur sur la courbe. Les données issues de la sélection peuvent être téléchargées dans la table sous la courbe. Le taux d'évolution de cet indice est calculé à partir des périodes triennales 2010-2012 & 2017-2019.",style="text-align:justify;color:black;padding-left:15px"),
                    
             )),
    )
  })
#land cover
  output$PdT0 <- renderUI({tagList(
    
    br(),
    #Legende CLC
    h5("Afficher la légende de la carte", style='color:black'),
    div(id="action1",actionButton("ShowLegendCLCMet", "CORINE Land Cover métropole")),
    
    
    tags$head(tags$style("#action1 .btn-default {
                                        color: black;
                                        background-color: #f5f5ff;
                                        border-color: #97989A;
                                        
                                        font-size: 12px;
                                        width:100%;
                                        font-weight: bold;
                                        
                                        border-radius: 12px;
                                        border: 2px solid #ccc;
                                        margin-bottom:2px
                                    }
                                    ")),
    
    
    
  )
    


})
  observeEvent(input$ShowLegendCLCMet, {
    showModal(modalDialog(
      title = "Légende CORINE Land Cover métropole",
      HTML('<img src="https://wxs.ign.fr/static/legends/CLC_FR.png" height="850px">'),
      easyClose = TRUE,
      footer = modalButton("Fermer la légende")
    ))
  }) 
  
  output$LCAR<- renderUI({tagList(
    
    br(),
    #Legende CLC
    h5("Afficher la légende de la carte", style='color:black'),
    div(id="action1",actionButton("ShowLegendCLCMet", "CORINE Land Cover métropole")),
    
    
    tags$head(tags$style("#action1 .btn-default {
                                        color: black;
                                        background-color: #f5f5ff;
                                        border-color: #97989A;
                                        
                                        font-size: 12px;
                                        width:100%;
                                        font-weight: bold;
                                        
                                        border-radius: 12px;
                                        border: 2px solid #ccc;
                                        margin-bottom:2px
                                    }
                                    ")),

    
    
    
  )
    
    
    
  }) 
#texte des infobox
  #IPR
  output$taux<-renderUI({
    if(input$zone == "Bassin hydrographique"){
      if(input$bv_tend=="RHIN-MEUSE"){
        fluidRow(
          
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
           br(),      
          div(infoBox(title=strong("-4.6 %",style="text-align:justify;color:red;padding-left:10px;font-size:23px"),width=12,value=p("Moyenne des taux d'évolution des 6 bassins hydrographiques entre 2013 et 2020",style="color:white;padding-left:15px;font-size:15px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
          
          div(infoBox(title=strong(paste0(format(tauxbvData()$y,nsmall=2),"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),width=12,value=p("Taux d’évolution du bassin hydrographique entre 2013 et 2020",style="color:white;padding-left:15px;font-size:15px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
          div(infoBox(title=strong(paste0(tauxbvData()$ordre,"/6"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),width=12,value=p("Classement du bassin hydrographique",style="color:white;padding-left:15px;font-size:15px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          
        ))
      }
      else{
        fluidRow(
          
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
             br(),    
          div(infoBox(title=strong("-2.6 %",style="text-align:justify;color:red;padding-left:10px;font-size:23px"),width=12,value=p("Moyenne des taux d'évolution des 6 bassins hydrographiques entre 2010 et 2020",style="color:white;padding-left:15px;font-size:15px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
          div(infoBox(title=strong(paste0(format(tauxbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),width=12,value=p("Taux d’évolution du bassin hydrographique entre 2010 et 2020",style="color:white;padding-left:15px;font-size:15px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
          div(infoBox(title=strong(paste0(tauxbvData()$ordre,"/6"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),width=12,value=p("Classement du bassin hydrographique",style="color:white;padding-left:15px;font-size:15px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          
        ))
      }
    
    }
    else if(input$zone == "Sous-bassin hydrographique"){
      if (input$ssbv_tend=="Meuse"){
      }
      else if (input$ssbv_tend=="Marne"){
      }
      else if (input$ssbv_tend=="Loire moyenne"){
      }
      else if (input$ssbv_tend=="Doubs"){
      }
      else if (input$ssbv_tend=="Sambre"){
      }
      else if (input$ssbv_tend=="Durance"){
      }
      else if (input$ssbv_tend=="Côtiers Côte d'Azur"){
      }
      else if (input$ssbv_tend=="Tarn - Aveyron"){
      }
      else if (input$ssbv_tend=="Adour"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
            br(),     
          div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
          
          div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(format(tauxssbvData()$y,nsmall=2),nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
          
          div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
        ))
      }
      else if (input$ssbv_tend=="Oise"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))      }
      else if (input$ssbv_tend=="Seine amont"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))
      }
      else if (input$ssbv_tend=="Mayenne - Sarthe - Loir"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))
      }
      else if (input$ssbv_tend=="Saône"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))      }
      else if (input$ssbv_tend=="Allier - Loire amont"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))      }
      else if (input$ssbv_tend=="Côtiers Languedoc Roussillon"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))      }
      else if (input$ssbv_tend=="Rhône moyen"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))
      }
      else if (input$ssbv_tend=="Seine aval"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))
      }
      else if (input$ssbv_tend=="Côtiers aquitains et charentais"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))}
      else if (input$ssbv_tend=="Dordogne"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))
      }
      else if (input$ssbv_tend=="Garonne"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),     
                 div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
          ))}
      
      else {
      fluidRow(
        column(12, align ="left",style="background-color:transparent;border-radius: 0px",
               br(),
               div(infoBox(value=p("Moyenne des taux d'évolution des 24 sous-bassins hydrographiques entre 2010 et 2020 ",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-2.62%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2020",style="text-align:justify;color:white;font-size:15px"),width=12,title=p(paste0(format(tauxssbvData()$y,nsmall=2),"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               
               div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=p(paste0(tauxssbvData()$ordre,"/24"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
               # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
               
        ))
      }
      # )
    }
    else {
      fluidRow(

        column(12, align ="left",style="background-color:transparent;border-radius: 0px",
        br(),
        div(infoBox(value=p("Taux d’évolution national entre 2010 et 2020",style="color:white;font-size:15px"),width=12,title=strong("-4.4 %",style="text-align:justify;color:red;font-size:23px"),,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
     
        
        div(infoBox(value=p("Taux d’évolution national intermédiaire entre 2010 et 2015",style="color:white;font-size:15px"),width=12,title=strong("-0.4 %",style="text-align:justify;color:red;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
    
        
        div(infoBox(value= p("Taux d’évolution national intermédiaire entre 2015 et 2020",style="color:white;font-size:15px"),width=12,title=strong("-3.2 %",style="text-align:justify;color:red;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               ))
               }
  })
  
  #IBD
  #texte des infobox
  output$tauxIBD<-renderUI({
    if(input$zs == "Bassin hydrographique"){
      if(input$tend_bv=="ARTOIS-PICARDIE"){
      }
      else if (input$tend_bv=="RHIN-MEUSE"){
      }
      else {
        fluidRow(
          
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),    
                 div(infoBox(title=strong("-0.87 %",style="text-align:justify;color:red;padding-left:10px;font-size:23px"),width=12,value=p("Moyenne des taux d'évolution des 4 bassins hydrographiques entre 2010 et 2019",style="color:white;padding-left:15px;font-size:15px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 div(infoBox(title=strong(paste0(tauxbvDatab()$y,"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),width=12,value=p("Taux d’évolution du bassin hydrographique entre 2010 et 2019",style="color:white;padding-left:15px;font-size:15px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 div(infoBox(title=strong(paste0(tauxbvDatab()$ordre,"/4"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),width=12,value=p("Classement du bassin hydrographique",style="color:white;padding-left:15px;font-size:15px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
                 
          ))
      }
      
    }
    else if(input$zs == "Sous-bassin hydrographique"){
      if (input$tend_ssbv=="Sambre"){
      }
      else if (input$tend_ssbv=="Moselle - Sarre"){
      }
      else if (input$tend_ssbv=="Rhin supérieur"){
      }
      
   
      else if (input$ssbv_tend=="Durance"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),
                 div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019 ",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
                 # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
                 
          ))
      }
     
      else if (input$ssbv_tend=="Tarn - Aveyron"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),
                 div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
                 # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
                 
          ))
      }


      else if (input$ssbv_tend=="Allier Loire amont"){
      fluidRow(
        column(12, align ="left",style="background-color:transparent;border-radius: 0px",
               br(),
               div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               
               div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
               # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
               
        ))  }
      else if (input$ssbv_tend=="Lot"){
      fluidRow(
        column(12, align ="left",style="background-color:transparent;border-radius: 0px",
               br(),
               div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               
               div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
               # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
               
        ))
       }
     
     
      else if (input$ssbv_tend=="Loire moyenne"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),
                 div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
                 # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
                 
          ))
      }
      else if (input$ssbv_tend=="Villaine et côtiers bretons"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),
                 div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019 ",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
                 # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
                 
          ))}
      else if (input$ssbv_tend=="Doubs"){
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),
                 div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:chartreuse;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
                 # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
                 
          ))
      }
      
      else {
        fluidRow(
          column(12, align ="left",style="background-color:transparent;border-radius: 0px",
                 br(),
                 div(infoBox(value=p("Moyenne des taux d'évolution des 28 sous-bassins hydrographiques entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong("-0.83%",style="text-align:justify;color:red;padding-left:10px;font-size:23px") ,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 div(infoBox(value=p("Taux d’évolution du sous-bassin hydrographique entre 2010 et 2019",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$y,"%"),style="text-align:justify;color:red;padding-left:10px;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
                 
                 
                 div(infoBox(value=p("Classement du sous-bassin hydrographique",style="text-align:justify;color:white;font-size:15px"),width=12,title=strong(paste0(tauxssbvDatab()$ordre,"/28"),style="text-align:justify;color:yellow;padding-left:10px;font-size:23px"),icon=icon("boxes"),color="fuchsia",fill=TRUE)),
                 # p(em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;padding-left:15px")
                 
          ))
      }
      # )
    }
    else {
      fluidRow(
        
        column(12, align ="left",style="background-color:transparent;border-radius: 0px",
               br(),
               div(infoBox(value=p("Taux d’évolution national entre 2010 et 2019",style="color:white;font-size:15px"),width=12,title=strong("-0.79%",style="text-align:justify;color:red;font-size:23px"),,icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               
               div(infoBox(value=p("Taux d’évolution national intermédiaire entre 2010 et 2015",style="color:white;font-size:15px"),width=12,title=strong("0.44%",style="text-align:justify;color:chartreuse;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
               
               
               div(infoBox(value= p("Taux d’évolution national intermédiaire entre 2015 et 2019",style="color:white;font-size:15px"),width=12,title=strong("-0.55%",style="text-align:justify;color:red;font-size:23px"),icon=icon("chart-line"),color="fuchsia",fill=TRUE)),
        ))
    }
  })
#note sous la tendance
 output$note<-renderUI({
   if(input$zone=='Sous-bassin hydrographique'){
   
   if (input$ssbv_tend=="Meuse"){
   }
   else if (input$ssbv_tend=="Marne"){
   }
   else if (input$ssbv_tend=="Loire moyenne"){
   }
   else if (input$ssbv_tend=="Doubs"){
   }
   else if (input$ssbv_tend=="Sambre"){
   }
   else if (input$ssbv_tend=="Durance"){
   }
   else if (input$ssbv_tend=="Côtiers Côte d'Azur"){
   }
   else if (input$ssbv_tend=="Tarn - Aveyron"){
   }
   else {
     fluidRow(
       column(12, align ="left",style="background-color:transparent;border-radius: 0px",
              p(span("Note :",style="font-weight:bold;"),em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité. Le classement permet de comparer les sous-bassins hydrographiques entre eux, plus un sous-bassin hydrographique aura un taux d'évolution négative, plus ce dernier se positionnera en bas du classement."),style="text-align:justify;color:black;font-size:17px;padding:14px;margin:0px")
       ))
   }
 }
   else if(input$zone=='Bassin hydrographique') {
     fluidRow(
       column(12, align ="left",style="background-color:transparent;border-radius: 0px",
              p(span("Note :",style="font-weight:bold;"),em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité. Le classement permet de comparer les bassins hydrographiques entre eux, plus un bassin hydrographique aura un taux d'évolution négative, plus ce dernier se positionnera en bas du classement."),style="text-align:justify;color:black;font-size:17px;padding:14px;margin:0px")
       ))
   }
   else {
     fluidRow(
     column(12, align ="left",style="background-color:transparent;border-radius: 0px",
   p(span("Note :",style="font-weight:bold;"),em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;font-size:17px;padding:14px;margin:0px")
     ))
   }
 }) 
 #Ibd
 output$note_tx<-renderUI({
   if(input$zs=='Sous-bassin hydrographique')
     {
   if (input$tend_ssbv=="Sambre"){
   }
   else if (input$tend_ssbv=="Rhin supérieur"){
   }
   else if (input$tend_ssbv=="Moselle - Sarre"){
   }
   else {
     fluidRow(
       column(12, align ="left",style="background-color:transparent;border-radius: 0px",
              p(span("Note :",style="font-weight:bold;"),em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité. Le classement permet de comparer les sous-bassins hydrographiques entre eux, plus un sous-bassin hydrographique aura un taux d'évolution négative, plus ce dernier se positionnera en bas du classement."),style="text-align:justify;color:black;font-size:17px;padding:14px;margin:0px")
       ))
   }}
   else if(input$zs=='Bassin hydrographique') {
     if (input$tend_bv=="ARTOIS-PICARDIE"){
     }
     else if (input$tend_bv=="RHIN-MEUSE"){
     }
     else
       {
     fluidRow(
       column(12, align ="left",style="background-color:transparent;border-radius: 0px",
              p(span("Note :",style="font-weight:bold;"),em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité. Le classement permet de comparer les bassins hydrographiques entre eux, plus un bassin hydrographique aura un taux d'évolution négative, plus ce dernier se positionnera en bas du classement."),style="text-align:justify;color:black;font-size:17px;padding:14px;margin:0px")
       ))}
   }
   else {
     fluidRow(
       column(12, align ="left",style="background-color:transparent;border-radius: 0px",
              p(span("Note :",style="font-weight:bold;"),em("Un taux d'évolution positif renseigne sur une amélioration de la qualité de l'eau, à l'inverse, un taux négatif exprimera une dégradation de cette qualité."),style="text-align:justify;color:black;font-size:17px;padding:14px;margin:0px")
       ))
   }
 }) 
 
#note note des manquants
 output$note1<-renderUI({
   if(input$zone == "Sous-bassin hydrographique")
     {
   if (input$ssbv_tend=="Meuse"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red") 
   }
   else if (input$ssbv_tend=="Marne"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red") 
   }
   else if (input$ssbv_tend=="Loire moyenne"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red")    
   }
   else if (input$ssbv_tend=="Doubs"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red")    
   }
   else if (input$ssbv_tend=="Sambre"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red")   
   }
   else if (input$ssbv_tend=="Durance"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red")    
   }
   else if (input$ssbv_tend=="Côtiers Côte d'Azur"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red")    
   }
   else if (input$ssbv_tend=="Tarn - Aveyron"){
     p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red")
   }
   else{

   }
     }
   else{
     
   }
 })
 
 #IBD
 output$note_manquant<-renderUI({
   if(input$zs == "Sous-bassin hydrographique")
   {
     if (input$tend_ssbv=="Sambre"){
       p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red") 
     }
     else if (input$tend_ssbv=="Moselle - Sarre"){
       p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red") 
     }
     else if (input$tend_ssbv=="Rhin supérieur"){
       p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red")    
     }

     else{
       
     }
   }
   else if(input$zs == "Bassin hydrographique"){
     if (input$tend_bv=="ARTOIS-PICARDIE"){
       p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red") 
     }
     else if (input$tend_bv=="RHIN-MEUSE"){
       p(strong("L’absence de données sur la période des 10 ans ne permet pas de dresser la tendance pour l’échelon sélectionné."),style="text-align:center;color:red") 
     }
     
   }
   else {
     
   }
 })
  ###IBD
#Note map IBD 
 output$IBDmap<-renderUI({
    
    fluidRow(
      column(12,
      column(12,aligne="center",style="background-color:#d1dad8;border-radius: 12px;padding-left:0px;padding-right:10px;",
             
             p("Un ensemble de stations de mesure permet de dresser une cartographie de l'état de l'indice au niveau du réseau hydrographique (cours d'eau de France continentale, Corse comprise). L'écart entre les relevés sur une station donnée et la composition en situation de référence est mesuré et donne lieu à cinq classes de qualité allant de très bonne (>17) à très mauvaise (<5). Une qualité supérieure à 17 traduira une faible pollution organique, une faible eutrophisation, une salinité ou une acidification moindre du milieu. Les données collectées proviennent de la base",a("NAIADES",href="http://www.naiades.eaufrance.fr/", target="_blank") ,"(données sur la qualité des eaux de surface). Seuls les prélèvements dont les données sont jugées correctes, non définissables, indifférentes et non qualifiées  sont retenus. Lorsqu'une même station fait l'objet de plusieurs prélèvements la même année ou que deux années sont sélectionnées, la moyenne des indices est calculée et affichée sur la carte. Une information complémentaire est accessible à partir de l'infobulle (nom de la station, fiche SANDRE détaillée, valeur et classe de l'indice, cours d'eau associé, etc.). Les données issues de la sélection peuvent être téléchargées dans le module 'Table'.",style="text-align:justify;color:black;padding-left:15px"),
             
             
      )),
    )
  })
  
#Titre mapIBD
  output$IBD_map<-renderUI({
    fluidRow(
      column(12,
             h3(("Qualité des cours d'eau vis-à-vis  de l'Indice Biologique Diatomées (IBD)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
             ,style="background:#466964;border-radius:12px")
    )
  })
#note repartition IBD
  output$IBD3<-renderUI({
    
    fluidRow(
      column(12,
      column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px",
             
             
        
             
             p("Un ensemble de stations de mesure permet de dresser une cartographie de l'état de l'indice au niveau du réseau hydrographique (cours d'eau de France continentale, Corse comprise). L'écart entre les relevés sur une station donnée et la composition en situation de référence est mesuré et donne lieu à cinq classes de qualité allant de très bonne (>17) à très mauvaise (<5). Une qualité supérieure à 17 traduira une faible pollution organique, une faible eutrophisation, une salinité ou une acidification moindre du milieu. Les données collectées proviennent de la base",a("NAIADES",href="http://www.naiades.eaufrance.fr/", target="_blank") ,"(données sur la qualité des eaux de surface), Seuls les prélèvements dont les données sont jugées correctes, non définissables, indifférentes et non qualifiées  sont retenus. Lorsqu'une même station fait l'objet de plusieurs prélèvements la même année ou que deux années sont sélectionnées, la moyenne des indices est calculée et représentée. En l'absence de données suffisantes, certaines régions, bassins, départements et sous-bassins ne sont pas comptabilisés. Le nombre de stations pour chaque classe est disponible en passant le curseur sur le diagramme. Un affichage par année ou par période est possible. Les données issues de la sélection peuvent être téléchargées dans la table sous le diagramme.",style="text-align:justify;color:black;padding-left:15px"),
             
             
           
      )),
    )
  })
#titre repartition IBD
  output$IBD4<-renderUI({
    if(input$zoniis=="France métropolitaine"){
      fluidRow(
        column(12,
               h3(paste("Répartition des stations selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      )
    }
    else if(input$zoniis=="Région"){
      fluidRow(
        column(12,
               h3(paste("Répartition des régions selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      )
    }
    else if(input$zoniis=="Département"){
      fluidRow(
        column(12,
               h3(paste("Répartition des départements selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      )
    }
    else if(input$zoniis=="Bassin hydrographique"){
      fluidRow(
        column(12,
               h3(paste("Répartition des bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      )
    }
    else if(input$zoniis=="Sous-bassin hydrographique"){
      fluidRow(
        column(12,
               h3(paste("Répartition des sous-bassins hydrographiques selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      )
    }
    else if(input$zoniis==zoni[6]){
      fluidRow(
        column(12,
               h3(paste("Répartition des cours d'eau selon la classe de qualité de l'indice (en %)"),align ="center",style="font-size:20px;color:#f5f5ff;margin-top: 8px;")
               ,style="background:#466964;border-radius:8px")
      )
    }
  })

  #Table affichee dans l'application
  
  ###IPR
  output$v<-renderDataTable(DT::datatable({

    ab<-filteredData()%>%select('Annee'=annee,'Point prelevement'=pop_id,'Nom de la station'=sta_libelle_sandre,'Code SANDRE de la station'=sta_code_sandre,'Fiche detaillee de la station'=url_site,
                                'Valeur IPR'=ipr2,'Classe de qualite'=type,'Date de prelevement'=ope_date,'Sous-Bassin'=NomSsBassi,
                                'Region'=reg_libelle,'Departement'=dep_libelle,'Code Insee'=dep_code_insee,
                                'Commune'=com_libelle,"Cours d'eau"=enh_libelle_sandre,"Code SANDRE du cours d'eau"=enh_code_sandre)
    ab[,input$show_vars, drop = FALSE]
   
  },escape = FALSE,extensions = 'Buttons',
  options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                   "}"),
                 columnDefs=list(list(className='dt-center',width='200px',targets="_all"))
  ),rownames= FALSE
  ))
  
  output$r<-renderDataTable(DT::datatable({
    if(input$zones==zone[1]){
      
    if(input$periodselected==periode[1]){
    data_rep(repData(),2010,2011)
    }
    else if(input$periodselected==periode[2]){
      data_rep(repData(),2011,2012)
    }
    else if(input$periodselected==periode[3]){
      data_rep(repData(),2012,2013)
    }
    else if(input$periodselected==periode[4]){
      data_rep(repData(),2013,2014)
    }
    else if(input$periodselected==periode[5]){
      data_rep(repData(),2014,2015)
    }
    else if(input$periodselected==periode[6]){
      data_rep(repData(),2015,2016)
    }
    else if(input$periodselected==periode[7]){
      data_rep(repData(),2016,2017)
    }
      else if(input$periodselected==periode[8]){
        data_rep(repData(),2017,2018)
      }
      else if(input$periodselected==periode[9]){
        data_rep(repData(),2018,2019)
      }
      else if(input$periodselected==periode[10]){
        data_rep(repData(),2019,2020)
      }
    }
    else if(input$zones==zone[2]){
      
      if(input$periodselected==periode[1]){
        data_bv(repData(),2010,2011)
      }
      else if(input$periodselected==periode[2]){
        data_bv(repData(),2011,2012)
      }
      else if(input$periodselected==periode[3]){
        data_bv(repData(),2012,2013)
      }
      else if(input$periodselected==periode[4]){
        data_bv(repData(),2013,2014)
      }
      else if(input$periodselected==periode[5]){
        data_bv(repData(),2014,2015)
      }
      else if(input$periodselected==periode[6]){
        data_bv(repData(),2015,2016)
      }
      else if(input$periodselected==periode[7]){
        data_bv(repData(),2016,2017)
      }
      else if(input$periodselected==periode[8]){
        data_bv(repData(),2017,2018)
      }
      else if(input$periodselected==periode[9]){
        data_bv(repData(),2018,2019)
      }
      else if(input$periodselected==periode[10]){
        data_bv(repData(),2019,2020)
      }
    }
    else if(input$zones==zone[3]){
      
      if(input$periodselected==periode[1]){
        data_ssbv(repData(),2010,2011)
      }
      else if(input$periodselected==periode[2]){
        data_ssbv(repData(),2011,2012)
      }
      else if(input$periodselected==periode[3]){
        data_ssbv(repData(),2012,2013)
      }
      else if(input$periodselected==periode[4]){
        data_ssbv(repData(),2013,2014)
      }
      else if(input$periodselected==periode[5]){
        data_ssbv(repData(),2014,2015)
      }
      else if(input$periodselected==periode[6]){
        data_ssbv(repData(),2015,2016)
      }
      else if(input$periodselected==periode[7]){
        data_ssbv(repData(),2016,2017)
      }
      else if(input$periodselected==periode[8]){
        data_ssbv(repData(),2017,2018)
      }
      else if(input$periodselected==periode[9]){
        data_ssbv(repData(),2018,2019)
      }
      else if(input$periodselected==periode[10]){
        data_ssbv(repData(),2019,2020)
      }
    }
    else if(input$zones==zone[6]){
      
      if(input$periodselected==periode[1]){
        data_cd(repData(),2010,2011)
      }
      else if(input$periodselected==periode[2]){
        data_cd(repData(),2011,2012)
      }
      else if(input$periodselected==periode[3]){
        data_cd(repData(),2012,2013)
      }
      else if(input$periodselected==periode[4]){
        data_cd(repData(),2013,2014)
      }
      else if(input$periodselected==periode[5]){
        data_cd(repData(),2014,2015)
      }
      else if(input$periodselected==periode[6]){
        data_cd(repData(),2015,2016)
      }
      else if(input$periodselected==periode[7]){
        data_cd(repData(),2016,2017)
      }
      else if(input$periodselected==periode[8]){
        data_cd(repData(),2017,2018)
      }
      else if(input$periodselected==periode[9]){
        data_cd(repData(),2018,2019)
      }
      else if(input$periodselected==periode[10]){
        data_cd(repData(),2019,2020)
      }
    }
    else if(input$zones==zone[4]){
      
      if(input$periodselected==periode[1]){
        data_reg(repData(),2010,2011)
      }
      else if(input$periodselected==periode[2]){
        data_reg(repData(),2011,2012)
      }
      else if(input$periodselected==periode[3]){
        data_reg(repData(),2012,2013)
      }
      else if(input$periodselected==periode[4]){
        data_reg(repData(),2013,2014)
      }
      else if(input$periodselected==periode[5]){
        data_reg(repData(),2014,2015)
      }
      else if(input$periodselected==periode[6]){
        data_reg(repData(),2015,2016)
      }
      else if(input$periodselected==periode[7]){
        data_reg(repData(),2016,2017)
      }
      else if(input$periodselected==periode[8]){
        data_reg(repData(),2017,2018)
      }
      else if(input$periodselected==periode[9]){
        data_reg(repData(),2018,2019)
      }
      else if(input$periodselected==periode[10]){
        data_reg(repData(),2019,2020)
      }
    }
    else if(input$zones==zone[5]){
      
      if(input$periodselected==periode[1]){
        data_dep(repData(),2010,2011)
      }
      else if(input$periodselected==periode[2]){
        data_dep(repData(),2011,2012)
      }
      else if(input$periodselected==periode[3]){
        data_dep(repData(),2012,2013)
      }
      else if(input$periodselected==periode[4]){
        data_dep(repData(),2013,2014)
      }
      else if(input$periodselected==periode[5]){
        data_dep(repData(),2014,2015)
      }
      else if(input$periodselected==periode[6]){
        data_dep(repData(),2015,2016)
      }
      else if(input$periodselected==periode[7]){
        data_dep(repData(),2016,2017)
      }
      else if(input$periodselected==periode[8]){
        data_dep(repData(),2017,2018)
      }
      else if(input$periodselected==periode[9]){
        data_dep(repData(),2018,2019)
      }
      else if(input$periodselected==periode[10]){
        data_dep(repData(),2019,2020)
      }
    }
  },extensions = 'Buttons',
  options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                   "}"),
                 columnDefs=list(list(className='dt-center',targets="_all"))
  ),rownames= FALSE))
  output$a<-renderDataTable(DT::datatable({
    if(input$zone == "Bassin hydrographique"){
      data_gam(tendanceData())
      
    }
    else if(input$zone == "Sous-bassin hydrographique"){
      if (input$ssbv_tend=="Meuse"){
      }
      else if (input$ssbv_tend=="Marne"){
      }
      else if (input$ssbv_tend=="Loire moyenne"){
      }
      else if (input$ssbv_tend=="Doubs"){
      }
      else if (input$ssbv_tend=="Sambre"){
      }
      else if (input$ssbv_tend=="Durance"){
      }
      else if (input$ssbv_tend=="Côtiers Côte d'Azur"){
      }
      else if (input$ssbv_tend=="Tarn - Aveyron"){
      }
      else {
      data_gam(tendanceData())}
      
    }
    
    else { 
      tendanceData()%>%mutate('Annee'=annee,'Valeurs de l\'IPR'=format(round(ind100,2),nsmall=2),'Borne minimale de l\'intervalle de confiance'=format(round(yminus,2),nsmall=2), 'Borne maximale de l\'intervalle de confiance'=format(round(yplus,2),nsmall=2))%>%
        select(Annee:'Borne maximale de l\'intervalle de confiance')
 
      
    }
    
  },extensions = 'Buttons',
  options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                   "}"),
                 columnDefs=list(list(className='dt-center',targets="_all"))
  ),rownames= FALSE)) 
  
  #IBD
  output$tendd<-renderDataTable(DT::datatable({
    if (input$tend_bv=="ARTOIS-PICARDIE"){
    }
    else if (input$tend_bv=="RHIN-MEUSE"){
    }
    else if (input$tend_ssbv=="Sambre"){
    }
    else if (input$tend_ssbv=="Moselle - Sarre"){
    }
    else if (input$tend_ssbv=="Rhin supérieur"){
    }
    else{
    tendanceDatab()%>%mutate('Annee'=annee,'Valeurs de l\'IBD'=format(round(ind100,2),nsmall=2),'Borne minimale de l\'intervalle de confiance'=format(round(yminus,2),nsmall=2), 'Borne maximale de l\'intervalle de confiance'=format(round(yplus,2),nsmall=2))%>%
      select(Annee:'Borne maximale de l\'intervalle de confiance')}
  },extensions = 'Buttons',
  options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                   "}"),
                 columnDefs=list(list(className='dt-center',targets="_all"))
  ),rownames= FALSE)) 
    
  output$Table<-renderDataTable(DT::datatable({
    
    ab<-repDataIBD()%>%mutate('Valeur IBD'=format(IBD,nsmall=2))%>%select('Nom de la station'=LbStationMesureEauxSurface,'Annee'=annee,'Code SANDRE de la station'=CODE_SANDRE,
                            'Valeur IBD','Classe de qualite'=type1,'Date de prelevement'=DATE_DEBUT,'Sous-Bassin'=NomSsBassi,
                            'Region'=LbRegion,'Departement'=LbDepartement,
                            'Commune'=LbCommune)
    ab[,input$show, drop = FALSE]
  },escape = FALSE,extensions = 'Buttons',
  options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                   "}"),
                 columnDefs=list(list(className='dt-center',width='200px',targets="_all"))
  ),rownames= FALSE))
  
  output$repart<-renderDataTable(DT::datatable({

    if(input$zoniis==zoni[1]){

      if(input$time=="Année"){
        data_IBD(fDataIBD(),2010,2020)
      }
      else {
        data_per_IBD(perDataIBD())
      }
    }
  
    else if(input$zoniis==zoni[2]){
      
      if(input$time=="Année"){
        data_IBD_bv(fDataIBD(),2010,2020)
      }
      else {
        data_per_IBD_bv(perDataIBD())
      }
      
    }
    else if(input$zoniis==zoni[3]){

      if(input$time=="Année"){
        data_IBD_ssbv(fDataIBD(),2010,2020)
      }
      else {
        data_per_IBD_ssbv(perDataIBD())
      }

    }
    else if(input$zoniis==zoni[4]){

      if(input$time=="Année"){
        data_IBD_reg(fDataIBD(),2010,2020)
      }
      else {
        data_per_IBD_reg(perDataIBD())
      }

    }
    else if(input$zoniis==zoni[5]){

      if(input$time=="Année"){
        data_IBD_dep(fDataIBD(),2010,2020)
      }
      else {
        data_per_IBD_dep(perDataIBD())
      }

    }
    else if(input$zoniis==zoni[6]){

      if(input$time=="Année"){
        IBD_cd(fDataIBD(),2010,2020)
      }
      else {
        data_per_IBD_cd(perDataIBD())
      }

    }
    
  },extensions = 'Buttons',
  options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                   "}"),
                 columnDefs=list(list(className='dt-center',targets="_all"))
  ),rownames= FALSE))

  ###IBD
 
  #Image 
  output$image2 <- renderImage({

    list(src = "image2.png")
  }, deleteFile = FALSE)
  output$image3 <- renderImage({
    
    list(src = "bandeau.png",align = "center")
  }, deleteFile = FALSE)
  output$image4 <- renderImage({
    
    list(src = "image2.png")
  }, deleteFile = FALSE)
  output$image5 <- renderImage({
    
    list(src = "StatPublique.png")
  }, deleteFile = FALSE)

}
shinyApp(ui, server)