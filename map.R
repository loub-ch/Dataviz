
map<-function(df,g,pa,da){
  dt1<-mef_filtrer_sta_annee(df, premiere_annee =pa,derniere_annee =da)
  dt<-sqldf('select distinct(pop_id),sta_id,avg(ipr) as ipr from dt1 group by pop_id')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >="7" and ipr <"16" then "Bon"
                when ipr >="16" and  ipr < "25" then "Moyen"
                when ipr >="25" and ipr<"36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM dt')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ipr,count( classe_ipr) as nbre_class  FROM
                  dt group by classe_ipr ')
  
  #Creation de %
  dt<- dt %>%
    arrange(desc(classe_ipr)) %>%
    mutate(nbre_class_pr = (round(prop.table(nbre_class)*100)))
  dt<-dt %>%
    arrange(desc(classe_ipr)) %>%
    mutate(lab.ypos = cumsum(nbre_class_pr) - 0.5*nbre_class_pr)
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ipr
WHEN "Tres bon" THEN 1
WHEN "Bon" THEN 2
WHEN "Moyen" THEN 3
WHEN "Mediocre" THEN 4
ELSE 5 end ')
  n<-sqldf('select count(distinct(pop_id)) from dt1')
  dt<-sqldf('SELECT *,case when classe_ipr=="Tres bon" then "blue"
                 when classe_ipr=="Bon" then "darkgreen"
                when classe_ipr=="Moyen" then "yellow"
                when classe_ipr=="Mediocre" then "orange"
                else "red"
                end as couleur FROM dt')
  dt1<-sqldf('SELECT *,case when classe_ipr=="Tres bon" then "blue"
                 when classe_ipr=="Bon" then "darkgreen"
                when classe_ipr=="Moyen" then "yellow"
                when classe_ipr=="Mediocre" then "orange"
                else "red"
                end as couleur FROM dt1')

  dt1<-dt1%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  dt<-dt%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  dm<-dt1 %>%
    sf::st_as_sf(coords = c("X_WGS84", "Y_WGS84"), crs = 4326)
  dm$ipr<-round(dm$ipr,2)
  dm$url<-paste0('<a href=',"https://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:StationMesureEauxSurface:FRA:code:",dm$sta_code_sandre,':::::html target="_blank"> cliquez ici</a>')
  # url2<-"https://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:StationMesureEauxSurface:FRA:code:"
  # dm <- dm %>%
  #   mutate(url= paste0('<a href=',
  #                        url2,
  #                        sta_code_sandre,
  #                        ':::::html ,target="_blank"> cliquez ici</a>'))
           
  dm$Departement<-paste0(dm$dep_libelle," ","(",dm$dep_code_insee,")")
  dm<-dm%>%select('Nom de la station'=sta_libelle_sandre,'Code SANDRE de la station'=sta_code_sandre,"Fiche detaillee de la station"=url,
                  'Valeur IPR'=ipr,'Classe de qualite'=type,'Date de prelevement'=ope_date,'Sous-Bassin'=NomSsBassi,
                  'Region'=reg_libelle,Departement,
                  'Commune'=com_libelle,"Cours d'eau"=enh_libelle_sandre,"Code SANDRE du cours d'eau"=enh_code_sandre)
  mapviewOptions(fgb = F)
  ma<-mapview(dm,zcol ="Classe de qualite",col.regions=dt1$couleur,burst=F,label=F
  ,popup = paste("<div style='width:400px'><b><h5>",
    strong("Nom de la station :"), dt1$sta_libelle_sandre,"<br>",
    strong("Code SANDRE de la station : "), dt1$sta_code_sandre, "<br>",
    
    strong("Fiche d\U00E9taill\U00E9\U0065 de la station :"),paste0('<a href=',"https://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:StationMesureEauxSurface:FRA:code:",dt1$sta_code_sandre,':::::html target="_blank">Acc\U00E9\U0064\U0065z \U00E0 la fiche</a>'), "<br>",
    
    
    strong( "Valeur IPR :       "), format(round(dt1$ipr,2),nsmall=2), "<br>",
    
    strong("Classe de qualit\U00E9 : "), dt1$type, "<br>",
    
    strong("Date de pr\U00E9l\U00E8vement : "), dt1$ope_date, "<br>",
    
    strong("Sous-bassin : "), dt1$NomSsBassi, "<br>",
    
    strong("R\U00E9gion : "), dt1$reg_libelle, "<br>",
    
    strong("D\U00E9partement : "), dm$Departement, "<br>",
    
    strong("Commune : "), dt1$com_libelle, "<br>",
    
    
    strong("Cours d'eau : "), dt1$enh_libelle_sandre, "<br>",
    
    strong("Code SANDRE du cours d'eau : "), dt1$enh_code_sandre, "</div>"
    
  )%>%lapply(htmltools::HTML),homebutton = FALSE,cex =5, alpha.regions =1,legend=F)
  ma@map%>%addLegend(colors=dt$couleur,labels=c(paste0(dt$type," ","(",(dt$nbre_class_pr)," % "," soit ",dt$nbre_class," ",g,")")),opacity = 1,title =paste("Classes de qualit\U00E9 des ",n," stations"),
                     
                     position = "bottomright")%>%
    addTiles("http://wxs.ign.fr/vp2cxyan0t7ufwrfbyl3eg7s/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=CORINE%20Land%20Cover&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=LANDCOVER.CLC18&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
             options = WMSTileOptions(tileSize = 256),
             group = "CORINE Land Cover 2018")  %>%
    addProviderTiles("OpenTopoMap", group = "Carte topographique") %>% 
    addProviderTiles("Esri.WorldImagery", group = "Orthophotographie")%>%
    addProviderTiles("CartoDB.Positron", group = "General")%>%
    addProviderTiles("OpenStreetMap", group = "SCAN")%>%

    addLayersControl(baseGroups = c("G\U00E9n\U00E9ral","Carte topographique", "Orthophotographie","SCAN","CORINE Land Cover 2018"),options = layersControlOptions(collapsed = FALSE))%>%addSearchOSM()
  }
map(data_net,"stations",2010,2011)

# data_net$data<-"www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:StationMesureEauxSurface:FRA:code:data_net$sta_code_sandre:::::html"


# label=paste(
#   strong("Nom de la station :"), dt1$sta_libelle_sandre, "<br>",
# 
#   strong("Code SANDRE de la station : "), dt1$sta_code_sandre, "<br>",
# 
#   strong("Fiche d\U00E9taill\U00E9\U0065 de la station :"),paste0('<a href=',"https://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:StationMesureEauxSurface:FRA:code:",dm$sta_code_sandre,':::::html target="_blank"> cliquez ici</a>'), "<br>",
# 
# 
#   strong( "Valeur IPR :       "), round(dt1$ipr,2), "<br>",
# 
#   strong("Classe de qualit\U00E9 : "), dt1$classe_ipr, "<br>",
# 
#   strong("Date de pr\U00E9l\U00E8vement : "), dt1$ope_date, "<br>",
# 
#   strong("Sous-Bassin : "), dt1$NomSsBassi, "<br>",
# 
#   strong("R\U00E9gion : "), dt1$reg_libelle, "<br>",
# 
#   strong("D\U00E9partement : "), dm$Departement, "<br>",
# 
#   strong("Commune : "), dt1$com_libelle, "<br>",
# 
# 
#   strong("Cours d'eau : "), dt1$enh_libelle_sandre, "<br>",
# 
#   strong("Code SANDRE du cours d'eau : "), dt1$enh_code_sandre
# 
# )%>%lapply(htmltools::HTML)
