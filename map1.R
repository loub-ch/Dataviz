mapy<-function(df,g,pa,da){
  dt2<-filter_annee(df, premiere_annee =pa,derniere_annee =da)
  dt1<-sqldf('select distinct (CODE_SANDRE),DATE_DEBUT,annee,avg(IBD) as IBD from dt2 group by DATE_DEBUT,CODE_SANDRE')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when IBD >="17" then "Tres bonne"
                 when IBD  >="13" and IBD <"17" then "Bonne"
                when IBD >="9" and  IBD <"13" then "Passable"
                when IBD >="5" and IBD<"9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,CODE_SANDRE,annee,count(classe_ibd) as nbre_class  FROM 
                  dt group by classe_ibd')
  
  #Creation de % 
  
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  
  
  n<-sqldf('select count(distinct(CODE_SANDRE)) from dt1')
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ibd
WHEN "Tres bonne" THEN 1
WHEN "Bonne" THEN 2
WHEN "Passable" THEN 3
WHEN "Mauvaise" THEN 4
ELSE 5 end ')
  dt2<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt2')
  dt2<-dt2%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dm<-dt2%>%sf::st_as_sf(coords = c("POINT_X","POINT_Y"), crs = 4326)
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  
  dm$IBD<-round(dm$IBD,2)
  
  dm$Departement<-paste0(dm$LbDepartement," ","(",dm$CodeDepartement,")")
  dm<-dm%>%mutate('Valeur IBD'=format(IBD,nsmall=2))%>%select('Nom de la station'=LbStationMesureEauxSurface,'Code SANDRE de la station'=CODE_SANDRE,"Fiche detaillee de la station"=lien,
                  'Valeur IBD','Classe de qualite'=type1,'Date de prelevement'=DATE_DEBUT,'Sous-Bassin'=NomSsBassi,
                  'Region'=LbRegion,Departement,
                  'Commune'=LbCommune,"Cours d'eau"=cd)
  mapviewOptions(fgb = F)
  ma<-mapview(dm,zcol ="Classe de qualite",label=F,col.regions=dt2$couleur,burst=F,popup =  paste( "<div style='width:400px'><b><h5>",
                                                                                                   strong("Nom de la station :"), dt2$LbStationMesureEauxSurface,"<br>",
                                                                                                   strong("Code SANDRE de la station : "), dt2$CODE_SANDRE, "<br>",
                                                                                                   
                                                                                                   strong("Fiche d\U00E9taill\U00E9\U0065 de la station :"),paste0('<a href=',"https://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:StationMesureEauxSurface:FRA:code:",dt2$CODE_SANDRE,':::::html target="_blank">Acc\U00E9\U0064\U0065z \U00E0 la fiche</a>'), "<br>",
                                                                                                   
                                                                                                   
                                                                                                   strong( "Valeur IBD :       "), format(round(dt2$IBD,2),nsmall=2), "<br>",
                                                                                                   
                                                                                                   strong("Classe de qualit\U00E9 : "), dt2$type1, "<br>",
                                                                                                   
                                                                                                   strong("Date de pr\U00E9l\U00E8vement : "), dt2$DATE_DEBUT, "<br>",
                                                                                                   
                                                                                                   strong("Sous-bassin : "), dt2$NomSsBassi, "<br>",
                                                                                                   
                                                                                                   strong("R\U00E9gion : "), dt2$LbRegion, "<br>",
                                                                                                   
                                                                                                   strong("D\U00E9partement : "), dm$Departement, "<br>",
                                                                                                   
                                                                                                   strong("Commune : "), dt2$LbCommune, "<br>",
                                                                                                   
                                                                                                   
                                                                                                   strong("Cours d'eau : "), dt2$cd,  "</div>"
                                                                                                   
  )%>%lapply(htmltools::HTML),homebutton = FALSE,cex =5, alpha.regions =1,legend=F)
  ma@map%>%addLegend(colors=dt$couleur,labels=c(paste0(dt$type1," ","(",(dt$nbre_class_pr)," % "," soit ",dt$nbre_class," ",g,")")),opacity = 1,title =paste("Classes de qualit\U00E9 des ",n," stations"),
                     
                     position = "bottomright")%>%addTiles("http://wxs.ign.fr/vp2cxyan0t7ufwrfbyl3eg7s/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=CORINE%20Land%20Cover&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=LANDCOVER.CLC18&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
                                                          options = WMSTileOptions(tileSize = 256),
                                                          group = "CORINE Land Cover 2018")  %>%
    addProviderTiles("OpenTopoMap", group = "Carte topographique") %>% 
    addProviderTiles("Esri.WorldImagery", group = "Orthophotographie")%>%
    addProviderTiles("CartoDB.Positron", group = "General")%>%
    addProviderTiles("OpenStreetMap", group = "SCAN")%>%
    
    addLayersControl(baseGroups = c("G\U00E9n\U00E9ral","Carte topographique", "Orthophotographie","SCAN","CORINE Land Cover 2018"),options = layersControlOptions(collapsed = FALSE))%>%addSearchOSM()
  
}
# mapy(IBD_net,"stations",2010,2011)
