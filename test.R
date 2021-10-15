source("mef_filtrer_sta_annee.R")
data_rep<-function(df,pa,da){
  dt<-mef_filtrer_sta_annee(df, premiere_annee =pa,derniere_annee =da)
  dt<-sqldf('select distinct(pop_id),sta_id,avg(ipr) as ipr from dt group by pop_id')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >"5" and ipr <= "16" then "Bon"
                when ipr >"16" and  ipr <= "25" then "Moyen"
                when ipr >"25" and ipr<= "36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM dt')
  
  #Repartition de la classe de qualitee
  #calcul de Nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ipr,count( classe_ipr) as nbre_class  FROM 
                  dt group by classe_ipr ')
  
  #Creation de % 
  dt<- dt %>%
    mutate(nbre_class_pr=round(prop.table(nbre_class)*100,2))
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ipr
WHEN "Tres bon" THEN 1
WHEN "Bon" THEN 2
WHEN "Moyen" THEN 3
WHEN "Mediocre" THEN 4
ELSE 5 end ')
  dt<-dt%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  
  dt<-dt%>%mutate('Proportion des stations en %'=format(nbre_class_pr,nsmall=2))%>%select('Classe de qualite IPR'=type,'Nombre de stations'=nbre_class,'Proportion des stations en %')
  
}

data_reg<-function(df,pa,da){
  dt<-mef_filtrer_sta_annee(df, premiere_annee =pa,derniere_annee =da)
  dt<-sqldf('select distinct(reg_code_insee),avg(ipr) as ipr from dt group by reg_code_insee ')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >"5" and ipr <= "16" then "Bon"
                when ipr >"16" and  ipr <= "25" then "Moyen"
                when ipr >"25" and ipr<= "36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM dt')
  
  #Repartition de la classe de qualitee
  #calcul de Nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ipr,count( classe_ipr) as nbre_class  FROM 
                  dt group by classe_ipr ')
  dt<-dt%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  #Creation de % 
  dt<- dt %>%
    mutate(nbre_class_pr=round(prop.table(nbre_class)*100,2))
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ipr
WHEN "Tres bon" THEN 1
WHEN "Bon" THEN 2
WHEN "Moyen" THEN 3
WHEN "Mediocre" THEN 4
ELSE 5 end ')
  dt<-dt%>%mutate('Proportion des regions en %'=format(nbre_class_pr,nsmall=2))%>%select('Classe de qualite IPR'=type,'Nombre de regions'=nbre_class,'Proportion des regions en %')
  
}

data_dep<-function(df,pa,da){
  dt<-mef_filtrer_sta_annee(df, premiere_annee =pa,derniere_annee =da)
  dt<-sqldf('select distinct(dep_libelle),avg(ipr) as ipr from dt group by dep_libelle')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >"5" and ipr <= "16" then "Bon"
                when ipr >"16" and  ipr <= "25" then "Moyen"
                when ipr >"25" and ipr<= "36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM dt')
  
  #Repartition de la classe de qualitee
  #calcul de Nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ipr,count( classe_ipr) as nbre_class  FROM 
                  dt group by classe_ipr ')
  
  #Creation de % 
  dt<- dt %>%
    mutate(nbre_class_pr=round(prop.table(nbre_class)*100,2))
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ipr
WHEN "Tres bon" THEN 1
WHEN "Bon" THEN 2
WHEN "Moyen" THEN 3
WHEN "Mediocre" THEN 4
ELSE 5 end ')
  dt<-dt%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  dt<-dt%>%mutate('Proportion des departements en %'=format(nbre_class_pr,nsmall=2))%>%select('Classe de qualite IPR'=type,'Nombre de departements'=nbre_class,'Proportion des departements en %')
  
}

data_cd<-function(df,pa,da){
  dt<-mef_filtrer_sta_annee(df, premiere_annee =pa,derniere_annee =da)
  dt<-sqldf('select distinct(enh_code_sandre),avg(ipr) as ipr from dt group by enh_code_sandre ')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >"5" and ipr <= "16" then "Bon"
                when ipr >"16" and  ipr <= "25" then "Moyen"
                when ipr >"25" and ipr<= "36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM dt')
  
  #Repartition de la classe de qualitee
  #calcul de Nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ipr,count( classe_ipr) as nbre_class  FROM 
                  dt group by classe_ipr ')
  
  #Creation de % 
  dt<- dt %>%
    mutate(nbre_class_pr=round(prop.table(nbre_class)*100,2))
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ipr
WHEN "Tres bon" THEN 1
WHEN "Bon" THEN 2
WHEN "Moyen" THEN 3
WHEN "Mediocre" THEN 4
ELSE 5 end ')
  dt<-dt%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  dt<-dt%>%mutate('Proportion des cours d_eau en %'=format(nbre_class_pr,nsmall=2))%>%select('Classe de qualite IPR'=type,'Nombre de cours_d_eau'=nbre_class,'Proportion des cours d_eau en %')
  
}

data_bv<-function(df,pa,da){
  dt<-mef_filtrer_sta_annee(df, premiere_annee =pa,derniere_annee =da)
  dt<-sqldf('select distinct(BV),avg(ipr) as ipr from dt group by BV')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >"5" and ipr <= "16" then "Bon"
                when ipr >"16" and  ipr <= "25" then "Moyen"
                when ipr >"25" and ipr<= "36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM dt')
  
  #Repartition de la classe de qualitee
  #calcul de Nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ipr,count(classe_ipr) as nbre_class  FROM 
                  dt group by classe_ipr ')
  
  #Creation de % 
  dt<- dt %>%
    mutate(nbre_class_pr=round(prop.table(nbre_class)*100,2))
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ipr
WHEN "Tres bon" THEN 1
WHEN "Bon" THEN 2
WHEN "Moyen" THEN 3
WHEN "Mediocre" THEN 4
ELSE 5 end ')
  dt<-dt%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  dt%>%mutate('Proportion des bassins hydrographiques en %'=format(nbre_class_pr,nsmall=2))%>%select('Classe de qualite IPR'=type,'Nombre de bassins hydrographiques'=nbre_class,'Proportion des bassins hydrographiques en %')
}

data_ssbv<-function(df,pa,da){
  dt<-mef_filtrer_sta_annee(df, premiere_annee =pa,derniere_annee =da)
  dt<-sqldf('select distinct(NomSsBassi),avg(ipr) as ipr from dt group by NomSsBassi')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when ipr <="7" then "Tres bon"
                 when ipr  >"5" and ipr <= "16" then "Bon"
                when ipr >"16" and  ipr <= "25" then "Moyen"
                when ipr >"25" and ipr<= "36" then "Mediocre"
                else "Mauvais"
                end as classe_ipr FROM dt')
  
  #Repartition de la classe de qualitee
  #calcul de Nombre de stations selon les classes de qualitee
  dt<-sqldf('SELECT classe_ipr,count( classe_ipr) as nbre_class  FROM 
                  dt group by classe_ipr ')
  dt<-dt%>%mutate(
    type= case_when(
      classe_ipr=="Tres bon" ~ "Tr\U00E8s bon",
      classe_ipr=="Bon" ~ "Bon",
      classe_ipr=="Moyen" ~ "Moyen",
      classe_ipr=="Mediocre" ~"M\U00E9\U0064iocre",
      classe_ipr=="Mauvais" ~ "Mauvais"
    )
  )
  #Creation de % 
  dt<- dt %>%
    mutate(nbre_class_pr=round(prop.table(nbre_class)*100,2))
  dt<-sqldf('SELECT *  FROM dt order by  CASE classe_ipr
WHEN "Tres bon" THEN 1
WHEN "Bon" THEN 2
WHEN "Moyen" THEN 3
WHEN "Mediocre" THEN 4
ELSE 5 end ')
  dt%>%mutate('Proportion des sous-bassins hydrographiques en %'=format(nbre_class_pr,nsmall=2))%>%select('Classe de qualite IPR'=type,'Nombre de sous-bassins hydrographiques'=nbre_class,'Proportion des sous-bassins hydrographiques en %')
  
}
