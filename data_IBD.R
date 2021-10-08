source("filter_annee.R")

data_IBD<-function(df,pa,da){
  dt2<-filter_annee(df, premiere_annee =pa,derniere_annee =da)
  dt1<-sqldf('select distinct (CODE_SANDRE),DATE_DEBUT,annee,avg(IBD) as IBD from dt2 group by annee,CODE_SANDRE')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,annee,count( classe_ibd) as nbre_class  FROM 
                  dt group by classe_ibd,annee')
  
  #Creation de % 
  dt<-dplyr::group_by(dt,annee)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  
  

  dt<-dt%>%mutate('Proportion des stations en %'=format(nbre_class_pr,nsmall=2),'Classe de qualite IBD'=type1)%>%select('Annee'=annee,'Classe de qualite IBD','Nombre de stations'=nbre_class,'Proportion des stations en %')
  
}
data_IBD_reg<-function(df,pa,da){
  dt2<-filter_annee(df, premiere_annee =pa,derniere_annee =da)
  dt1<-sqldf('select distinct(CodeRegion),annee,avg(IBD) as IBD from dt2 group by annee,CodeRegion')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,annee,count(CodeRegion) as nbre_class  FROM 
                  dt group by classe_ibd,annee')
  #Creation de % 
  dt<-dplyr::group_by(dt,annee)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt<-dt%>%mutate('Proportion des regions en %'=format(nbre_class_pr,nsmall=2))%>%select('Annee'=annee,'Classe de qualite IBD'=type1,'Nombre de regions'=nbre_class,'Proportion des regions en %')
  
}


data_IBD_dep<-function(df,pa,da){
  dt2<-filter_annee(df, premiere_annee =pa,derniere_annee =da)
  dt1<-sqldf('select distinct(CodeDepartement),annee,avg(IBD) as IBD from dt2 group by annee,CodeDepartement')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,annee,count(CodeDepartement) as nbre_class  FROM 
                  dt group by classe_ibd,annee')
  #Creation de % 
  dt<-dplyr::group_by(dt,annee)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt<-dt%>%mutate('Proportion des departements en %'=format(nbre_class_pr,nsmall=2))%>%select('Annee'=annee,'Classe de qualite IBD'=type1,'Nombre de departements'=nbre_class,'Proportion des departements en %')
  
}

#fonction de repartition par periode
data_per_IBD<-function(df){
  
  dt1<-sqldf('select distinct (CODE_SANDRE),DATE_DEBUT,Periode,avg(IBD) as IBD from df group by annee,CODE_SANDRE')
  ##Rajout des classes de qualitees
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,Periode,count( classe_ibd) as nbre_class  FROM 
                  dt group by classe_ibd,Periode')
  
  #Creation de % 
  dt<-dplyr::group_by(dt,Periode)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt<-dt%>%mutate('Proportion des stations en %'=format(nbre_class_pr,nsmall=2))%>%select('Periode'=Periode,'Classe de qualite IBD'=type1,'Nombre de stations'=nbre_class,'Proportion des stations en %')
  
  
}

data_per_IBD_reg<-function(df){
  dt1<-sqldf('select distinct(CodeRegion),Periode,avg(IBD) as IBD from df group by Periode,CodeRegion')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,Periode,count(CodeRegion) as nbre_class  FROM 
                  dt group by classe_ibd,Periode')
  #Creation de % 
  dt<-dplyr::group_by(dt,Periode)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt<-dt%>%mutate('Proportion des regions en %'=format(nbre_class_pr,nsmall=2))%>%select('Periode'=Periode,'Classe de qualite IBD'=type1,'Nombre de regions'=nbre_class,'Proportion des regions en %')
  
}


data_per_IBD_dep<-function(df){
  
  dt1<-sqldf('select distinct(CodeDepartement),Periode,avg(IBD) as IBD from df group by Periode,CodeDepartement')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,Periode,count(CodeDepartement) as nbre_class  FROM 
                  dt group by classe_ibd,Periode')
  #Creation de % 
  dt<-dplyr::group_by(dt,Periode)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  
  dt<-dt%>%mutate('Proportion des departements en %'=format(nbre_class_pr,nsmall=2))%>%select('Periode'=Periode,'Classe de qualite IBD'=type1,'Nombre de departements'=nbre_class,'Proportion des departements en %')
  
}




data_IBD_ssbv<-function(df,pa,da){
  dt2<-filter_annee(df, premiere_annee =pa,derniere_annee =da)
  dt1<-sqldf('select distinct(NomSsBassi),annee,avg(IBD) as IBD from dt2 group by annee,NomSsBassi')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,annee,count(NomSsBassi) as nbre_class  FROM 
                  dt group by classe_ibd,annee')
  #Creation de % 
  dt<-dplyr::group_by(dt,annee)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt<-dt%>%mutate('Proportion des sous-bassins hydrographiques en %'=format(nbre_class_pr,nsmall=2))%>%select('Annee'=annee,'Classe de qualite IBD'=type1,'Nombre de sous-bassins hydrographiques'=nbre_class,'Proportion des sous-bassins hydrographiques en %')
  
}

#fonction de repartition par periode
data_per_IBD_ssbv<-function(df){
  
  dt1<-sqldf('select distinct(NomSsBassi),Periode,avg(IBD) as IBD from df group by Periode,NomSsBassi')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,Periode,count(NomSsBassi) as nbre_class  FROM 
                  dt group by classe_ibd,Periode')
  #Creation de % 
  dt<-dplyr::group_by(dt,Periode)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  
  dt<-dt%>%mutate('Proportion des sous-bassins hydrographiques en %'=format(nbre_class_pr,nsmall=2))%>%select('Periode'=Periode,'Classe de qualite IBD'=classe_ibd,'Nombre de sous-bassins hydrographiques'=nbre_class,'Proportion des sous-bassins hydrographiques en %')
  
}

data_IBD_bv<-function(df,pa,da){
  dt2<-filter_annee(df, premiere_annee =pa,derniere_annee =da)
  dt1<-sqldf('select distinct(BV),annee,avg(IBD) as IBD from dt2 group by annee,BV')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,annee,count(BV) as nbre_class  FROM 
                  dt group by classe_ibd,annee')
  #Creation de % 
  dt<-dplyr::group_by(dt,annee)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  
  dt<-dt%>%mutate('Proportion des  bassins hydrographiques en %'=format(nbre_class_pr,nsmall=2))%>%select('Annee'=annee,'Classe de qualite IBD'=type1,'Nombre de bassins hydrographiques'=nbre_class,'Proportion des  bassins hydrographiques en %')
  
}

#fonction de repartition par periode
data_per_IBD_bv<-function(df){
  
  dt1<-sqldf('select distinct(BV),Periode,avg(IBD) as IBD from df group by Periode,BV')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,Periode,count(BV) as nbre_class  FROM 
                  dt group by classe_ibd,Periode')
  #Creation de % 
  dt<-dplyr::group_by(dt,Periode)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt<-dt%>%mutate('Proportion des bassins hydrographiques en %'=format(nbre_class_pr,nsmall=2))%>%select('Periode'=Periode,'Classe de qualite IBD'=type1,'Nombre de bassins hydrographiques'=nbre_class,'Proportion des bassins hydrographiques en %')
  
}

IBD_cd<-function(df,pa,da){
  dt2<-filter_annee(df, premiere_annee =pa,derniere_annee =da)
  dt1<-sqldf('select distinct(cd),annee,avg(IBD) as IBD from dt2 group by annee,cd')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,annee,count(cd) as nbre_class  FROM 
                  dt group by classe_ibd,annee')
  #Creation de % 
  dt<-dplyr::group_by(dt,annee)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  
  dt<-dt%>%mutate('Proportion des  cours d_eau en %'=format(nbre_class_pr,nsmall=2))%>%select('Annee'=annee,'Classe de qualite IBD'=type1,'Nombre de cours d_eau'=nbre_class,'Proportion des  cours d_eau en %')
  
}

#fonction de repartition par periode
data_per_IBD_cd<-function(df){
  
  dt1<-sqldf('select distinct(cd),Periode,avg(IBD) as IBD from df group by Periode,cd')
  dt<-sqldf('SELECT *,case when IBD >"17" then "Tres bonne"
                 when IBD  >"13" and IBD <= "17" then "Bonne"
                when IBD >"9" and  IBD <= "13" then "Passable"
                when IBD >"5" and IBD<= "9" then "Mauvaise"
                else "Tres mauvaise"
                end as classe_ibd FROM dt1')
  
  #Repartition de la classe de qualitee
  #calcul de nombre de stations selon les classes de qualitees
  dt<-sqldf('SELECT classe_ibd,Periode,count(cd) as nbre_class  FROM 
                  dt group by classe_ibd,Periode')
  #Creation de % 
  dt<-dplyr::group_by(dt,Periode)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  
  dt<-dt%>%mutate('Proportion des cours d_eau en %'=format(nbre_class_pr,nsmall=2))%>%select('Periode'=Periode,'Classe de qualite IBD'=type1,'Nombre de cours d_eau'=nbre_class,'Proportion des cours d_eau en %')
  
}

