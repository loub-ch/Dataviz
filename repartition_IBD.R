source("filter_annee.R")
library(plotly)


#repartition par annee
repartition_IBD<-function(df,pa,da){
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
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  
  # dt<-dt%>%mutate(
  #   order= case_when(
  #     classe_ibd=="Tres bonne" ~ "1",
  #     classe_ibd=="Bonne" ~ "2",
  #     classe_ibd=="Passable" ~ "3",
  #     classe_ibd=="Mauvaise" ~"4",
  #     classe_ibd=="Tres mauvaise" ~ "5"
  #   )
  # )
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt<-dt%>%mutate(
    order= case_when(
      type1=="Tr\U00E8s bonne" ~ "1",
      type1=="Bonne" ~ "2",
      type1=="Passable" ~ "3",
      type1=="Mauvaise" ~"4",
      type1=="Tr\U00E8s mauvaise" ~ "5"
    )
  )
 
  
  #Creation de % 
  dt<-dplyr::group_by(dt,annee)
  
  dt<-dplyr::mutate(dt,nbre_class_pr =round(prop.table(nbre_class)*100,2))

 # c(rep("Tr\U00E8s bonne",5),rep("Bonne",5),rep("Passble",5),rep("Mauvaise",5),rep("Tr\U00E8s mauvaise",5))
  # dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s bonne","Bonne","Passable","Mauvaise","Tr\U00E8s mauvaise"))
  # dt$couleur <-as.factor( dt$couleur)
  # dt$couleur <- factor(dt$couleur, levels = c("blue","darkgreen","yellow","orange","red"))
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
  plot_ly(x = ~annee,y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),type='bar',hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"stations")) %>%
    # add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,tickvals=2010:2020,title=paste("Ann\U00E9\U0065")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}

repartition_IBD_reg<-function(df,pa,da){
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~annee, y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"r\U00E9gions")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,tickvals=2010:2020,title=paste("Ann\U00E9\U0065")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}
repartition_IBD_dep<-function(df,pa,da){
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~annee, y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"d\U00E9partements")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,tickvals=2010:2020,title=paste("Ann\U00E9\U0065")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}
repartition_IBD_cd<-function(df,pa,da){
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~annee, y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"cours d'eau")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,tickvals=2010:2020,title=paste("Ann\U00E9\U0065")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}
repartition_IBD_bv<-function(df,pa,da){
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~annee, y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"bassins hydrographiques")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,tickvals=2010:2020,title=paste("Ann\U00E9\U0065")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}
repartition_IBD_ssbv<-function(df,pa,da){
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~annee, y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"sous-bassins hydrographiques")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,tickvals=2010:2020,title=paste("Ann\U00E9\U0065")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}

#fonction de repartition par periode
rep_IBD<-function(df){
  
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
  
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~Periode, y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"stations")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,title=paste("P\U00E9riode")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}


rep_IBD_reg<-function(df){
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~Periode, y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"r\U00E9gions")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor = "transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,title=paste("P\U00E9riode")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}
rep_IBD_dep<-function(df){

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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~Periode,y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"d\U00E9partements")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor="transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,title=paste("P\U00E9riode")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}

rep_IBD_bv<-function(df){
  
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~Periode,y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"bassins hydrographiques")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor="transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,title=paste("P\U00E9riode")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}

rep_IBD_ssbv<-function(df){
  
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~Periode,y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"sous-bassins hydrographiques")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor="transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,title=paste("P\U00E9riode")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}

rep_IBD_cd<-function(df){
  
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
  
  
  
  
  
  dt<-sqldf('SELECT *,case when classe_ibd=="Tres bonne" then "blue"
                 when classe_ibd=="Bonne" then "darkgreen"
                when classe_ibd=="Passable" then "yellow"
                when classe_ibd=="Mauvaise" then "orange"
                else "red"
                end as couleur FROM dt')
  dt<-dt%>%mutate(
    type1= case_when(
      classe_ibd=="Tres bonne" ~ "Tr\U00E8s bonne",
      classe_ibd=="Bonne" ~ "Bonne",
      classe_ibd=="Passable" ~ "Passable",
      classe_ibd=="Mauvaise" ~"Mauvaise",
      classe_ibd=="Tres mauvaise" ~ "Tr\U00E8s mauvaise"
    )
  )
  dt$type1 <- factor(dt$type1, levels = c("Tr\U00E8s mauvaise","Mauvaise","Passable","Bonne","Tr\U00E8s bonne"))
  
  fig <- dt%>%
    plot_ly(x = ~Periode,y =~nbre_class_pr,name=~type1,color=~type1,colors=c("red","orange","yellow","darkgreen","blue"),hoverinfo="text",hovertext= ~paste(nbre_class_pr,"% soit",nbre_class,"cours d'eau")) %>%
    add_bars() %>%
    layout(barmode = "stack",legend = list(x = 100, y = 0.5),paper_bgcolor="transparent",plot_bgcolor="transparent",
           xaxis = list(showgrid = F, zeroline = F, showticklabels = T,title=paste("P\U00E9riode")),
           yaxis = list(showgrid = F, zeroline = F, showticklabels = T,title="Proportion du nombre de classes (en %)"))
  fig
}