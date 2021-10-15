library(mgcv)
library(Hmisc)
library(ggplot2)
library(rgl)
gam<-function(df){
  Mod3<-mgcv::gam(logIPR ~ as.factor(annee) + s(jour_annee, bs= "cc") + s(pop_id, bs = "re"),data=df)
  par(mar = c(5.1, 6, 4.1, 2.1), mgp = c(4, 1, 0))
  coef.table <- as.data.frame(summary(f)$p.table)
  colnames(coef.table)[2] <- "SE"
  coef.table$annee <- as.numeric(substr( row.names(coef.table), nchar( row.names(coef.table))-3, nchar(
    row.names(coef.table))))
  coef.table$annee[1] <- min(na.omit(df$annee))
  coef.table$Estimate[1] <- 0
  coef.table$ind100 <- 100*(1+coef.table$Estimate)
  coef.table$SE100 <- 100*coef.table$SE
  coef.table$SE100 [1]<- 0
  coef.table$yminus<-coef.table$ind100-1.96*coef.table$SE100
  coef.table$yplus<-coef.table$ind100+1.96*coef.table$SE100
  ggplot(data = coef.table,
         aes(x = annee, y = ind100))+scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                                                                         breaks = 2010:2020,
         )+


    geom_point() +
    geom_smooth()+theme(plot.background=element_rect(fill="transparent",colour=NA))+labs(x =paste("Ann\U00E9\U0065"), y = "Valeur de l'indice IPR (base 100)")

}
gam1<-function(df){

  Mod3<-mgcv::gam(logIPR ~ as.factor(annee) + s(jour_annee, bs= "cc") + s(pop_id, bs = "re"),data=df)
}
f<-gam1(data_tendance)

saveRDS(f,"data_nat")
gam_bis<-function(df){

  ggplot(data =df,
         aes(x = annee, y = ind100))+scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                                                        breaks = 2010:2020,
         )+
    
    
    geom_point() +
    geom_smooth()+theme(plot.background=element_rect(fill="transparent",colour=NA))+labs(x =paste("Ann\U00E9\U0065"), y = "Valeur de l'indice IPR (base 100)")
  
}

data_gam<-function(df){
  
  Mod3<-mgcv::gam(logIPR ~ as.factor(annee) + s(jour_annee, bs= "cc") + s(pop_id, bs = "re"),data=df)
  
  par(mar = c(5.1, 6, 4.1, 2.1), mgp = c(4, 1, 0))
  coef.table <- as.data.frame(summary(Mod3)$p.table)
  colnames(coef.table)[2] <- "SE"
  coef.table$annee <- as.numeric(substr( row.names(coef.table), nchar( row.names(coef.table))-3, nchar(
    row.names(coef.table))))
  coef.table$annee[1] <- min(na.omit(df$annee))
  coef.table$Estimate[1] <- 0
  coef.table$ind100 <-100*(1+coef.table$Estimate)
  coef.table$SE100 <- 100*coef.table$SE
  coef.table$SE100 [1]<- 0
  coef.table$yminus<-coef.table$ind100-1.96*coef.table$SE100
  coef.table$yplus<-coef.table$ind100+1.96*coef.table$SE100
  coef.table %>%mutate('Annee'=annee,'Valeurs de l\'IPR'=format(round(ind100,2),nsmall=2),'Borne minimale de l\'intervalle de confiance'=format(round(yminus,2),nsmall=2), 'Borne maximale de l\'intervalle de confiance'=format(round(yplus,2),nsmall=2))%>%
    select(Annee:'Borne maximale de l\'intervalle de confiance')
  

}
gam_bv<-function(df){
  
  
  Mod3_bv<-mgcv::gam(logIPR ~ as.factor(annee) + s(jour_annee, bs= "cc") + s(pop_id, bs = "re"),data=df)
  
  par(mar = c(5.1, 6, 4.1, 2.1), mgp = c(4, 1, 0))
  coef.table <- as.data.frame(summary(Mod3_bv)$p.table)
  colnames(coef.table)[2] <- "SE"
  coef.table$annee <- as.numeric(substr( row.names(coef.table), nchar( row.names(coef.table))-3, nchar(
    row.names(coef.table))))
  coef.table$annee[1] <- min(na.omit(df$annee))
  coef.table$Estimate[1] <- 0
  coef.table$ind100 <- 100*(1+coef.table$Estimate)
  coef.table$SE100 <- 100*coef.table$SE
  coef.table$SE100 [1]<- 0
  coef.table$yminus<-coef.table$ind100-1.96*coef.table$SE100
  coef.table$yplus<-coef.table$ind100+1.96*coef.table$SE100
  ggplot(data = coef.table,
         aes(x = annee, y = ind100))+scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                                                                         breaks = 2010:2020,
         ) +
    
    geom_point() +
    geom_smooth()+theme(plot.background=element_rect(fill="transparent",colour=NA))+labs(x = paste("Ann\U00E9\U0065"), y = "Valeur de l'indice IPR (base 100)")
  
  
}
gam_ssbv<-function(df){
  
  Mod3_ssbv<-mgcv::gam(logIPR ~ as.factor(annee) + s(jour_annee, bs= "cc") + s(pop_id, bs = "re"),data=df)
  
  par(mar = c(5.1, 6, 4.1, 2.1), mgp = c(4, 1, 0))
  coef.table <- as.data.frame(summary(Mod3_ssbv)$p.table)
  colnames(coef.table)[2] <- "SE"
  coef.table$annee <- as.numeric(substr( row.names(coef.table), nchar( row.names(coef.table))-3, nchar(
    row.names(coef.table))))
  coef.table$annee[1] <- min(na.omit(df$annee))
  coef.table$Estimate[1] <- 0
  coef.table$ind100 <- 100*(1+coef.table$Estimate)
  coef.table$SE100 <- 100*coef.table$SE
  coef.table$SE100 [1]<- 0
  coef.table$yminus<-coef.table$ind100-1.96*coef.table$SE100
  coef.table$yplus<-coef.table$ind100+1.96*coef.table$SE100
  ggplot(data = coef.table,
         aes(x = annee, y = ind100))+scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                                                                         breaks = 2010:2020,
         ) +
    
    geom_point() +
    geom_smooth()+theme(plot.background=element_rect(fill="transparent",colour=NA))+labs(x =paste("Ann\U00E9\U0065"), y = "Valeur de l'indice IPR (base 100)")
  
  
}



gam_ibd<-function(df){
  ggplot(data =df,
         aes(x = annee, y = ind100))+scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                                                                         breaks = 2010:2020,
         ) +
    geom_point() +
    geom_smooth()+theme(plot.background=element_rect(fill="transparent",colour=NA))+labs(x =paste("Ann\U00E9\U0065"), y = "Valeur de l'indice IBD (base 100)")
}

