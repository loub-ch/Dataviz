mef_filtrer_sta_annee <- function(df, stations_id = NA, premiere_annee = NA,
                                  derniere_annee = NA)
  
{
  
  # -----------------------------------------------------------
  # Filtrage des données selon les arguments
  # -----------------------------------------------------------
  # selon stations_id
  if (!is.na(stations_id)) df <- df %>% filter(sta_id %in% stations_id)
  
  # selon les années sélectionnées
  if (!is.na(premiere_annee))
  {
    df <- df %>% filter(annee >= premiere_annee)
  } else{
    premiere_annee <- min(df$annee, na.rm = T)
  }
  
  if (!is.na(derniere_annee))
  {
    df <- df %>% filter(annee <= derniere_annee)
  } else{
    derniere_annee <- max(df$annee, na.rm = T)
  }
  
  df
  
}
