source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\nowcasting_einzel_Krankheiten.R")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\Berechnungen.R")
Nowcast_sari
quantil=unique(Nowcast_sari[[1]]$quantile)[2:length(unique(Nowcast_sari[[1]]$quantile))]
Nowcast_nach_Quantilen=function(Nowcast_Krankheit){
# First, filter out rows where most values are NA
Nowcast_filtered <- lapply(Nowcast_Krankheit, function(df) {
  # Remove rows where quantile is NA
  df[!is.na(df$quantile), ]
})

# Now extract data by quantile as originally intended
quantil <- unique(Nowcast_filtered[[1]]$quantile)
Nowcast_Krankheit_quantile <- list()

for (quan in quantil) {
  key <- as.character(quan)
  
  for (i in seq_along(Nowcast_filtered)) {
    df_q <- Nowcast_filtered[[i]][Nowcast_filtered[[i]]$quantile == quan, ]
    
    if (is.null(Nowcast_Krankheit_quantile[[key]])) {
      Nowcast_Krankheit_quantile[[key]] <- df_q
    } else {
      Nowcast_Krankheit_quantile[[key]] <- rbind(Nowcast_Krankheit_quantile[[key]], df_q)
    }
  }
}
  for (i in seq_along(Nowcast_Krankheit)){
    df_q <- Nowcast_Krankheit[[i]][Nowcast_Krankheit[[i]]$type == "mean", ]
    if (is.null(Nowcast_Krankheit_quantile[[as.character("mean")]])) {
      Nowcast_Krankheit_quantile[[as.character("mean")]] <- df_q
    } else {
      Nowcast_Krankheit_quantile[[as.character("mean")]] <- rbind(Nowcast_Krankheit_quantile[[as.character("mean")]], df_q)
    }
  }

return(Nowcast_Krankheit_quantile)
}
Nowcast_sari_quantile=Nowcast_nach_Quantilen(Nowcast_sari)
Nowcast_covid_quantile=Nowcast_nach_Quantilen(Nowcast_covid)
Nowcast_influenza_quantile=Nowcast_nach_Quantilen(Nowcast_influenza)
Nowcast_RSV_quantile=Nowcast_nach_Quantilen(Nowcast_RSV)
Nowcast_Rest_quantile=Nowcast_nach_Quantilen(Nowcast_Rest)

#Altersstratifizierung aus target raus bekommen
tar
for (i in seq_along(unique(targets[["sari_covid"]]$date))){
  for (i in 2:5){
    
  }
}

alpha=0.05
IS_Covid=interval_score(targets["sari_covid"],alpha,Nowcast_covid_quantile[[as.character(alpha/2)]][,8],Nowcast_covid_quantile[[as.character(1-alpha/2)]][,8])
