library(dplyr)
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\WIS.R")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\Nowcastings\\nowcasting_Krankheiten_False.R")   #Alternativ kann man auch die Nowcasts laden
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\truth\\truth_berechnen_Krankheitsstratifiziert_spät.R")
#Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcast_RSV.csv")
WIS=function(Nowcast,triangle){
  if(Nowcast$disease[1]=="sari_rsv"){
    Evaluations_Nowcast=Nowcast[match(as.Date("2025-01-23"),Nowcast$forecast_date):length(Nowcast[,1]),]
  }else{
    Evaluations_Nowcast=Nowcast[match(as.Date("2024-12-26"),Nowcast$forecast_date):length(Nowcast[,1]),]      #begrenzt die Nowcasts auf den Zeitraum ab dem mit History von 10 Wochen der erste Nowcast möglich ist
  }
    Nowcast_truth=as.data.frame(truth(triangle))
Nowcast_truth[,5] <- as.Date(Nowcast_truth[,5]) + 4

Evaluations_Nowcast=as.data.frame(Evaluations_Nowcast)
Evaluations_Nowcast[,3]=as.Date(Evaluations_Nowcast[,3])
Evaluations_Nowcast <- Evaluations_Nowcast %>%
  left_join(Nowcast_truth, by = c("age_group", "forecast_date"="date"))
Evaluations_Nowcast$model=Evaluations_Nowcast$disease[1]
compute_wis(Evaluations_Nowcast)
}
score_schnitt=numeric(5)
for (i in 1:length(Nowcast_einzel)){
  options(digits = 10)
x=print(as.data.frame(WIS(Nowcast_einzel[[i]],triangles[[i]])),digits = 10)
score_schnitt[i]=x$score
}
mean(score_schnitt[2:5])
median(score_schnitt)
diseases
