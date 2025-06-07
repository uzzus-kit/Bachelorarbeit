library(dplyr)
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\WIS.R")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\Nowcastings\\nowcasting_künstlichesProb_spät_True.R")   #Alternativ kann man auch die Nowcasts laden
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\truth\\truth_berechnen_Altersstratifiziert.R")
#Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcast_RSV.csv")
WIS=function(Nowcast,triangle){
    Evaluations_Nowcast=Nowcast[match(as.Date("2024-12-26"),Nowcast$forecast_date):length(Nowcast[,1]),]      #begrenzt die Nowcasts auf den Zeitraum ab dem mit History von 10 Wochen der erste Nowcast möglich ist
    Nowcast_truth=as.data.frame(truth(triangle))
Nowcast_truth[,5] <- as.Date(Nowcast_truth[,5]) + 4

Evaluations_Nowcast=as.data.frame(Evaluations_Nowcast)
Evaluations_Nowcast[,3]=as.Date(Evaluations_Nowcast[,3])
Evaluations_Nowcast <- Evaluations_Nowcast %>%
  left_join(Nowcast_truth, by = c("age_group", "forecast_date"="date"))
Evaluations_Nowcast$model=Evaluations_Nowcast$age_group[1]
compute_wis(Evaluations_Nowcast)
}
score_schnitt=numeric(7)
triangles_geteilt=list(7)
i=1  
for (age in unique(triangles[[1]]$age_group)){
    triangles_geteilt[[i]]=triangles[[1]][which(triangles[[1]]$age_group==age),]
    i=i+1
  }
for (i in 1:length(Nowcast_einzel)){
  options(digits = 10)
x=print(as.data.frame(WIS(Nowcast_einzel[[i]],triangles_geteilt[[i]])),digits = 10)
score_schnitt[i]=x$score
}
mean(score_schnitt[1:7])
median(score_schnitt)

