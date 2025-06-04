library(dplyr)
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\WIS.R")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\Nowcastings\\nowcasting_Krankheiten_True_Durchschnitt.R")   #Alternativ kann man auch die Nowcasts laden
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\truth_berechnen.R")
#Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcast_RSV.csv")
WIS=function(Nowcast,triangle){
Nowcast_truth=as.data.frame(truth(triangle))
Nowcast_truth[,5] <- as.Date(Nowcast_truth[,5]) + 4

for(i in 1:length(Nowcast)){
  Nowcast=as.data.frame(Nowcast)
}
Nowcast[,3]=as.Date(Nowcast[,3])
Nowcast <- Nowcast %>%
  left_join(Nowcast_truth, by = c("age_group", "forecast_date"="date"))
Nowcast$model="Test"
compute_wis(Nowcast)
}
score_schnitt=numeric(5)
for (i in 1:length(Nowcast_einzel)){
  options(digits = 10)
x=print(as.data.frame(WIS(Nowcast_einzel[[i]],triangles[[i]])),digits = 10)
score_schnitt[i]=x$score
}
mean(score_schnitt[1:5])
median(score_schnitt)
diseases
