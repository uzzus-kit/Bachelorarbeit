library(dplyr)
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\WIS.R")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\Nowcastings\\nowcasting_Krankheiten_False.R")   #Alternativ kann man auch die Nowcasts laden
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\truth_berechnen.R")
#Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcast_RSV.csv")
#WIS=function(Nowcaast){
Nowcast_1_truth=as.data.frame(truth(triangles[[1]]))
Nowcast_1_truth[,5] <- as.Date(Nowcast_1_truth[,5]) + 4

for(i in 1:length(Nowcast_einzel)){
  Nowcast_einzel[[i]]=as.data.frame(Nowcast_einzel[[i]])
}
Nowcast_einzel[[1]][,3]=as.Date(Nowcast_einzel[[1]][,3])
Nowcast_einzel[[1]] <- Nowcast_einzel[[1]] %>%
  left_join(Nowcast_1_truth, by = c("age_group", "forecast_date"="date"))
Nowcast_einzel[[1]]$model="Test"
compute_wis(Nowcast_einzel[[1]])
#}