library(dplyr)
target=list()
target[["sari"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari.csv")
target[["sari_covid"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_covid19.csv")
target[["sari_influenza"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_influenza.csv")
target[["sari_rsv"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_rsv.csv")
target[["Rest"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target_Rest.csv")
nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcasts\\Nowcast_Krankheit_False5.csv")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\WIS.R")
#source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\Nowcastings\\nowcasting_Krankheiten_False.R")   #Alternativ kann man auch die Nowcasts laden
#source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\truth\\truth_berechnen_Krankheitsstratifiziert.R")
#Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcast_RSV.csv")
Nowcast_zerlegen_Krankheit=function(Nowcast){
  Nowcast_einzel=list()
  i=1
  diseases=unique(Nowcast$disease)
  for (disea in diseases){
    Nowcast_einzel[[i]]=Nowcast[which(Nowcast$disease==disea),]
    i=i+1
  }
  return(Nowcast_einzel)
}
nowcast=Nowcast_zerlegen_Krankheit(nowcast)

diseases=c("sari","sari_covid","sari_influenza","sari_rsv","Rest")
for (disea in diseases) {
  target[[disea]]=target[[disea]][which(target[[disea]]$age_group=="00+"),]
  colnames(target[[disea]])[6]="truth"
}
WIS=function(target,Nowcast){
  #target=target[[1]]
  #Nowcast=nowcast[[1]]
  if(Nowcast$disease[1]=="sari_rsv"){
    Evaluations_Nowcast=Nowcast[match(as.Date("2025-01-23"),Nowcast$forecast_date):length(Nowcast[,1]),]
  }else{
    Evaluations_Nowcast=Nowcast[match(as.Date("2024-12-26"),Nowcast$forecast_date):length(Nowcast[,1]),]      #begrenzt die Nowcasts auf den Zeitraum ab dem mit History von 10 Wochen der erste Nowcast möglich ist
  }
  
Evaluations_Nowcast=as.data.frame(Evaluations_Nowcast)
target=as.data.frame(target)
Evaluations_Nowcast[,3]=as.Date(Evaluations_Nowcast[,3])
Evaluations_Nowcast <- Evaluations_Nowcast %>%
  left_join(target, by = c("age_group", "target_end_date"="date"))
Evaluations_Nowcast$model=Evaluations_Nowcast$disease[1]
compute_wis(Evaluations_Nowcast)
}
score_schnitt=numeric(5)
for (i in 1:length(nowcast)){
  options(digits = 10)
  target[[i]]$date=as.Date(target[[i]]$date)
  nowcast[[i]]$target_end_date=as.Date(nowcast[[i]]$target_end_date)
x=print(as.data.frame(WIS(target[[i]],nowcast[[i]])),digits = 10)
score_schnitt[i]=x$score
}
mean(score_schnitt[2:5])
median(score_schnitt)
diseases
