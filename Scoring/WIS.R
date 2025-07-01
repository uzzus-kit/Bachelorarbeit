target=list()
target[["sari"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari.csv")
target[["sari_covid"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_covid19.csv")
target[["sari_influenza"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_influenza.csv")
target[["sari_rsv"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_rsv.csv")
target[["Rest"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target_Rest.csv")
Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcasts\\Nowcast_Krankheit_True.csv")
nowcast=list()
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
diseas=unique(Nowcast$disease)
nowcast=Nowcast_zerlegen_Krankheit(Nowcast)
Indikatorfunktion=function(y,q){
  if(y<=q){
    return(1)
  }else if(y>q){
    return(0)
  }
}
WIS_berechnen=function(Nowcast,target){
  if(Nowcast$disease[1]=="sari_rsv"){
    Nowcast=Nowcast[match(as.Date("2025-01-23"),Nowcast$forecast_date):length(Nowcast[,1]),]
  }else{
    Nowcast=Nowcast[match(as.Date("2024-12-26"),Nowcast$forecast_date):length(Nowcast[,1]),]      #begrenzt die Nowcasts auf den Zeitraum ab dem mit History von 10 Wochen der erste Nowcast möglich ist
  }
  tau=unique(Nowcast$quantile)
  tau=tau[!is.na(tau)]
  Nowcast_aufgebrochen=list()
  i=1
  for (date in unique(Nowcast$forecast_date)){
    Nowcast_aufgebrochen[[i]]=subset(Nowcast,Nowcast$forecast_date==date)
    i=i+1
  }
  WIS=list()
  for (i in 1:length(Nowcast_aufgebrochen)){
    WIS[[i]]=numeric(4)
    j=1
    for(date in unique(Nowcast_aufgebrochen[[i]]$target_end_date)){                   #WIS für ein Forecast über alle target dates
      K=length(tau)
      y=target[which(target$date==date),6]
      summe=0
      for(k in 1:K){
        q_tauk=Nowcast_aufgebrochen[[i]][which(Nowcast_aufgebrochen[[i]]$target_end_date==date&Nowcast_aufgebrochen[[i]]$quantile==tau[k]),8]
        summe=summe+(2*(Indikatorfunktion(y,q_tauk)-tau[k]))*(q_tauk-y)
        }
      WIS[[i]][j]=1/(K)*summe
      j=j+1
    }
  }
  return(WIS)
}
for (disea in diseas) {
  target[[disea]]=target[[disea]][which(target[[disea]]$age_group=="00+"),]
}
WIS_auswerten=function(WIS){
  WIS_gesamt=numeric(length(WIS))
  for (i in 1:length(WIS_gesamt)){
    WIS_gesamt[i]=mean(WIS[[i]])
  }
  average=mean(WIS_gesamt)
  return(average)
}
x=WIS_berechnen(nowcast[[1]],target[["sari"]])
print(WIS_auswerten(x))
length(unique(nowcast[[1]]$forecast_date))
