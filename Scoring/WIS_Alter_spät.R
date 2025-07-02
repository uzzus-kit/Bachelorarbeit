target=list()
target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari.csv")
Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcasts\\Nowcast_künstlichesProb_spät10.csv")
nowcast=list()
Nowcast_zerlegen_Alter=function(Nowcast){
  ages=unique(Nowcast$age_group)
  Nowcast_einzel=list()
  i=1
  for (age in ages){
    Nowcast_einzel[[i]]=Nowcast[which(Nowcast$age_group==age),]
    i=i+1
  }
  return(Nowcast_einzel)
}
ages=unique(Nowcast$age_group)
nowcast=Nowcast_zerlegen_Alter(Nowcast)
Indikatorfunktion=function(y,q){
  if(y<=q){
    return(1)
  }else if(y>q){
    return(0)
  }
}
WIS_berechnen=function(Nowcast,target){
  #Nowcast=nowcast[[2]]
  #target=target_age[[2]]
  Nowcast=Nowcast[match(as.Date("2024-12-26"),Nowcast$forecast_date):length(Nowcast[,1]),]
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
      if((length(which(target$date==date))==0)){
        #print(paste0(date,"gibt es nicht in ",target$date))
        break
      }else{
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
  }
  return(WIS)
}
target_age=list()
for (age in ages) {
  target_age[[age]]=target[which(target$age_group==age),]
}
WIS_auswerten=function(WIS){
  WIS_gesamt=numeric(length(WIS))
  for (i in 1:length(WIS_gesamt)){
    WIS_gesamt[i]=mean(WIS[[i]])
  }
  average=mean(WIS_gesamt)
  return(average)
}
WIS=list()
for (i in 1:length(ages)){
  WIS[[i]]=WIS_berechnen(nowcast[[i]],target_age[[i]])
  WIS[[i]]=WIS_auswerten(WIS[[i]])
}
for(i in 1:length(WIS)){
  print(paste0(ages[[i]],": ",WIS[[i]]))
}
