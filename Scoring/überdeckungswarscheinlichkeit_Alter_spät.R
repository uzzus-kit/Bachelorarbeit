Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcasts\\Nowcast_künstlichesProb_spät10.csv")
target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari.csv")

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
nowcast=Nowcast_zerlegen_Alter(Nowcast)
unique(nowcast[[1]]$target_end_date)
Überdeckungswarscheinlichkeit_Krankheit=function(nowcast,target, Konfidenzniveau){
  nowcast=nowcast[match(as.Date("2024-12-26"),nowcast$forecast_date):length(nowcast[,1]),]
  treffer=0
  gesamt=0
  alpha=1-Konfidenzniveau
  #target=target_age[[2]]
  #nowcast=nowcast[[2]]
  print(paste0(unique(nowcast$age_group),"  ", Konfidenzniveau))
  for (date in unique(nowcast$target_end_date)){
    nowcast_date=unique(nowcast[which(nowcast$target_end_date==date),3])
    for(j in nowcast_date){
      if((length(which(target$date==date))==0)){
        #print(paste0(date,"gibt es nicht in ",target$date))
        break
      }
        if(target[which(target$date==date),6]<=nowcast[which(nowcast$forecast_date==j&nowcast$target_end_date==date&as.character(nowcast$quantile)==as.character(1-alpha/2)),8]&
           target[which(target$date==date),6]>=nowcast[which(nowcast$forecast_date==j&nowcast$target_end_date==date&nowcast$quantile==as.character(alpha/2)),8]){
       
        treffer=treffer+1
      }
      gesamt=gesamt+1
    }
    #browser()
  }
  Wahrscheinlichkeit=treffer/gesamt
  return(Wahrscheinlichkeit)
}
ages=c("00+","00-04","05-14","15-34","35-59","60-79","80+")
target_age=list()
for (age in ages) {
  target_age[[age]]=target[which(target$age_group==age),]
}
for (i in 1:7) {
  print(Überdeckungswarscheinlichkeit_Krankheit(nowcast[[i]],target_age[[i]],0.5))
  print(Überdeckungswarscheinlichkeit_Krankheit(nowcast[[i]],target_age[[i]],0.95))
}

