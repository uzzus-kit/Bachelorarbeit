Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcasts\\Nowcast_Krankheit_False5.csv")
unique(Nowcast$disease)
target=list()
target[["sari"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari.csv")
target[["sari_covid"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_covid19.csv")
target[["sari_influenza"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_influenza.csv")
target[["sari_rsv"]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target-icosari-sari_rsv.csv")

sari_gekürzt_target=target[["sari"]][which(target[["sari"]]$age_group=="00+" & target[["sari"]]$date %in% target[["sari_rsv"]]$date),]
row.names(sari_gekürzt_target)=NULL
target[["Rest"]]=sari_gekürzt_target
for(i in 1:length(target[["Rest"]][,1])){
  target[["Rest"]][i,which(colnames(target[["Rest"]])=="value")]=
    sari_gekürzt_target[i,which(colnames(sari_gekürzt_target)=="value")]-
    target[["sari_rsv"]][i,which(colnames(target[["sari_rsv"]])=="value")]-
    target[["sari_influenza"]][i,which(colnames(target[["sari_influenza"]])=="value")]-
    target[["sari_covid"]][i,which(colnames(target[["sari_covid"]])=="value")]
}

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
nowcast=Nowcast_zerlegen_Krankheit(Nowcast)
unique(nowcast[[1]]$target_end_date)
Überdeckungswarscheinlichkeit_Krankheit=function(cast,target, Konfidenzniveau){
  #cast=nowcast[[1]]
  #target=target[[1]]
  #Konfidenzniveau=0.95
  treffer=0
  ges=0
  alpha=1-Konfidenzniveau
  for(now_date in unique(cast$forecast_date)){
    for(target_date in unique(cast[which(cast$forecast_date==now_date),]$target_end_date)){
      unteres_quantil=cast[which(cast$forecast_date==now_date&cast$target_end_date==target_date&cast$quantile==as.character(alpha/2)),8]
      oberes_quantil=cast[which(cast$forecast_date==now_date&cast$target_end_date==target_date&cast$quantile==as.character(1-alpha/2)),8]
      wahrer_Wert=target[which(target$date==target_date),6]
      if(wahrer_Wert>=unteres_quantil&wahrer_Wert<=oberes_quantil){
        treffer=treffer+1
      }
      ges=ges+1
    }
  }
  Wahrscheinlichkeit=treffer/ges
  return(Wahrscheinlichkeit)
}
diseases=c("sari","sari_covid","sari_influenza","sari_rsv","Rest")
for (disea in diseases) {
  target[[disea]]=target[[disea]][which(target[[disea]]$age_group=="00+"),]
}
for (i in 1:5) {
  print(Überdeckungswarscheinlichkeit_Krankheit(nowcast[[i]],target[[i]],0.5))
  print(Überdeckungswarscheinlichkeit_Krankheit(nowcast[[i]],target[[i]],0.95))
}

