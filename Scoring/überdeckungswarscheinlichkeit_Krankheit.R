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
write.csv(target[["Rest"]],"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Evaluation\\target_Rest.csv",row.names = FALSE)
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
Überdeckungswarscheinlichkeit_Krankheit=function(nowcast,target, Konfidenzniveau){
if(unique(nowcast$disease)=="sari_rsv"){
  nowcast=nowcast[match(as.Date("2025-01-23"),nowcast$forecast_date):length(nowcast[,1]),]
}else{
  nowcast=nowcast[match(as.Date("2024-12-26"),nowcast$forecast_date):length(nowcast[,1]),]      #begrenzt die Nowcasts auf den Zeitraum ab dem mit History von 10 Wochen der erste Nowcast möglich ist
}
  treffer=0
  gesamt=0
  alpha=1-Konfidenzniveau
  print(paste0(unique(nowcast$disease),"  ", Konfidenzniveau))
  for (date in unique(nowcast$target_end_date)){
    nowcast_date=unique(nowcast[which(nowcast$target_end_date==date),3])
    for(j in nowcast_date){
      if(target[which(target$date==date),6]<=nowcast[which(nowcast$forecast_date==j&nowcast$target_end_date==date&nowcast$quantile==as.character(1-alpha/2)),8]&
         target[which(target$date==date),6]>=nowcast[which(nowcast$forecast_date==j&nowcast$target_end_date==date&nowcast$quantile==as.character(alpha/2)),8]){
       
        treffer=treffer+1
      }
      gesamt=gesamt+1
      }
  }
  Wahrscheinlichkeit=treffer/gesamt
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

