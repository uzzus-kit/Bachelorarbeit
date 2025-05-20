Covid=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_covid19.csv")
Influenza=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_influenza.csv")
RSV=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_rsv.csv")
sari=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari.csv")
Covid_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_covid19.csv")
Influenza_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_influenza.csv")
RSV_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_rsv.csv")
sari_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv")
Covid=Covid[c(1:27, 136:189),]                  #Überflüssige Alterstratifizierungen rausnehmen
row.names(Covid)=NULL
Influenza=Influenza[1:27,]                      #Gibt nur eine Altersgruppe
RSV=RSV[1:27,]                                  #Gibt nur eine Altersgruppe
Covid_gesamt=matrix(0,nrow = (length(Covid[,1])/3),ncol = length(Covid[1,]))
Covid_gesamt=Covid[1:27,]
sari_gesamt=sari[1:119,]
#Stratifizierung der Sari daten raus nehmen
for (i in 1:119){
  for(j in which(names(sari)=="value_0w"):which(names(sari)=="value_.10w")){
    sari_gesamt[i, j] <- sum(sari[which(sari$date == sari$date[i]), j], na.rm = TRUE)
  }
}
#Jetzt noch die Stratifizierung der Covid Daten raus nehmen
for(i in 1:(length(Covid[,1])/3)){
  for(j in which(names(Covid)=="value_0w"):which(names(Covid)=="value_.10w")){
    Covid_gesamt[i,j]=Covid[i,j]+Covid[i+27,j]+Covid[i+(2*27),j]
  }
}
#Gesamt Verzüge berechnen
Gesamtverzüge=Covid_gesamt
Woche0=which(names(Gesamtverzüge)=="value_0w")
Woche10=which(names(Gesamtverzüge)=="value_.10w")
for(i in 1:length(Gesamtverzüge[,1])){
  for(j in Woche0:Woche10){
    Gesamtverzüge[i,j]=Covid_gesamt[i,j]+RSV[i,j]+Influenza[i,j]
  }
}
Rest=Gesamtverzüge
#Rest werte berechnen
sari_gekürzt=sari_gesamt[93:119,]
for (i in 1:27){
  for(j in Woche0:Woche10){
    Rest[i,j]=sari_gekürzt[i,j]-Gesamtverzüge[i,j]
  }
}
write.csv(Rest,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Rest_nowcast.csv",row.names = FALSE)
write.csv(Covid_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Covid_nowcast.csv",row.names = FALSE)
write.csv(Influenza,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Influenza_nowcast.csv",row.names = FALSE)
write.csv(RSV,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\RSV_nowcast.csv",row.names = FALSE)
write.csv(sari_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\sari_nowcast.csv",row.names = FALSE)

gesamt_target=function(Daten_target){
age_group=unique(Daten_target$age_group)
dates=unique(Daten_target$date)
Daten_target_gesamt=Daten_target[which(Daten_target$age_group==age_group[1]),]
i=1
for(date in dates){
    Daten_target_gesamt[i,which(names(Daten_target_gesamt)=="value")]=sum(Daten_target$value[which(Daten_target$date==date)])
    i=i+1
}
return(Daten_target_gesamt)
}
sari_target_gesamt=gesamt_target(sari_target)
Covid_target_gesamt=gesamt_target(Covid_target)
Influenza_target_gesamt=gesamt_target(Influenza_target)
RSV_target_gesamt=gesamt_target(RSV_target)

#Rest berechnen
Rest_target_gesamt=Covid_target_gesamt
dates=Rest_target_gesamt$date
for (i in dates){
  Rest_target_gesamt[which(Rest_target_gesamt$date==i),which(names(Rest_target_gesamt)=="value")]=
    sari_target_gesamt[which(sari_target_gesamt$date==i),which(names(sari_target_gesamt)=="value")]-
    Influenza_target_gesamt[which(Influenza_target_gesamt$date==i),which(names(Influenza_target_gesamt)=="value")]-
    Covid_target_gesamt[which(Covid_target_gesamt$date==i),which(names(Covid_target_gesamt)=="value")]-
    RSV_target_gesamt[which(RSV_target_gesamt$date==i),which(names(RSV_target_gesamt)=="value")]
}
write.csv(Rest_target_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Rest_target_ohne_alter.csv",row.names = FALSE)
write.csv(sari_target_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Sari_target_ohne_alter.csv",row.names = FALSE)
write.csv(Covid_target_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Covid_target_ohne_alter.csv",row.names = FALSE)
write.csv(Influenza_target_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Influenza_target_ohne_alter.csv",row.names = FALSE)
write.csv(RSV_target_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\RSV_target_ohne_alter.csv",row.names = FALSE)

