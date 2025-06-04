library(dplyr)
Covid=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_covid19.csv")
Influenza=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_influenza.csv")
RSV=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_rsv.csv")
sari=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari.csv")
Covid_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_covid19.csv")
Influenza_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_influenza.csv")
RSV_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_rsv.csv")
sari_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv")

sari_gekürzt=sari[which(sari$age_group=="00+" & sari$date %in% RSV$date),]
row.names(sari_gekürzt)=NULL
Rest=sari_gekürzt
for (i in 1:length(Rest[,1])){
  for (j in which(colnames(Rest)=="value_0w"):which(colnames(Rest)=="value_.10w")){
    Rest[i,j]=sari_gekürzt[i,j]-Covid[i,j]-Influenza[i,j]-RSV[i,j]
  }
}
write.csv(Rest,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-Rest.csv",row.names = FALSE)

sari_gekürzt_target=sari_target[which(sari_target$age_group=="00+" & sari_target$date %in% RSV_target$date),]
row.names(sari_gekürzt_target)=NULL
Rest_target=sari_gekürzt_target
for(i in 1:length(Rest_target[,1])){
  Rest_target[i,which(colnames(Rest_target)=="value")]=
    sari_gekürzt_target[i,which(colnames(sari_gekürzt_target)=="value")]-
    RSV_target[i,which(colnames(RSV_target)=="value")]-
    Influenza_target[i,which(colnames(Influenza_target)=="value")]-
    Covid_target[i,which(colnames(Covid_target)=="value")]
}
write.csv(Rest_target,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-rest.csv",row.names = FALSE)

#Überlegung: Durschnittswert über alle als Observed 2 nehmen
Durchschnitt=sari_gekürzt
for (i in 1:length(Durchschnitt[,1])){
  for (j in which(colnames(Rest)=="value_0w"):which(colnames(Rest)=="value_.10w")){
    Durchschnitt[i,j]=sari_gekürzt[i,j]+Covid[i,j]+Influenza[i,j]+RSV[i,j]/4
  }
}
Durchschnitt=rbind(sari[1:92,],Durchschnitt)
write.csv(Durchschnitt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-Durchschnitt.csv",row.names = FALSE)
Durchschnitt_target=sari_gekürzt_target
value=which(colnames(Durchschnitt_target)=="value")
for(i in 1:length(Durchschnitt_target[,1])){
  Durchschnitt_target[i,value]=sari_gekürzt_target[i,value]+RSV_target[i,value]+Influenza_target[i,value]+Covid_target[i,value]/4
}
Durchschnitt_target=rbind(sari_target[1:92,],Durchschnitt_target)
write.csv(Durchschnitt_target,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-Durchschnitt.csv",row.names = FALSE)

