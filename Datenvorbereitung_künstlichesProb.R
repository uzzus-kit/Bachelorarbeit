Sari_triangle=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari.csv")
Sari_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv")
triangle_künstlich=function(Datum){
  Dates=unique(Sari_triangle$date)
  Zeitraum=Dates[which(Dates==Datum):length(Dates)]
  Sari_künstlichesProb_triangle=Sari_triangle[which(Sari_triangle$date %in% Zeitraum),]
  write.csv(Sari_künstlichesProb_triangle,paste0("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\triangle_künstliches_Prob_",Datum,".csv"),row.names =FALSE)
}

target_künstlich=function(Datum){
  Dates=unique(Sari_target$date)
  Zeitraum=Dates[which(Dates==Datum):length(Dates)]
  Sari_künstlichesProb_target=Sari_target[which(Sari_target$date %in% Zeitraum),]
  write.csv(Sari_künstlichesProb_target,paste0("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target_künstliches_Prob_",Datum,".csv"),row.names = FALSE)
}
