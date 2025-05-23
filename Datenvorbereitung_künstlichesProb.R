Sari_triangle=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari.csv")
Dates=unique(Sari_triangle$date)
Zeitraum=Dates[which(Dates==as.Date("2023-10-22")):length(Dates)]
Sari_künstlichesProb_triangle=Sari_triangle[which(Sari_triangle$date %in% Zeitraum),]
write.csv(Sari_künstlichesProb_triangle,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\triangle_künstliches_Prob.csv",row.names =FALSE)

Sari_target=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv")
Sari_künstlichesProb_target=Sari_target[which(Sari_target$date %in% Zeitraum),]
write.csv(Sari_künstlichesProb_target,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target_künstliches_Prob.csv",row.names = FALSE)