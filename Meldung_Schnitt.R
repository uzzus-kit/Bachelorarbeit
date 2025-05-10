library(ggplot2)
Covid=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Covid_reg.csv")
Influenza=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Influenza_reg.csv")
RSV=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\RSV_reg.csv")
sari=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\sari_gesamt.csv")
rest=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Rest_reg.csv")
#Ich mach einmal total und einmal relativ
aufsummieren=function(Daten){
  Daten_summiert=matrix(0,nrow=1,ncol=5)
  colnames(Daten_summiert)=cbind("Woche0","Woche1","Woche2","Woche3","Woche4")
  for(j in 1:5){
    for(i in 1:length(Daten[,2])){
      Daten_summiert[1,j]=Daten_summiert[1,j]+Daten[i,4+j]
    }
  }
  return(Daten_summiert)
}
Covid_summiert=aufsummieren(Covid)
Influenza_summiert=aufsummieren(Influenza)
RSV_summiert=aufsummieren(RSV)
sari_summiert=aufsummieren(sari)
rest_summiert=aufsummieren(rest)

schnitt_berechnen=function(Daten_summiert,länge){
  Daten_schnitt=matrix(0,nrow=1,ncol=5)
  colnames(Daten_schnitt)=cbind("Woche0","Woche1","Woche2","Woche3","Woche4")
  for (i in 1:5){
    Daten_schnitt[1,i]=Daten_summiert[1,i]/länge
  }
  return(Daten_schnitt)
}
Covid_schnitt=schnitt_berechnen(Covid_summiert,length(Covid[,2]))
Influenza_schnitt=schnitt_berechnen(Influenza_summiert,length(Influenza[,2]))
RSV_schnitt=schnitt_berechnen(RSV_summiert,length(RSV[,2]))
sari_schnitt=schnitt_berechnen(sari_summiert,length(sari[,2]))
rest_schnitt=schnitt_berechnen(rest_summiert,length(rest[,2]))

#Daten total darstellen
total_plotten=function(Daten_schnitt,name){
  df <- data.frame(
    Verzug =as.character(colnames(Daten_schnitt)[1,]),
    Wert = as.numeric(Daten_schnitt[1,])
  )
  
  # Balkendiagramm
  ggplot(df, aes(x = Verzug, y = Wert)) +
    geom_bar(stat = "identity", fill = "#009682") +
    labs(title = paste0("Werte nach Wochen von ",name), x = "Woche", y = "Wert") +
    theme_minimal()
}
total_plotten(Covid_schnitt,"Covid")
total_plotten(Influenza_schnitt,"Influenza")
total_plotten(RSV_schnitt,"RSV")
total_plotten(sari_schnitt,"sari")
total_plotten(rest_schnitt,"Rest")
