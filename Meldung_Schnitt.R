library(ggplot2)
library(tidyr)
library(dplyr)
Covid=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_covid19.csv")
Influenza=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_influenza.csv")
RSV=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_rsv.csv")
sari=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari.csv")
rest=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-Rest.csv")
sari=sari[which(sari$age_group=="00+"),c(1:10)]
sari_kurz=sari[which(sari$age_group=="00+"),c(1:10)]
Influenza=Influenza[which(Influenza$age_group=="00+"),c(1:10)]
RSV=RSV[which(RSV$age_group=="00+"),c(1:10)]
Covid=Covid[which(Covid$age_group=="00+"),c(1:10)]
rest=rest[which(rest$age_group=="00+"),c(1:10)]
sari_kurz=sari[sari_kurz$date %in% Covid$date,]
row.names(sari_kurz)=NULL
#colnames(sari)=NULL
#Ich mach einmal total und einmal relativ
sari_kurz[is.na(sari_kurz)] <- 0
Influenza[is.na(Influenza)]=0
Covid[is.na(Covid)]=0
RSV[is.na(RSV)]=0
rest[is.na(rest)]=0
sari[is.na(sari)]=0
aufsummieren=function(Daten){
  Daten_summiert=matrix(0,nrow=1,ncol=5)
  colnames(Daten_summiert)=cbind("Woche0","Woche1","Woche2","Woche3","Woche4")
  for(j in 1:5){
      Daten_summiert[1,j]=sum(Daten[,5+j])
  }
  return(Daten_summiert)
}
Covid_summiert=aufsummieren(Covid)
Influenza_summiert=aufsummieren(Influenza)
RSV_summiert=aufsummieren(RSV)
sari_kurz_summiert=aufsummieren(sari_kurz)
rest_summiert=aufsummieren(rest)
sari_summiert=aufsummieren(sari)
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
sari_kurz_schnitt=schnitt_berechnen(sari_kurz_summiert,length(sari_kurz[,2]))
rest_schnitt=schnitt_berechnen(rest_summiert,length(rest[,2]))
sari_schnitt=schnitt_berechnen(sari_summiert,length(sari[,2]))
#Daten total darstellen
total_plotten=function(Daten_schnitt,name){
  df <- data.frame(
    Verzug =as.character(colnames(Daten_schnitt)[1,]),
    Wert = as.numeric(Daten_schnitt[1,])
  )
  
  # Balkendiagramm
  ggplot(df, aes(x = Verzug, y = Wert)) +
    scale_y_continuous(labels = scales::label_comma(decimal.mark = ",", big.mark = "."))+
    geom_bar(stat = "identity", fill = "#009682") +
    labs(title = paste0("Werte nach Wochen von ",name), x = "Woche", y = "Wert") +
    theme_minimal()
}
total_plotten(Covid_summiert,"Summe Covid")
total_plotten(Influenza_summiert,"Summe Influenza")
total_plotten(RSV_summiert,"Summe RSV")
total_plotten(sari_kurz_summiert,"Summe Sari")
total_plotten(rest_summiert,"Summe  Rest")
total_plotten(Covid_schnitt,"Covid")
total_plotten(Influenza_schnitt,"Influenza")
total_plotten(RSV_schnitt,"RSV")
total_plotten(sari_kurz_schnitt,"sari")
total_plotten(rest_schnitt,"Rest")
#Berechnung Anteil bezogen auf Grunddaten
Anteil=function(Daten_summiert){
  Daten_anteil=Daten_summiert
  Daten_gesamt=sum(Daten_summiert)
  for (i in 1:5){
    Daten_anteil[1,i]=Daten_summiert[1,i]/Daten_gesamt
  }
  return(Daten_anteil)
}
Anteil_Covid=Anteil(Covid_summiert)
Anteil_Influenza=Anteil(Influenza_summiert)
Anteil_RSV=Anteil(RSV_summiert)
Anteil_sari_kurz=Anteil(sari_kurz_summiert)
Anteil_rest=Anteil(rest_summiert)
Anteil_sari=Anteil(sari_summiert)
df <- data.frame(
  Krankheit=c("Covid","Influenza","RSV","Sari(kurz)","Rest","sari(lang)"),
  Woche0 = c(Anteil_Covid[1,1],Anteil_Influenza[1,1],Anteil_RSV[1,1],Anteil_sari_kurz[1,1],Anteil_rest[1,1],Anteil_sari[1,1]),
  Woche1 = c(Anteil_Covid[1,2],Anteil_Influenza[1,2],Anteil_RSV[1,2],Anteil_sari_kurz[1,2],Anteil_rest[1,2],Anteil_sari[1,2]),
  Woche2 = c(Anteil_Covid[1,3],Anteil_Influenza[1,3],Anteil_RSV[1,3],Anteil_sari_kurz[1,3],Anteil_rest[1,3],Anteil_sari[1,3]),
  Woche3 = c(Anteil_Covid[1,4],Anteil_Influenza[1,4],Anteil_RSV[1,4],Anteil_sari_kurz[1,4],Anteil_rest[1,4],Anteil_sari[1,4]),
  Woche4 = c(Anteil_Covid[1,5],Anteil_Influenza[1,5],Anteil_RSV[1,5],Anteil_sari_kurz[1,5],Anteil_rest[1,5],Anteil_sari[1,5])
)

# In long format bringen
df_long <- df %>%
  pivot_longer(cols = starts_with("Woche"),names_to = "Woche",values_to = "Anteil"
  )
df_long <- df_long %>%
  mutate(
    Woche = recode(Woche,
                   "Woche4" = "4 Wochen",
                   "Woche3" = "3 Wochen",
                   "Woche2" = "2 Wochen",
                   "Woche1" = "1 Woche",
                   "Woche0" = "0 Wochen"),
   )

# Faktorlevel für Reihenfolge der Wochen (Woche4 oben im Stack)
df_long$Woche <- factor(df_long$Woche, levels = c("4 Wochen","3 Wochen", "2 Wochen", "1 Woche", "0 Wochen"))

# Plotten
ggplot(df_long, aes(x = Krankheit, y = Anteil, fill = Woche)) + 
  scale_fill_manual(values = c("0 Wochen" = "#009682", "1 Woche" = "black", "2 Wochen"="#4664aa", "3 Wochen"="#A3107C","4 Wochen"="#FCE500"))+
  geom_bar(stat = "identity", width = 0.5) + 
  labs(title = "Gestapelter Balken für eine Krankheit mit Verzugszeiten",
       x = NULL, y = "Anteil", fill = "Meldeverzüge") +
  theme_minimal()
total_plotten(Anteil_Covid,"Anteil Covid")
total_plotten(Anteil_Influenza,"Anteil Influenza")
total_plotten(Anteil_RSV,"Anteil RSV")
total_plotten(Anteil_sari_kurz,"Anteil Sari (kurz)")
total_plotten(Anteil_rest,"Anteil Rest")

