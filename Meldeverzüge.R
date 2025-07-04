library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)
Covid=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_covid19.csv")
Influenza=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_influenza.csv")
RSV=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_rsv.csv")
sari=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari.csv")
#Zuerst Daten vorverarbeiten und dann eine Methode zur Darstellung schreiben
#Covid
Daten_kürzen=function(Daten){                           #Nur die Periode 0-4 wird betrachtet
  Daten=Daten[,1:which(names(Daten) == "value_4w")]
  return(Daten)
}
#Daten auf vorhandene Daten kürzen 
Covid=Daten_kürzen(Covid)
Influenza=Daten_kürzen(Influenza)
RSV=Daten_kürzen(RSV)
sari=Daten_kürzen(sari)
Covid=Covid[c(1:27, 136:189),]
row.names(Covid)=NULL
Influenza=Influenza[1:27,]
RSV=RSV[1:27,]
Covid_gesamt=matrix(0,nrow = (length(Covid[,1])/3),ncol = length(Covid[1,]))
Covid_gesamt=Covid[1:27,]
sari_gesamt=sari[1:119,]
#Stratifizierung der Sari daten raus nehmen
for (i in 1:119){
  for(j in which(names(sari)=="value_0w"):which(names(sari)=="value_4w")){
    sari_gesamt[i, j] <- sum(sari[which(sari$date == sari$date[i]), j], na.rm = TRUE)
  }
}
write.csv(sari_gesamt,"sari_ges.csv",row.names = TRUE)
read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\sari_ges.csv")
#Jetzt noch die Stratifizierung der Covid Daten raus nehmen
for(i in 1:(length(Covid[,1])/3)){
  for(j in which(names(Covid)=="value_0w"):which(names(Covid)=="value_4w")){
    Covid_gesamt[i,j]=Covid[i,j]+Covid[i+27,j]+Covid[i+(2*27),j]
  }
}
i=1
j=1
#NA weg machen Setzt NA künstlich auf 0
NA_Weg=function(Daten){
  for (i in 1:length(Daten[,1])) {
    for (j in 1:length(Daten[1,])){
      if (is.na(Daten[i,j])){
        Daten[i,j]=0
      }
    }
  }
  return(Daten)
}
Covid_gesamt=NA_Weg(Covid_gesamt)
Influenza=NA_Weg(Influenza)
RSV=NA_Weg(RSV)
sari_gesamt=NA_Weg(sari_gesamt)
Covid_gesamt=Covid_gesamt[,c(1,3:10)]
Influenza=Influenza[,c(1,3:10)]
RSV=RSV[,c(1,3:10)]
sari_gesamt=sari_gesamt[,c(1,3:10)]
write.csv(Covid_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Covid_reg.csv",row.names = FALSE)
write.csv(Influenza,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Influenza_reg.csv",row.names = FALSE)
write.csv(RSV,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\RSV_reg.csv",row.names=FALSE)
write.csv(sari_gesamt[93:119,],"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\sari_reg.csv",row.names=FALSE)
write.csv(sari_gesamt,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\sari_gesamt.csv",row.names = FALSE)
sari_gekürzt=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\sari_reg.csv")
#Gesamt Verzüge berechnen
Gesamtverzüge=Covid_gesamt
Woche0=which(names(Gesamtverzüge)=="value_0w")
Woche4=which(names(Gesamtverzüge)=="value_4w")
for(i in 1:length(Gesamtverzüge[,1])){
  for(j in Woche0:Woche4){
    Gesamtverzüge[i,j]=Covid_gesamt[i,j]+RSV[i,j]+Influenza[i,j]
  }
}
write.csv(Gesamtverzüge,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\gesamtverzüge_reg.csv",row.names = FALSE)
Rest=Gesamtverzüge
  #Rest werte berechnen
for (i in 1:27){
  for(j in Woche0:Woche4){
    Rest[i,j]=sari_gekürzt[i,j]-Gesamtverzüge[i,j]
  }
}
write.csv(Rest,"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Rest_reg.csv",row.names = FALSE)
#Daten zum Plotten vorbereiten
Summe_ber=function(Daten,länge){
  summe=numeric(länge)
  summe=rowSums(Daten[,which(names(Daten)=="value_0w"):which(names(Daten)=="value_4w")])  
  return(summe)
}
summe_Covid=Summe_ber(Covid_gesamt,length(Covid_gesamt[,1]))
summe_Influenza=Summe_ber(Influenza,length(Influenza[,1]))
summe_RSV=Summe_ber(RSV,length(RSV[,1]))
summe_Gesamtverzüge=Summe_ber(Gesamtverzüge,length(Gesamtverzüge[,1]))
summe_sari=Summe_ber(sari_gesamt, length(sari_gesamt[,1]))
summe_rest=Summe_ber(Rest, length(Rest[,1]))
Anteil=function(Daten,Summe){
  Woche0=which(names(Daten)=="value_0w")
  Woche4=which(names(Daten)=="value_4w")
  for(i in 1:length(Daten[,1])){
    for(j in Woche0:Woche4){
      if(Summe[i]!=0)
        Daten[i,j]=Daten[i,j]/Summe[i]
      else
        Daten[i,j]=0
    }
  }
  return(Daten)
}
Anteil_Covid=Anteil(Covid_gesamt,summe_Covid)
Anteil_Influenza=Anteil(Influenza,summe_Influenza)
Anteil_RSV=Anteil(RSV,summe_RSV)
Anteil_Gesamtverzüge=Anteil(Gesamtverzüge,summe_Gesamtverzüge)
Anteil_sari=Anteil(sari_gesamt,summe_sari)
Anteil_sari_gekürzt=matrix(0,nrow = 27,ncol=length(Influenza[1,]))
Anteil_sari_gekürzt <- Anteil_sari[Anteil_sari$date %in% Influenza$date, ]
Anteil_Rest=Anteil(Rest,summe_rest)
row.names(Anteil_sari_gekürzt)=NULL
#Daten vorverarbeitung abgeschlossen
#Plotten
plotten = function(Anteil_x, Name) {
  df = data.frame(
    Woche = Anteil_x$date,
    Wert_Woche0 = Anteil_x$value_0w,
    Wert_Woche1 = Anteil_x$value_1w,
    Wert_Woche2 = Anteil_x$value_2w,
    Wert_Woche3 = Anteil_x$value_3w,
    Wert_Woche4 = Anteil_x$value_4w
  )
  
  wochen_labels <- Anteil_x$date[seq(1, length(Anteil_x$date), by = 2)]
  
  df_long = df %>%
    pivot_longer(cols = -Woche, names_to = "Kategorie", values_to = "Wert") %>%
    mutate(Kategorie = recode(Kategorie,
                              "Wert_Woche0" = "0 Wochen",
                              "Wert_Woche1" = "1 Woche",
                              "Wert_Woche2" = "2 Wochen",
                              "Wert_Woche3" = "3 Wochen",
                              "Wert_Woche4" = "4 Wochen"),
           Kategorie = factor(Kategorie, levels = c("4 Wochen", "3 Wochen", "2 Wochen", "1 Woche", "0 Wochen")))
  
  df_long$Woche <- factor(df_long$Woche, levels = unique(df$Woche))
  
  ggplot(df_long, aes(x = Woche, y = Wert, fill = Kategorie)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(
      "0 Wochen" = "#009682",
      "1 Woche" = "black",
      "2 Wochen" = "#4664aa",
      "3 Wochen" = "#A3107C",
      "4 Wochen" = "#FCE500")) +
    scale_x_discrete(breaks = wochen_labels) +
    labs(title = paste("Meldeverzüge für", Name),
         x = "Kalenderwoche",
         y = "Anteil",
         fill = "Meldeverzüge") +
    theme_minimal() +
    scale_y_continuous(limits = c(-0.3, 1.3))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

}
Anteil_Covid$date
p1=plotten(Anteil_Covid, "COVID-19")
p2=plotten(Anteil_Influenza, "Influenza")
p3=plotten(Anteil_RSV,"RSV")
p4=plotten(Anteil_Rest,"Rest")
plotten(Anteil_Gesamtverzüge,"Gesamtverzüge")
p5=plotten(Anteil_sari_gekürzt,"SARI")
(p1 | p2) / (p3 | p4)/(p5) + plot_layout(guides = "collect")
layout <- c(
  area(1, 1),  # p1: oben links
  area(1, 2),  # p2: oben rechts
  area(2, 1),  # p3: mitte links
  area(2, 2),  # p4: mitte rechts
  area(3, 1),  # p5: unten links
  area(3, 2)   # leer oder anderer Plot möglich
)

# Zusammenfügen mit dem definierten Layout
(p1 + p2 + p3 + p4 + p5) + 
  plot_layout(design = layout, guides = "collect")
plotten = function(Anteil_x, Name) {
  df = data.frame(
    Woche = Anteil_x$date,
    Wert_Woche0 = Anteil_x$value_0w,
    Wert_Woche1 = Anteil_x$value_1w,
    Wert_Woche2 = Anteil_x$value_2w,
    Wert_Woche3 = Anteil_x$value_3w,
    Wert_Woche4 = Anteil_x$value_4w
  )
  
  wochen_labels <- Anteil_x$date[seq(1, length(Anteil_x$date), by = 5)]
  
  df_long = df %>%
    pivot_longer(cols = -Woche, names_to = "Kategorie", values_to = "Wert") %>%
    mutate(Kategorie = recode(Kategorie,
                              "Wert_Woche0" = "0 Wochen",
                              "Wert_Woche1" = "1 Woche",
                              "Wert_Woche2" = "2 Wochen",
                              "Wert_Woche3" = "3 Wochen",
                              "Wert_Woche4" = "4 Wochen"),
           Kategorie = factor(Kategorie, levels = c("4 Wochen", "3 Wochen", "2 Wochen", "1 Woche", "0 Wochen")))
  
  df_long$Woche <- factor(df_long$Woche, levels = unique(df$Woche))
  
  ggplot(df_long, aes(x = Woche, y = Wert, fill = Kategorie)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(
      "0 Wochen" = "#009682",
      "1 Woche" = "black",
      "2 Wochen" = "#4664aa",
      "3 Wochen" = "#A3107C",
      "4 Wochen" = "#FCE500")) +
    scale_x_discrete(breaks = wochen_labels) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
    labs(title = paste("Meldeverzüge für", Name, "anteilig nach Kalenderwoche"),
         x = "Kalenderwoche",
         y = "Anteil",
         fill = "Meldeverzüge") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
}
plotten(Anteil_sari,"sari")                           #Braucht neue Funktion da Skalierung auf x und y Achse angepasst werden muss da deutlich mehr Wochen an Daten gegeben

